(ns reduce-fsm
  "Generate and display functional finite state machines that accumulate state
in the same way as reduce.
This package allows you to:
 - Create basic fsm's (see fsm)
 - Create lazy sequences from state machines (see fsm-seq)
 - Create stateful filter functions for use with filter/remove (see fsm-filter)
 - Visualise state machines as"
  (:use [clojure.core [match :only [match]]])
  (:require
   [clojure [set :as set]]
   clojure.core.match.regex
   [dorothy [core :as d]]
   [clojure [string :as str]]))

(defn- fsm-fn?
  "return true if the symbol will be treated as a function in fsm state definitions."
  [sym]
  (not (keyword? sym)))

(defn- report-compile-error
  "Report fatal errors during fsm compilation"
  [& args]
  (let [msg (apply format args)]
    (println (str "error: " msg))
    (throw (Exception. (str "FSM compilation exception: " msg)))))

(defn- report-compile-warning
  "Report warnings during fsm compilation"
  [& args]
  (let [msg (apply format args)]
    (println (str "warning: " msg))))

(defn- sanity-check-fsm
  "Be nice to our users and check for problems in the fsm definition at compile time"
  [state-maps]
  (let [state-names (set (map :from-state state-maps))
	transitions (mapcat (fn [s] (map #(assoc % :from-state (:from-state s)) (:transitions s))) state-maps)]

    ;; all targets of a transition must exist
    (doseq [{:keys [from-state to-state]} transitions]
      (when-not (state-names to-state)
	(report-compile-error "The state %s was referenced in a transition from %s but does not exist" to-state from-state)))
    
    ;; all states except for the first should be reachable by a transition
    (let [state-has-incoming-trans (set (map :to-state transitions))]
      (doseq [state (rest (map :from-state state-maps))]
	(when-not (state-has-incoming-trans state)
	  (report-compile-warning "The state %s is not the initial state and is unreachable by any transitions" state))))
    
    ;; check for unexpected keys in transition properties
    (let [user-keys #{:emit :action}
	  expected-keys (into #{:from-state :to-state :evt} user-keys)]
      (doseq [t transitions]
	(let [xtra-keys (set/difference (-> t keys set) expected-keys)]
	  (when (seq xtra-keys)
	    (report-compile-warning "The key(s) %s was/were used in a transition from state %s, we only one expected or more of %s"
				    xtra-keys (:from-state t) user-keys)))))
    
    ;; check for unexpected keys in state params
    (let [expected-keys #{:pass :is-terminal}]
      (doseq [s state-maps]
	(let [xtra-keys (set/difference (-> s :state-params keys set) expected-keys)]
	  (when (seq xtra-keys)
	    (report-compile-warning "The key(s) %s was used in the state parameters for %s, we only expected one or more of %s"
				    xtra-keys (:from-state s) expected-keys)))))))

(defn- create-transition
  "create a single transition make with default params if none specified.
  the expected input is like: [[#\".*event c\"] [{:emit emit-event :action inc-matches}? :waiting-for-a\"]]"
  [[from to]]
  (let [has-params? (map? (first to))
	params (if has-params? (first to) {})
	to-state (if has-params? (second to) (first to))]
    (assoc params
      :evt (last from)
      :to-state to-state)))

(defn- create-state-map
  "Create an entry for a single '[:state evt1 -> :state1 :evt2 -> :state2 ...]"
  [forms]
  (let [from-state (first forms)
	transitions (partition 2 1
			       (remove #(= '-> (first %))
				       (partition-by #(= '-> %) (rest forms))))]
    {:from-state from-state
     :state-params (when (-> forms second map?) (second forms))
     :transitions (vec (map create-transition transitions))}))

(defn- create-state-maps
  "Create the sequence of maps that defines the fsm from vector representation.
  returns a list of maps similar to {:from-state :a-state :state-params {...} :transitions [{:to-state :another-state :evt 1}]}"
  [states]
  (let [state-maps  (map create-state-map states)]
    (sanity-check-fsm state-maps)
    state-maps))
  

(defn- state-fn-name
  "Create a name for the internal function that will represent this state.
   We want it to be recognisable to the user so stack traces are more intelligible"
  [sym]
  (cond
   (fn? sym) (let [fn-name (-> sym meta :name str)]
               (if (empty? fn-name)
                 (str (gensym "fn-"))
                 fn-name))
   (keyword? sym) (name sym)
   :else (str sym)))

(defn- state-fn-symbol
  "Create a name for the internal function that will represent this state.
   We want it to be recognisable to the user so stack traces are more intelligible"
  [sym]
  (gensym (str "state" "-" (state-fn-name sym) "-")))

(defn- state-for-action [state]
  (cond
   (fn? state) (-> state meta :name keyword)
   (keyword? state) state
   :else (keyword state)))


(defn- expand-evt-dispatch
  "Expand the dispatch of a single event, this corresponds to a single case in the match expression.
Parameters:
  state-fn-map - a map of state-symbol -> name of implementing function
  state-params - maps of state-symbol -> {:param1 ..}
  from-state   - the state we're transitioning from
  evt          - the name of the event parameter in the match statement
  acc          - the name of the accumulator parameter in the match statement
  events       - the sequence of events
  evt-map      - the map representing this transition, eg. {:to-state x :action .... }"
  [state-fn-map state-params from-state evt acc events evt-map]
  (let [target-state-fn (state-fn-map (:to-state evt-map))
	new-acc (gensym "new-acc")]
    `[~(:evt evt-map)
      (let [~new-acc ~(if (:action evt-map)
			`(~(:action evt-map) ~acc ~evt ~(state-for-action from-state) ~(state-for-action (:to-state evt-map)))
			acc)]
	~(cond
	  (-> evt-map :to-state state-params :is-terminal) `~new-acc ;; terminal states return the accumulated val
	  (fsm-fn? (:to-state evt-map))           ;; if the target state is a function we need to check for early conditional termination
	  `(if (~(:to-state evt-map) ~new-acc)    ;; truthy return from a state function causes the fsm to exit
	     ~new-acc
	     (partial ~target-state-fn ~new-acc (rest ~events)))
	  :else `(partial ~target-state-fn ~new-acc (rest ~events))))])) ;; normal (keyword/non-terminal) states

(defn- expand-dispatch [dispatch-type evt acc]
  (case dispatch-type
	:event-only [`match evt]
	:event-and-acc   [`match [acc evt]]
        :event-acc-vec [`match [[acc evt]]]
	(throw (RuntimeException. "unknown fsm dispatch type, expected one of [:event-only :event-and-acc :event-acc-vec]"))))
  
(defn- state-fn-impl
  "define the function used to represent a single state internally"
  [dispatch-type state-fn-map state-params state]
  (let [this-state-fn  (state-fn-map (:from-state state))
	events (gensym "events")
	acc (gensym "acc")
	evt (gensym "evt")]
    `(~this-state-fn
      [~acc ~events]
      (if-let [~evt (first ~events)]
	#(~@(expand-dispatch dispatch-type evt acc)
		~@(mapcat (partial expand-evt-dispatch state-fn-map state-params (:from-state state)  evt acc events) (:transitions state))
		:else (partial ~this-state-fn ~acc (rest ~events))
		)
	~acc))))
  

(defn- transitions-metadata
  "create the metadata representation of all transitions for a single state"
  [state]
   (let [from-state (keyword (:from-state state))]
     (map (fn [t]
 	   (let [trans (dissoc t :from-state :to-state)] ;; convert all non-state params into strings
 	     (assoc (zipmap (keys trans) (map (fn [x] `'~x) (vals trans)))
 	       :from-state from-state
 	       :to-state (keyword (:to-state t)))))
	  (:transitions state))))

(defn- state-metadata
  "create the metadata representation for a single state"
  [state]
  {:state  (keyword (:from-state state))
   :name (if (fsm-fn? (:from-state state))
		  (str "(" (:from-state state) ")")
		  (str (:from-state state)))
   :params (:state-params state)
   :transitions  (vec (transitions-metadata state))})

(defn- fsm-metadata
  "Create the metadata for the fsm. We'll use this at runtime to draw diagrams of the fsm"
  [fsm-type state-maps]
  {::fsm-type fsm-type
   ::states (vec (map state-metadata state-maps))})

(defn lookup-state [state-fn-map the-state]
  (if-let [a-state-fn (get state-fn-map the-state)]
    a-state-fn
    (throw (RuntimeException. (str "Could not find the state \"" the-state)) "\"")))

;;===================================================================================================
;; We want to turn an fsm definition looking like this:
;;
;; (fsm [[:waiting-for-a
;;        #".*event a" -> :waiting-for-b]
;;       [:waiting-for-b
;;        #".*event d" -> :waiting-for-a
;;        #".*event c" -> {:action (fn [acc evt & _] (conj acc evt))} :waiting-for-a]])
;;
;; into the following trampoline based implementation:
;;
;; (fn the-fsm
;;   ([events]
;;    (the-fsm nil events))
;;   ([acc events]
;;      (letfn [(state-waiting-for-a  [acc events]
;; 	      (if-let [evt (first events)]
;; 		#(match evt
;; 			  #".*event a" (partial state-waiting-for-b evt (rest events))
;; 			  :else        (partial state-waiting-for-a evt (rest events)))
;; 		acc))
;; 	     (state-waiting-for-b  [acc events]
;; 	      (if-let [evt (first events)]
;; 		#(match evt
;; 			  #".*event d" (partial state-waiting-for-a acc (rest events))
;; 			  #".*event c" (let [new-acc ((fn [acc evt & _] (conj acc evt)) acc evt :waiting-for-b :waiting-for-a)]
;; 					 (partial state-waiting-for-a new-acc (rest events)))
;; 			  :else (partial state-waiting-for-b acc (rest events)))
;; 		acc))]
;;        (trampoline state-waiting-for-a acc events))))
(defmacro fsm
  "Returns an fsm function that reads a sequence of events and returns
an accumulated value (like reduce). The returned function will have the following 2 arities:
 [events]                   - accepts a sequence of events
 [val events]               - accepts an initial value for the accumulator and a sequence of events.
 [initial-state val events] - start the fsm in the specified state with an accumulator value and a sequence of events.

The generated function will return when one of the following is true:
 - There are no more events in the event sequence
 - The fsm reaches a terminal state
 - The fsm reaches a state defined by a function and it returns a truthy value

Parameters:
 fsm      - the fsm definition (see below for syntax)
 fsm-opts - the following options are recognised:
  :default-acc val - sets the initial value for the accumulator in the single arity version of the function
  :dispatch - changes the way events are matched, the follow options are accepted:
    - :event-only (default) - events are matched using the  core.match/match syntax against the event only
    - :event-and-acc        - events use the default match syntax and are matched against [acc-value event]
    - :event-acc-vec        - events are matches against a single vector of [[acc-value event]]

FSM definitions:
 fsm's are defined as follows:
 [[state {:is-terminal true/false}?
   event -> {:action a-fn}? target-state
   event2 -> ...]
  [target-state ...]]

Where
 state  - is a keyword or function
 state  - options (:is-terminal) are optional
 event  - is any legal core.match pattern (see https://github.com/clojure/core.match)
 action - is optional but must be a function if specified and the return value
          will be used as the new accumulated state.

State and Event Functions:
 State functions are called like so (state-fn acc) where acc is the current accumulated state.
 Event functions are called with (event-fn acc event from-state to-state) where
   acc        - is the current accumulated state
   event      - is the event that fired the transition
   from-state - the state we're transitioning from
   to-state   - the state we're transitioning to

See https://github.com/cdorrat/reduce-fsm for examples and documentation"

  [states & fsm-opts]
  (let [{:keys [dispatch default-acc] :or {dispatch :event-only}} fsm-opts
	state-maps  (create-state-maps states)
	state-params (zipmap (map :from-state state-maps) (map :state-params state-maps))
	state-fn-names (map state-fn-symbol (map :from-state state-maps))
	state-fn-map (zipmap (map :from-state state-maps) state-fn-names)] ;; map of state -> letfn function name
    `(letfn [~@(map #(state-fn-impl dispatch state-fn-map state-params %) state-maps)]	    
      (with-meta
        (fn the-fsm#
          ([events#] (the-fsm# ~default-acc events#))
          ([acc# events#]
             (the-fsm# ~(-> state-maps first :from-state) acc# events#))
          ([initial-state# acc# events#]
             (trampoline (lookup-state ~state-fn-map initial-state#) acc# events#)))
        ~(fsm-metadata :fsm state-maps)))))

(defmacro defsm
  "A convenience macro to define a fsm, equivalent to (def fsm-name (fsm states opts)
   see reduce-fsm/fsm for details"
  [fsm-name states & opts]
  `(def ~fsm-name (fsm ~states ~@opts)))

;;===================================================================================================
;; support for incremental fsms
(defn- state-disp-name [sym]
  (keyword (state-fn-name sym)))

(defn- expand-inc-evt-dispatch
  "Expand the dispatch of a single event for incremental fsms, this corresponds to a single case in the match expression.
Parameters:
  state-fn-map - a map of state-symbol -> name of implementing function
  state-params - maps of state-symbol -> {:param1 ..}
  from-state   - the state we're transitioning from
  evt          - the name of the event parameter in the match statement
  acc          - the name of the accumulator parameter in the match statement
  events       - the sequence of events
  evt-map      - the map representing this transition, eg. {:to-state x :action .... }"
  [state-fn-map state-params from-state evt acc events evt-map]
  (let [new-acc (gensym "new-acc")]
    `[~(:evt evt-map)
      (let [~new-acc ~(if (:action evt-map)
                        `(~(:action evt-map) ~acc ~evt ~(state-for-action from-state) ~(state-for-action (:to-state evt-map)))
                        acc)]
        {:state ~(state-disp-name (:to-state evt-map))
         :value ~new-acc
         :fsm ~(state-fn-map (:to-state evt-map))
         :is-terminated? ~(if (fsm-fn? (:to-state evt-map)) ;; if the target state is a fn exit on truthy value
                               `(~(:to-state evt-map) ~new-acc)
                               (if (-> evt-map :to-state state-params :is-terminal)
                                 true
                                 false))})]))

(defn- inc-state-fn-impl
  "define the function used to represent a single state internally for incremental fsms"
  [dispatch-type state-fn-map state-params state]
  (let [this-state-fn  (state-fn-map (:from-state state))
	events (gensym "events")
	acc (gensym "acc")
	evt (gensym "evt")]
    `(~this-state-fn  [~acc ~evt]
	(~@(expand-dispatch dispatch-type evt acc)
         ~@(mapcat (partial expand-inc-evt-dispatch state-fn-map state-params (:from-state state)  evt acc events) (:transitions state))
         :else  {:state ~(state-disp-name (:from-state state))
                 :is-terminated? false
                 :value ~acc
                 :fsm ~ this-state-fn}))))
  
(defmacro fsm-inc
  "Define an incremental finite state machine.
State definitions and capabilities are the same as reduce-fsm/fsm but events
 are provided by calls to (fsm-event inc-fsm event) instead of a sequence.
Returns a function that takes the intial accumulator value (or none for the default nil) and returns an incremental fsm.
Subsequent chained calls to  fsm-event will move the fsm thought it's states.
 (reduce fsm-event ((inc-fsm [... fsm def ..])) events)
 is equivalent to
 (fsm [... fsm def ..] events)"
  [states & fsm-opts]
  (let [{:keys [dispatch default-acc] :or {dispatch :event-only}} fsm-opts
	state-maps  (create-state-maps states)
	state-params (zipmap (map :from-state state-maps) (map :state-params state-maps))
	state-fn-names (map state-fn-symbol (map :from-state state-maps))
	state-fn-map (zipmap (map :from-state state-maps) state-fn-names)] ;; map of state -> letfn function name
    `(letfn [~@(map #(inc-state-fn-impl dispatch state-fn-map state-params %) state-maps)]
       (with-meta
         (fn the-fsm#
           ([] (the-fsm# ~default-acc))
           ([acc#] (the-fsm# ~(-> state-maps first :from-state) acc#))
           ([initial-state# acc#]
              {:state (~state-disp-name initial-state#)
               :is-terminated? false
               :value acc#
               :fsm (lookup-state ~state-fn-map initial-state#)}))
         ~(fsm-metadata :inc-fsm state-maps)))))


(defn fsm-event
  "process a single event with an incremental finite state machine (those created with fsm-inc or defsm-inc)
Returns a map with the following keys:
  :state          - the current state of the fsm after processing the event
  :value          - the current accumulator value
  :is-terminated? - true when the fsm is in a terminal state and no more events can be processed"
  [fsm event]
  {:pre [(map? fsm) (contains? fsm :fsm)]} ;; only valid for incremental fsms
  (if (:is-terminated? fsm)
    fsm
    ((:fsm fsm) (:value fsm) event)))

(defmacro defsm-inc
"A convenience macro to define an incremental fsm, equivalent to (def fsm-name (fsm-inc states opts)
   see reduce-fsm/fsm-inc for details"
[fsm-name states & opts]
`(def ~fsm-name (fsm-inc ~states ~@opts)))

;;===================================================================================================
;; fsm-filter impl

(defn- expand-filter-evt-dispatch
  "Expand the dispatch line for a single fsm-filter dispatch line.
   The return value corresponds to a single case in a match clause"
  [state-fn-map state-params from-state evt acc evt-map]
  (let [target-state-fn (state-fn-map (:to-state evt-map))
	target-pass-val (-> evt-map :to-state state-params (get :pass true))
	new-acc (gensym "new-acc")]
    `[~(:evt evt-map)
      (let [~new-acc ~(if (:action evt-map)
			`(~(:action evt-map) ~acc ~evt ~(state-for-action from-state) ~(state-for-action (:to-state evt-map)))
			acc)]
	[~target-pass-val (~target-state-fn ~new-acc)])]))


(defn- state-filter-fn-impl
  "Expand the definition of a function to handle a single filter state"
  [dispatch-type state-fn-map state-params state]
  (let [this-state-fn  (state-fn-map (:from-state state))
	acc (gensym "acc")
	evt (gensym "evt")]
    `(~this-state-fn
      [~acc]
      (fn [~evt]
	(~@(expand-dispatch dispatch-type evt acc)
	 ~@(mapcat (partial expand-filter-evt-dispatch state-fn-map state-params (:from-state state) evt acc) (:transitions state))
	 :else [~(get (:state-params state) :pass true) (~this-state-fn ~acc)])))))

;;===================================================================================================
;; We want to turn the following filter definition:
;; (fsm-filter [[:initial
;; 	      3 -> :suppressing]
;; 	     [:suppressing {:pass false}
;; 	      6 -> :initial]])
;;
;; into this implementation:
;;
;; (letfn [(state-initial [acc]
;; 	 (fn [evt]
;; 	   (match evt
;; 		    3 [false (state-suppressing acc)]
;; 		    :else [true (state-initial acc)])))
;; 	(state-suppressing [acc]
;; 	 (fn  [evt]
;; 	   (match evt
;; 		    6 [true (state-initial acc)]
;; 		    :else  [false (state-suppressing acc)])))]
;;   (fn filter-builder
;;    ([] (filter-builder nil))
;;    ([acc]
;;     (let [curr-state (atom (state-initial acc))]
;;      (fn [evt]
;;       (let [[pass next-state] (@curr-state evt)]
;;        (reset! curr-state next-state)
;;        pass))))))
;;
(defmacro fsm-filter
"Returns a function that returns fsm filters suitable for use with clojure.core/filter and remove.
Each state in the fsm definition has a :pass attribute that will be returned from the generated function
when it is in that state.

The returned function will have the following 2 arities:
 []    - creates a filter with the default accumulator state (nil or the value from :default-acc)
 [val] - accepts an initial value for the accumulator
 [initial-state val] - start the fsm in the given state with the specified accumulator value

Parameters:
fsm      - the fsm definition (see below for syntax)
fsm-opts - the following options are recognised:
  :default-acc val - sets the initial value for the accumulator in the single arity version function
  :dispatch - changes the way events are matched, the follow options are accepted:
    - :event-only (default) - events are matched using the  core.match/match syntax against the event only
    - :event-and-acc        - events use the default match syntax and are matched against [acc-value event]

FSM definitions:
filters are defined as follows:
 [[state {:pass true/false}?
   event -> {:action a-fn}? target-state]
  [target-state ...]]

Where
  state is a keyword
  state option (:pass) is optional, it defaults to true
  event is any legal core.match pattern (see https://github.com/clojure/core.match)
  action is optional but must be a function if specified and their return value
         will be used as the new accumulated state

Event functions are called with (event-fn acc event from-state to-state) where
  acc        - is the current accumulated state
  event      - is the event that fired the transition
  from-state - the state we're transitioning from
  to-state   - the state we're transitioning to

Example:
  Suppress numbers after seeing a 3 until we see a 6.

  (def f (fsm-filter [[:initial
 	               3 -> :suppressing]
 	              [:suppressing {:pass false}
 	               6 -> :initial]]))

  (= [1 2 6 1 2] (filter (f) [1 2 3 4 5 1 2 6 1 2]))"
  [states & fsm-opts]
  (let [{:keys [dispatch default-acc] :or {dispatch :event-only}} fsm-opts
	state-maps  (create-state-maps states)
	state-fn-names (map state-fn-symbol (map :from-state state-maps))
	state-params (zipmap (map :from-state state-maps) (map :state-params state-maps))
	state-fn-map (zipmap (map :from-state state-maps) state-fn-names)] ;; map of state -> letfn function name
    `(letfn [~@(map #(state-filter-fn-impl dispatch state-fn-map state-params %) state-maps)]
       (with-meta
	 (fn filter-builder#
	   ([] (filter-builder# ~default-acc))
           ([acc#]
              (filter-builder# ~(-> state-maps first :from-state) acc#))
	   ([initial-state# acc#]
	      (let [curr-state# (atom ((lookup-state ~state-fn-map initial-state#) acc#))]
		(fn [evt#]
		  (let [[pass# next-state#] (@curr-state# evt#)]
		    (reset! curr-state# next-state#)
		    pass#)))))
	 ~(fsm-metadata :fsm-filter state-maps)))))
          
(defmacro defsm-filter
  "A convenience macro to define an fsm filter, equivalent to (def fsm-name (fsm-filter states opts)
   see reduce-fsm/fsm-filter for details"
    [name states & fsm-opts]
  `(def ~name (fsm-filter ~states ~@fsm-opts)))
  


;;===================================================================================================
;; fsm-seq impl

(defn- next-emitted
  "Process events with the fsm-seq function f until we emit a new value for the sequence"
  [f]
  (when f
    (loop [[emitted next-step] (f)]
      (if next-step
	(if (not= ::no-event emitted)
	  [emitted next-step]
	  (recur (next-step)))
	[emitted nil]))))

(defn ^{:skip-wiki true} fsm-seq-impl*
  "Create a lazy sequence from a fsm-seq state function"
  [f]
  (let [[emitted next-step] (next-emitted f)]
    (lazy-seq
     (if next-step
       (cons emitted (fsm-seq-impl* next-step))
       (when (not= ::no-event emitted)
	 (cons emitted nil))))))


(defn- expand-seq-evt-dispatch
  "Expand the dispatch line for a single fsm-seq dispatch line.
   The return value corresponds to a single case in a match clause"
  [state-fn-map state-params from-state evt acc evt-map]
  (let [target-state-fn (state-fn-map (:to-state evt-map))
	target-pass-val (-> evt-map :to-state state-params :pass)
	new-acc (gensym "new-acc")]

    `[~(:evt evt-map)
      (let [emitted# ~(if (:emit evt-map)
			`(~(:emit evt-map) ~acc (first ~evt))
			`::no-event)
	    ~new-acc ~(if (:action evt-map)
			`(~(:action evt-map) ~acc (first ~evt) ~(state-for-action from-state) ~(state-for-action (:to-state evt-map)))
			acc)]
	[emitted# (~target-state-fn ~new-acc (rest ~evt))])]))

(defn- state-seq-fn-impl
  "Expand the definition of a function to handle a single state"
  [dispatch-type state-fn-map state-params state]
  (let [this-state-fn  (state-fn-map (:from-state state))
	acc (gensym "acc")
	evt (gensym "evt")]
    `(~this-state-fn
      [~acc ~evt]
      (when (seq ~evt)
	#(~@(expand-dispatch dispatch-type `(first ~evt) acc)
	 ~@(mapcat (partial expand-seq-evt-dispatch state-fn-map state-params (:from-state state) evt acc) (:transitions state))
	 :else [::no-event (~this-state-fn ~acc (rest ~evt))])))))

;;===================================================================================================
;; We want to turn an fsm-seq definition looking like this:
;;
;;  (fsm-seq
;;   [[:waiting-for-a
;;     #".*event a" -> :waiting-for-b]
;;    [:waiting-for-b
;;     #".*event b" -> :waiting-for-c
;;     #".*event c" -> {:emit emit-evt} :waiting-for-a]
;;    [:waiting-for-c
;;     #".*event c" -> :waiting-for-a]])
;;
;; into this implementation:
;;
;;  (letfn [(state-waiting-for-a
;; 	  [acc events]
;; 	  (when (seq events)
;; 	    #(match (first events)
;; 		      #".*event a"  [::no-event  (state-waiting-for-b acc (rest events))]
;; 		      :else [::no-event (state-waiting-for-a acc (rest events))])))
;; 	 (state-waiting-for-b
;; 	  [acc events]
;; 	  (when (seq events)
;; 	    #(match (first events)
;; 		      #".*event b" [::no-event (state-waiting-for-c acc (rest events))]
;; 		      #".*event c" [(emit-evt acc (first events)) (state-waiting-for-a acc (rest events))]
;; 		      :else [::no-event (state-waiting-for-b acc (rest events))])))
;; 	 (state-waiting-for-c
;; 	  [acc events]
;; 	  (when (seq events)
;; 	    #(match  (first events)
;; 		       #".*event c" [::no-event (state-waiting-for-a acc (rest events))]
;; 		       :else [::no-event (state-waiting-for-c acc (rest events))])))]
;;   (fn fsm-seq-fn
;;    ([events] (fsm-seq-fn nil events))
;;    ([acc events]
;;     (when (seq events)
;;       (reduce-fsm/fsm-seq-impl*
;;        (state-waiting-for-a acc events))))))
;;
(defmacro fsm-seq
"Returns an fsm function that produces lazy sequences from a finite state machine.
 The state machines can optionally add new values to the lazy sequence on transitions
 (with the :emit option) and may accumulate state in the same way as reduce.

The returned function will have the following 2 arities:
 [events]                    - accepts a sequence of events
 [val events]                - accepts an initial value for the accumulator and a sequence of events.
 [initial-state vals events] - start the seq fsm with a given state and accumulator value 

The generated function will produce a lazy sequence that ends when one of the following is true:
 - There are no more events in the event sequence
 - The fsm reaches a terminal state
 - The fsm reaches a state defined by a function and it returns a truthy value

Parameters:
 fsm      - the fsm definition (see below for syntax)
 fsm-opts - the following options are recognised:
  :default-acc val - sets the initial value for the accumulator in the single arity version of the function
  :dispatch - changes the way events are matched, the follow options are accepted:
    - :event-only (default) - events are matched using the  core.match/match syntax against the event only
    - :event-and-acc        - events use the default match syntax and are matched against [acc-value event]

FSM definitions:
 fsm's are defined as follows:
 [[state {:is-terminal true/false}?
   event -> {:emit emit-fn :action action-fn}? target-state
   event2 -> ...]
  [target-state ...]]

Where
 state  is a keyword or function
 state options (:is-terminal) are optional
 event  is any legal core.match pattern (see https://github.com/clojure/core.match)
 emit   is optional but its value must be a function, the return value will be added to the lazy sequence
 action is optional but its value must be a function if specified and their return value
        will be used as the new accumulated state

State and Event Functions:
 State functions are called with the current accumulated statelike so (state-fn acc).
 Emit functions are called with  (emit-fn acc event),
 Action functions are called with (action-fn acc event from-state to-state) where
   acc        - is the current accumulated state
   event      - is the event that fired the transition
   from-state - the state we're transitioning from
   to-state   - the state we're transitioning to

See https://github.com/cdorrat/reduce-fsm for examples and documentation"  
  [states & fsm-opts]
  (let [{:keys [dispatch default-acc] :or {dispatch :event-only}} fsm-opts
	state-maps  (create-state-maps states)
	state-fn-names (map state-fn-symbol (map :from-state state-maps))
	state-params (zipmap (map :from-state state-maps) (map :state-params state-maps))
	state-fn-map (zipmap (map :from-state state-maps) state-fn-names)] ;; map of state -> letfn function name
    `(letfn [~@(map #(state-seq-fn-impl dispatch state-fn-map state-params %) state-maps)]
       (with-meta
	 (fn fsm-seq-fn#
	   ([events#] (fsm-seq-fn# ~default-acc events#))
           ([acc# events#]
              (fsm-seq-fn# ~(-> state-maps first :from-state) acc# events#))
	   ([initial-state# acc# events#]
	      (when (seq events#)
		(fsm-seq-impl* ((lookup-state ~state-fn-map initial-state#)  acc# events#)))))
	 ~(fsm-metadata :fsm-seq state-maps)))))

(defmacro defsm-seq
  "A convenience macro to define an fsm sequence, equivalent to (def fsm-name (fsm-seq states opts)
   see reduce-fsm/fsm-seq for details"
  [name states & fsm-opts]
  `(def ~name (fsm-seq ~states ~@fsm-opts)))


;; ===================================================================================================
;; methods to display fsm

(defn- dot-exists
  "return true if the dot executable from graphviz is available on the path"
  [& _ ]
  (try
    (->> "dot -V"
	 (.exec (Runtime/getRuntime))
	 (.waitFor)
	 (= 0))
    (catch Exception e false)))

(defn- no-graphviz-message []
  (println "The dot executable from graphviz was not found on the path, unable to draw fsm diagrams")
  (println "Download a copy from http://www.graphviz.org/"))

(defn- graphviz-installed? []
  (if (dot-exists)
    true
    (do
      (no-graphviz-message)
      false)))


(defn- dorothy-edge
  "Create a single edge (transition) in a dorothy graph"
  [from-state trans]
  (let [label (str  " " (:evt trans)
		    (when (:action trans)
		      (str "\\n(" (-> trans :action meta :name str) ")") ))]
    (vector from-state (:to-state trans) {:label label} )))

(defn- dorothy-state
  "Create a single dorothy state"
  [fsm-type {:keys [params name state]}]
  (let [is-terminal? (if (= :fsm-filter fsm-type)
                       (not (:pass params true))
                       (or (:is-terminal params)
                           (= \( (first name))))]
    [state
     (merge {:label name} 
            (when is-terminal?
              {:style "filled,setlinewidth(2)"
               :fillcolor "grey88"}))]))

(defn- transitions-for-state
  "return a sequence of dorothy transitions for a single state"
  [state]
  (letfn [(transition-label [trans idx]
			    (str
			     (format "<TABLE BORDER=\"0\"><TR><TD TITLE=\"priority = %d\">%s</TD></TR>" idx (:evt trans))
			     (when (:action trans)
			       (format "<TR><TD>(%s)</TD></TR>" (:action trans)))
			     (when (:emit trans)
			       (format "<TR><TD>(%s) -&gt;</TD></TR>" (:emit trans)))
			     "</TABLE>"))
	  (format-trans [trans idx]
			[(:from-state trans) (:to-state trans) {:label (transition-label trans idx)} ])]
    (map format-trans (:transitions state) (range (count (:transitions state))))))

(defn fsm-dorothy
  "Create a dorothy digraph definition for an fsm"
  [fsm]
  (let [start-state (keyword (gensym "start-state"))
        state-map (->> fsm meta :reduce-fsm/states)
        fsm-type (->> fsm meta :reduce-fsm/fsm-type)]
    (d/digraph
      (concat
        [[start-state {:label "start" :style :filled :color :black :shape "point" :width "0.2" :height "0.2"}]]
        (map (partial dorothy-state fsm-type) state-map)
        [[start-state (-> state-map first :state)]]
        (mapcat transitions-for-state state-map)))))

(defn fsm-dot
  "Create the graphviz dot output for an fsm"
  [fsm]
    (d/dot (fsm-dorothy fsm)))

(defn- show-dorothy-fsm [fsm]
  (d/show! (fsm-dot fsm)))

(defn show-fsm
  "Display the fsm as a diagram using graphviz (see http://www.graphviz.org/)"
  [fsm]
  (when (graphviz-installed?)
    (show-dorothy-fsm fsm)))
  

(defn save-fsm-image
    "Save the state transition diagram for an fsm as a png.
Expects the following parameters:
  - fsm      - the fsm to render
  - filename - the output file for the png."
  [fsm filename]
  (when (graphviz-installed?)
    (d/save! (fsm-dot fsm) filename {:format :png}))
  nil)

