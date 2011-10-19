(ns reduce-fsm
  "Generate and display functional finite state machines that accumumlate state inthe same was as reduce.
This package allows you to:
 - Create basic fsm's  (see fsm)
 - Create lazy sequences from state machines (see fsm-seq)
 - Create stateful filter functions for use with filter/remove (see fsm-filter)"
  (:use [clojure.core.match
	 [core :only [match match-1]]	 
	 regex])
  (:require
   [clojure [set :as set]]
   [dorothy [core :as d]]
	    [clojure [string :as str]]
	    [vijual :only [draw-directed-graph]])
  )

(defn fn-sym?
  "is sym a symbol that resoves to a function in the current
   namespace or an anonymous function definition"
  [sym]
  (cond
   (symbol? sym) (when-let [a-var (ns-resolve *ns* sym)]
		   (fn? @a-var))
   (keyword? sym) false
   (seq sym) (#{'fn 'fn*} (first sym))
   :else false))

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
    ;; (mapcat (fn [s] (map #(vector (:from-state s) (:to-state %)) (:transitions s))) state-maps)]
    
    ;; all states must be either keywords or functions
    (doseq [state state-names ]
      (when (not (or (keyword? state) (fn-sym? state)))
	(report-compile-error "The state \"%s\" must be a keyword or valid function in the current namespace" state)))
    
    ;; all targets of a transition must exist
    (doseq [{:keys [from-state to-state]} transitions]
      (when-not (state-names to-state)	
	(report-compile-error "The state %s was referenced in a transition from %s but does not exist" to-state from-state)))
    
    ;; all actions must be functions
    (doseq [action (remove nil? (map :action transitions))]
      (when-not (fn-sym? action)
	(report-compile-error "The action \"%s\" is not a function in the current namespace" action)))
    
    ;; all emit values must be functions
    (doseq [action (remove nil? (map :action transitions))]
      (when-not (fn-sym? action)
	(report-compile-error "The emit function \"%s\" is not defined in the current namespace" action)))

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
	  (when-not (empty? xtra-keys)
	    (report-compile-warning "The key(s) %s was/were used in a transition from state %s, we only one expected or more of %s"
				    xtra-keys (:from-state t) user-keys)))))
    
    ;; check for unexpected keys in state params
    (let [expected-keys #{:pass :is-terminal}]
      (doseq [s state-maps]	
	(let [xtra-keys (set/difference (-> s :state-params keys set) expected-keys)]
	  (when-not (empty? xtra-keys)
	    (report-compile-warning "The key(s) %s was used in the state parameters for %s, we only expected one or more of expected %s"
				    xtra-keys (:from-state s) expected-keys)))))    
    ))

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
     :state-params (when (and (-> forms second map?) (->> forms (drop 3) first (= '->))) (second forms))
     :transitions (vec (map create-transition transitions))}))

(defn- create-state-maps
  "Create the sequence of maps that defines the fsm from vector representation.
  returns a list of maps similar to {:from-state :a-state :state-params {...} :transitions [{:to-state :another-state :evt 1}]}"
  [states]
  (let [state-maps  (map create-state-map states)]
    (sanity-check-fsm state-maps)
    state-maps))
  

(defn- state-fn-name
  "Create a name for the internal fuction that will represent this state.
   We want it to be recognisable to the user so stack traces are more intelligible"
  [sym]
  (let [name (cond
	      (fn? sym) (-> sym meta :name str)
	      (keyword? sym) (name sym)
	      :else (str sym))]
    (gensym (str "state" "-" name "-"))))

(defn- state-for-action [state]
  (cond
   (fn? state) (-> state meta :name keyword)
   (keyword? state) state
   :else (keyword state)))


(defn- expand-evt-dispatch
  "Expand the dispatch of a single event, this corresponds to a single case in the match expression"
  [state-fn-map from-state evt acc r evt-map]
  (let [target-state-fn (state-fn-map (:to-state evt-map))
	new-acc (gensym "new-acc")]   
    
    `[~(:evt evt-map)
      (let [~new-acc ~(if (:action evt-map)
			`(~(:action evt-map) ~acc ~evt ~(state-for-action from-state) ~(state-for-action (:to-state evt-map)))
			acc)]
	~(if (fn-sym? (:to-state evt-map))      ;; if the target state is a function we need to check for early conditional termination
	   `(if (~(:to-state evt-map) ~new-acc) ;; truthy return from a state function causes the fsm to exit
	      ~new-acc
	      (~target-state-fn ~new-acc (rest ~r)))
	   `(~target-state-fn ~new-acc (rest ~r))))])) ;; normal (keyword) states 

(defn- expand-dispatch [dispatch-type evt acc]
  (case dispatch-type
	:event-only [`match-1 evt]
	:event-and-acc   [`match [acc evt]]
	(throw (RuntimeException. "unknown fsm dispatch type, expected one of [:event-only :event-and-acc]"))))
  
(defn- state-fn-impl
  "define the function used to represent a single state internally"
  [dispatch-type state-fn-map state]
  (let [this-state-fn  (state-fn-map (:from-state state))
	events (gensym "events")
	acc (gensym "acc")
	evt (gensym "evt")]
    `(~this-state-fn
      [~acc ~events]
      (if-let [~evt (first ~events)] 
	#(~@(expand-dispatch dispatch-type evt acc)
		~@(mapcat (partial expand-evt-dispatch state-fn-map (:from-state state)  evt acc events) (:transitions state))
		:else (~this-state-fn ~acc (rest ~events))
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
   :name (if (fn-sym? (:from-state state))
		  (str "(" (:from-state state) ")")
		  (str (:from-state state)))
   :params (:state-params state)
   :transitions  (vec (transitions-metadata state))
   })


(defn- fsm-metadata
  "Create the metadata for the fsm. We'll use this at runtime to draw diagrams of the fsm"
  [fsm-type state-maps]
  {::fsm-type fsm-type
   ::states (vec (map state-metadata state-maps))
   })

;;===================================================================================================
;; We want to turn an fsm definition looking like this:
;;
;; (fsm [[:waiting-for-a
;;        #".*event a" -> :waiting-for-b]
;;       [:waiting-for-b
;;        #".*event d" -> :waiting-for-a
;;        #".*event c" -> {:action (fn [acc evt & _] (conj acc evt))} :waiting-for-a]])
;;
;; into the following trampoline based inplementation:
;;
;; (fn the-fsm
;;   ([events]
;;    (the-fsm nil events))
;;   ([acc events]
;;      (letfn [(state-waiting-for-a
;; 	      [acc events]
;; 	      (if-let [evt (first events)]
;; 		#(match-1 evt
;; 			  #".*event a" (state-waiting-for-b evt (rest events))
;; 			  :else        (state-waiting-for-a evt (rest events)))
;; 		acc))
;; 	     (state-waiting-for-b
;; 	      [acc events]
;; 	      (if-let [evt (first events)]       
;; 		#(match-1 evt
;; 			  #".*event d" (state-waiting-for-a acc (rest events))
;; 			  #".*event c" (let [new-acc ((fn [acc evt & _] (conj acc evt)) acc evt :waiting-for-b :waiting-for-a)]
;; 					 (state-waiting-for-a new-acc (rest events)))
;; 			  :else (state-waiting-for-b acc (rest events)))
;; 		acc))]
;;        (trampoline state-waiting-for-a acc events))))
(defmacro fsm
  "Returns an fsm function that reads a sequence of events and returns
an accumulated value (like reduce). The returned function will have the following 2 arities:
 [events]     - accepts a sequence of events
 [val events] - accepts an initial value for the accumulator and a sequence of events.

The generated function will return when one of the following is true:
 - There are no more events in the event sequence
 - The fsm reaches a terminal state
 - The fsm reaches a state defined by a function and it returns a truthy value

Parmaters:
 fsm      - the fsm definition (see below for syntax)
 fsm-opts - the following options are recognised:
  :default-acc val - sets the initial value for the acculator in the single arity version function
  :dispatch - changes the way events are matched, the follow options are accepted:
    - :event-only (default) - events are matched using the  core.match/match-1 syntax against the event only
    - :event-and-acc        - events use the default match syntax and are matched against [acc-value event]

FSM definitions:
 fsm's are defined as follows:
 [[state {:is-terminal true/false}?
   event -> {:action a-fn}? target-state
   event2 -> ...]
  [target-state ...]]

Where
 state  is a keyword or function
 state  options (:is-terminal) are optional
 event  is any legal core.match pattern (see https://github.com/clojure/core.match)
 action is optional but must be a function if specified and their return value
        will be used as the new accumulated state

State and Event Functions:
 State functions are called like so (state-fn acc) where acc is the current accumulated state.
 Event functions are called with (event-fn acc event from-state to-state) where
   acc   - is the current accumulated state
   event - is the event that fired the transition
   from-state - the state we're transitionin from
   to-state   - the state we're transitioning to

See https://github.com/cdorrat/reduce-fsm for examples and documentation"

  [states & fsm-opts]
  (let [{:keys [dispatch default-acc] :or {dispatch :event-only}} fsm-opts 
	state-maps  (create-state-maps states)
	state-fn-names (map state-fn-name (map :from-state state-maps))
	state-fn-map (zipmap (map :from-state state-maps) state-fn-names)] ;; map of state -> letfn function name    
    `(with-meta
       (fn the-fsm#
	([events#] (the-fsm# ~default-acc events#))
	([acc# events#]
	  (letfn [~@(map #(state-fn-impl dispatch state-fn-map %) state-maps)]
	    (trampoline ~(first state-fn-names) acc# events#)
	    )))
       ~(fsm-metadata :fsm state-maps)
       )))

(defmacro defsm
  "A convenience macro to define a fsm, equivalent to (def fsm-name (fsm states opts)
   see reduce-fsm/fsm for details"
  [fsm-name states & opts]
  `(def ~fsm-name (fsm ~states ~@opts)))

;;===================================================================================================
;; fsm-filter impl

(defn- expand-filter-evt-dispatch
  "Expand the dispatch line for a single fsm-filter dipatch line.
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
	 :else [~(get (:state-params state) :pass true) (~this-state-fn ~acc)]
	))
	)))

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
;; 	   (match-1 evt
;; 		    3 [false (state-suppressing acc)]
;; 		    :else [true (state-initial acc)])))
;; 	(state-suppressing [acc]
;; 	 (fn  [evt]
;; 	   (match-1 evt
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

Parmaters:
fsm      - the fsm definition (see below for syntax)
fsm-opts - the following options are recognised:
  :default-acc val - sets the initial value for the acculator in the single arity version function
  :dispatch - changes the way events are matched, the follow options are accepted:
    - :event-only (default) - events are matched using the  core.match/match-1 syntax against the event only
    - :event-and-acc        - events use the default match syntax and are matched against [acc-value event]

FSM definitions:
filters's are defined as follows:
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
  acc   - is the current accumulated state
  event - is the event that fired the transition
  from-state - the state we're transitionin from
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
	state-fn-names (map state-fn-name (map :from-state state-maps))
	state-params (zipmap (map :from-state state-maps) (map :state-params state-maps))
	state-fn-map (zipmap (map :from-state state-maps) state-fn-names)] ;; map of state -> letfn function name
    `(letfn [~@(map #(state-filter-fn-impl dispatch state-fn-map state-params %) state-maps)]
       (with-meta
	 (fn filter-builder#
	   ([] (filter-builder# ~default-acc))
	   ([acc#]
	      (let [curr-state# (atom (~(first state-fn-names) acc#))]
		(fn [evt#]
		  (let [[pass# next-state#] (@curr-state# evt#)]
		    (reset! curr-state# next-state#)
		    pass#)))))
	 ~(fsm-metadata :fsm-filter state-maps)))))
          
(defmacro defsm-filter [name states & fsm-opts]
  `(def ~name (fsm-filter states ~@fsm-opts)))
  


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

(defn fsm-seq-impl*
  "Create a lazy sequence from a fsm-seq state function" 
  [f]
  (let [[emitted next-step] (next-emitted f)]
    (lazy-seq
     (if next-step
       (cons emitted (fsm-seq-impl* next-step))
       (when (not= ::no-event emitted)
	 (cons emitted nil))))))


(defn- expand-seq-evt-dispatch
  "Expand the dispatch line for a single fsm-seq dipatch line.
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
	 :else [::no-event (~this-state-fn ~acc (rest ~evt))]
	)))))

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
;; 	    #(match-1 (first events)
;; 		      #".*event a"  [::no-event  (state-waiting-for-b acc (rest events))]
;; 		      :else [::no-event (state-waiting-for-a acc (rest events))])))
;; 	 (state-waiting-for-b
;; 	  [acc events]
;; 	  (when (seq events)
;; 	    #(match-1 (first events)
;; 		      #".*event b" [::no-event (state-waiting-for-c acc (rest events))]
;; 		      #".*event c" [(emit-evt acc (first events)) (state-waiting-for-a acc (rest events))]
;; 		      :else [::no-event (state-waiting-for-b acc (rest events))])))	
;; 	 (state-waiting-for-c
;; 	  [acc events]
;; 	  (when (seq events)
;; 	    #(match-1  (first events)
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
 [events]     - accepts a sequence of events
 [val events] - accepts an initial value for the accumulator and a sequence of events.

The generated function will produce a lazy seqnuence that ends when one of the following is true:
 - There are no more events in the event sequence
 - The fsm reaches a terminal state
 - The fsm reaches a state defined by a function and it returns a truthy value

Parmaters:
 fsm      - the fsm definition (see below for syntax)
 fsm-opts - the following options are recognised:
  :default-acc val - sets the initial value for the acculator in the single arity version function
  :dispatch - changes the way events are matched, the follow options are accepted:
    - :event-only (default) - events are matched using the  core.match/match-1 syntax against the event only
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
   acc   - is the current accumulated state
   event - is the event that fired the transition
   from-state - the state we're transitionin from
   to-state   - the state we're transitioning to

See https://github.com/cdorrat/reduce-fsm for examples and documentation"  
  [states & fsm-opts]
  (let [{:keys [dispatch default-acc] :or {dispatch :event-only}} fsm-opts 
	state-maps  (create-state-maps states)
	state-fn-names (map state-fn-name (map :from-state state-maps))
	state-params (zipmap (map :from-state state-maps) (map :state-params state-maps))
	state-fn-map (zipmap (map :from-state state-maps) state-fn-names)] ;; map of state -> letfn function name
    `(letfn [~@(map #(state-seq-fn-impl dispatch state-fn-map state-params %) state-maps)]
       (with-meta
	 (fn fsm-seq-fn#
	   ([events#] (fsm-seq-fn# ~default-acc events#))
	   ([acc# events#]
	      (when (seq events#)
		(fsm-seq-impl* (~(first state-fn-names) acc# events#)))))
	 ~(fsm-metadata :fsm-seq state-maps)))))

(defmacro defsm-seq [name states & fsm-opts]
  `(def ~name (fsm-seq states ~@fsm-opts)))


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

(defmulti show-fsm "show the fsm using graphviz if available, vijual if it's not" (memoize dot-exists))
(defmulti save-fsm-image "save the state transition diagram for an fsm as a png. Expects an fsm and filename parameters" (memoize dot-exists))

(defn- dorothy-edge [from-state trans]
  (let [label (str  " " (:evt trans)
		    (when (:action trans)
		      (str "\\n(" (-> trans :action meta :name str) ")") ))]
    (vector from-state (:to-state trans) {:label label} )
    ))

(defn- transitions-for-state [state]
  (letfn [(transition-label [trans idx]
			    (str
			     (format "<TABLE BORDER=\"0\"><TR><TD TITLE=\"priority = %d\">%s</TD></TR>" idx (:evt trans) )
			     (when (:action trans)
			       (format "<TR><TD>(%s)</TD></TR>" (:action trans)))
			     (when (:emit trans)
			       (format "<TR><TD>(%s) -&gt;</TD></TR>" (:emit trans)))
			     "</TABLE>"))
	  (format-trans [trans idx]
			[(:from-state trans) (:to-state trans) {:label (transition-label trans idx)} ])]
    (map format-trans (:transitions state) (range (count (:transitions state))))))

(defn- dorothy-fsm-dot
  "Create the graphviz dot outptu for an fsm"
  [fsm]
  (let [start-state (keyword (gensym "start-state"))
	state-map (->> fsm meta ::states)]
    (-> (d/digraph
	 (concat
	  [[start-state {:label "start" :style :filled :color :black :shape "point" :width 0.2 :height 0.2}]]
	  (map #(vector (:state %) {:label (:name %)}) state-map)
	  [[start-state (-> state-map first :state)]]
	  (mapcat transitions-for-state state-map)))
	d/dot)))


(defn- show-dorothy-fsm [fsm]
  (d/show! (dorothy-fsm-dot fsm)))

(defmethod show-fsm true 
  [fsm]
  (show-dorothy-fsm fsm))
  

(defmethod save-fsm-image true 
  [fsm filename]
  (d/save! (dorothy-fsm-dot fsm) filename))


;; The vijual library would have been a nice alternative to graphviz with no external dependencies
;; however the current clojars versions seem broken an wont draw directed graphs (even the examples in the documentation)
;; This implementation should work if fixed

;;(defn- fsm-to-vijual [fsm]
;;  (mapcat #(map (fn [trans] (vector (:from-state %) (:to-state trans))) (:transitions %))
;;	  (-> fsm meta ::states)))

;; (defmethod show-fsm false
;;   [fsm]
;;   (vijual/draw-directed-graph
;;    (fsm-to-vijual fsm)))

;; (defmethod save-fsm-image false
;;   [fsm filename]
;;   (vijual/save-image
;;    (vijual/draw-directed-graph-image (fsm-to-vijual fsm))
;;    filename))

(defn- no-grapiz-message []
  (println "The dot executable from graphviz was not found on the path, unable to draw fsm diagrams")
  (println "Download a copy from http://www.graphviz.org/"))

(defmethod show-fsm false [fsm]
  (no-grapiz-message))

(defmethod save-fsm-image false [fsm]
  (no-grapiz-message))	   

(comment
  (pprint
   (create-state-map '[:locked
		       #"[0-9]" -> {:action store-code} :locked
		       "*" -> :locked
		       "#" -> {:guard code-matches :action unlock-door} :unlocked]))

  

  ;; ===================================================================================================
  ;; sample of searching a log for a sequence of events
  (defn save-line [state evt from-state to-state]
    (conj state evt))
  
    (deffsm log-search [[:waiting-for-a
			 #".*event a" -> :seen-a]
			[:seen-a
			 #".*event b" -> :waiting-for-c
			 #".*event c" -> {:action save-line} :waiting-for-a
			 #".*event d" -> exit-now]
			[:waiting-for-c
			 #".*event c" -> :waiting-for-a]])
  
  (deffsm log-search [[:waiting-for-a
		       [#".*event a"] -> :seen-a]
		      [:seen-a
		       [#".*event b"] -> :waiting-for-c
		       [#".*event c"] -> {:action save-line} :waiting-for-a
		       [#".*event d"] -> exit-now]
		      [:waiting-for-c
		       [#".*event c"] -> :waiting-for-a]])

  (log-search [] (ds/read-lines "my-log.txt"))
  
  (log-search2 [] ["1 event a" "2 event b" "3 event c" "4 event a" "5 event c" "6 event a"])
  )


;; macro expansion
(defn save-line [state evt from-state to-state]
  (conj state evt))

(defn exit-fsm [state]
  true)

(defn log-search2 [state events]
  (letfn [(state-waiting-for-a [state [evt & r]]
			       (if evt
				 #(match [evt]
					 [#".*event a"] (state-seen-a state r)
					 :else (state-waiting-for-a state r))
				 state))
 	  (state-seen-a [state [evt & r]]
 			(if evt
 			  #(match [evt]
 				  [#".*event b"] (state-waiting-for-c state r)
 				  [#".*event c"] (state-waiting-for-a (save-line state evt :seen-a :waiting-for-a) r)
				  [#".*event d"] (let [new-state (save-line state evt :seen-a "exit-fsm")]
						   (if (exit-fsm new-state)
						     new-state
						     (state-seen-a new-state r)))
 				  :else (state-seen-a state r))
 			  state))
 	  (state-waiting-for-c [state [evt & r]]
 			       (if [evt]
 				 #(match [evt]
 					 [#".*event c"] (state-waiting-for-a  state  r)
					 :else (state-waiting-for-c state r))
				 state))]
    (trampoline state-waiting-for-a state events)))



