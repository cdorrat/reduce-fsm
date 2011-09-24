(ns reduce-fsm
  "Generate and display functional finate state machines"
  (:use [clojure pprint]
	[clojure.core.match
	 [core :only [match]]
	 regex])
  (:require [vijual :only [draw-directed-graph]]
	    [dorothy [core :as d]]
	    )
  )

;; fsm-seq    - creates lazy sequence based on an fsm
;; fsm-filter - stateful filters for use with filter/remove

(defn- default-guard [evt & r]
  true)


(defn- create-transition
  "create a single transition make with default params if none specified"
  [[from to]]
  (let [has-params? (map? (first to))
	{:keys [guard action]} (if has-params? (first to) {:guard (symbol `default-guard) })
	to-state (if has-params? (second to) (first to))]
    {:evt (last from)
     :guard guard
     :action action
     :to-state to-state}))

(defn- create-state-map
  "Create an entry for a single '[:state evt1 -> :state1 :evt2 -> :state2 ...]"
  [forms]
  (let [from-state (first forms)
	transitions (partition 2 1
			       (remove #(= '-> (first %))
				       (partition-by #(= '-> %) (rest forms))))]
    {:from-state from-state
     :transitions (vec (map create-transition transitions))}))

(defn fn-sym?
  "is sym a symbol that resoves to a function in the current namespace"
  [sym]
  (when (symbol? sym)
    (when-let [a-var (ns-resolve *ns* sym)]
      (fn? @a-var))))

(defn- state-fn-name [sym]
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

(defn- report-compile-error [& args]
  (let [msg (apply format args)]
    (println (str "error: " msg))
    (throw (Exception. (str "FSM compilation exception: " msg)))))

(defn- expand-evt-dispatch [state-fn-map from-state evt acc r evt-map]
  (let [target-state-fn (state-fn-map (:to-state evt-map))
	new-acc (gensym "new-acc")]   
    
    (when (nil? target-state-fn)
      (report-compile-error "The state %s was referenced in a transition from %s but does not exist" (:to-state evt-map) (str from-state)))

    `[~(:evt evt-map)
      (let [~new-acc ~(if (:action evt-map)
			`(~(:action evt-map) ~acc ~evt ~(state-for-action from-state) ~(state-for-action (:to-state evt-map)))
			acc)]
	~(if (fn-sym? (:to-state evt-map))      ;; if the target state is a function we need to check for early conditional termination
	   `(if (~(:to-state evt-map) ~new-acc) ;; truthy return from a state function causes the fsm to exit
	      ~new-acc
	      (~target-state-fn ~new-acc ~r))
	   `(~target-state-fn ~new-acc ~r)))])) ;; normal (keyword) states 

(defn- state-fn-impl [state-fn-map state]
  (let [this-state-fn  (state-fn-map (:from-state state))
	rst (gensym "rst")
	acc (gensym "acc")
	evt (gensym "evt")]
    `(~this-state-fn
      [~acc [~evt & ~rst]]
      (if ~evt
	#(match [~evt]
		~@(mapcat (partial expand-evt-dispatch state-fn-map (:from-state state)  evt acc rst) (:transitions state))
		:else (~this-state-fn ~acc ~rst)
		)
	~acc))))
  

(defmacro fsm [states]
  (let [state-maps  (map create-state-map states)
	state-fn-names (map state-fn-name (map :from-state state-maps))
	state-fn-map (zipmap (map :from-state state-maps) state-fn-names)] ;; map of state -> letfn function name
    `(with-meta
       (fn the-fsm#
	([events#] (the-fsm# nil events#))
	([acc# events#]
	  (letfn [~@(map #(state-fn-impl state-fn-map %) state-maps)]
	    (trampoline ~(first state-fn-names) acc# events#)
	    )))
       {::states ~(vec state-maps)}
       )))


(defmacro deffsm [fsm-name states]
  `(def ~fsm-name (fsm ~states)))

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

(defn- dorothy-edge [from-state trans]
  (let [label (str  " " (:evt trans)
		    (when (:action trans)
		      (str "\\n(" (-> trans :action meta :name str) ")") ))]
    (vector from-state (:to-state trans) {:label label} )
    ))

(defmethod show-fsm true 
  [fsm]
  (-> (d/digraph
       (mapcat #(map (partial dorothy-edge (:from-state %)) (:transitions %))
	       (-> fsm meta :reduce-fsm/states)))
      d/dot
      d/show!))

(defmethod show-fsm false  [fsm]
  (vijual/draw-directed-graph
   (mapcat #(map (fn [trans] (vector (:from-state %) (:to-state trans))) (:transitions %))
	       (-> fsm meta :reduce-fsm/states))))

  

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

(comment 
  ;; sample action code with state functions
  (let [new-state (default-action state evt :seen-a :waiting-for-c)]
    (if (a-state-fn new-state) ;; true return from a state function exits
      new-state
      (state-waiting-for-c new-state r)))

  ;; sample action code without  state functions
  (state-waiting-for-c (default-action state evt :seen-a :waiting-for-c) r)

  (state-waiting-for-c state r)
  )
