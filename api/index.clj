{:namespaces
 ({:source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj",
   :wiki-url
   "http://cdorrat.github.com/reduce-fsm/reduce-fsm-api.html",
   :name "reduce-fsm",
   :doc
   "Generate and display functional finite state machines that accumumlate state\nin the same way as reduce.\nThis package allows you to:\n - Create basic fsm's (see fsm)\n - Create lazy sequences from state machines (see fsm-seq)\n - Create stateful filter functions for use with filter/remove (see fsm-filter)\n - Visualise state machines as"}),
 :vars
 ({:arglists ([fsm-name states & opts]),
   :name "defsm",
   :namespace "reduce-fsm",
   :source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj#L289",
   :raw-source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj",
   :wiki-url
   "http://cdorrat.github.com/reduce-fsm//reduce-fsm-api.html#reduce-fsm/defsm",
   :doc
   "A convenience macro to define a fsm, equivalent to (def fsm-name (fsm states opts)\nsee reduce-fsm/fsm for details",
   :var-type "macro",
   :line 289,
   :file "/home/cnd/src/clojure/reduce-fsm/src/reduce_fsm.clj"}
  {:arglists ([name states & fsm-opts]),
   :name "defsm-filter",
   :namespace "reduce-fsm",
   :source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj#L419",
   :raw-source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj",
   :wiki-url
   "http://cdorrat.github.com/reduce-fsm//reduce-fsm-api.html#reduce-fsm/defsm-filter",
   :doc
   "A convenience macro to define an fsm filter, equivalent to (def fsm-name (fsm-filter states opts)\nsee reduce-fsm/fsm-filter for details",
   :var-type "macro",
   :line 419,
   :file "/home/cnd/src/clojure/reduce-fsm/src/reduce_fsm.clj"}
  {:arglists ([name states & fsm-opts]),
   :name "defsm-seq",
   :namespace "reduce-fsm",
   :source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj#L585",
   :raw-source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj",
   :wiki-url
   "http://cdorrat.github.com/reduce-fsm//reduce-fsm-api.html#reduce-fsm/defsm-seq",
   :doc
   "A convenience macro to define an fsm sequence, equivalent to (def fsm-name (fsm-seq states opts)\nsee reduce-fsm/fsm-seq for details",
   :var-type "macro",
   :line 585,
   :file "/home/cnd/src/clojure/reduce-fsm/src/reduce_fsm.clj"}
  {:arglists ([states & fsm-opts]),
   :name "fsm",
   :namespace "reduce-fsm",
   :source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj#L230",
   :raw-source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj",
   :wiki-url
   "http://cdorrat.github.com/reduce-fsm//reduce-fsm-api.html#reduce-fsm/fsm",
   :doc
   "Returns an fsm function that reads a sequence of events and returns\nan accumulated value (like reduce). The returned function will have the following 2 arities:\n [events]     - accepts a sequence of events\n [val events] - accepts an initial value for the accumulator and a sequence of events.\n\nThe generated function will return when one of the following is true:\n - There are no more events in the event sequence\n - The fsm reaches a terminal state\n - The fsm reaches a state defined by a function and it returns a truthy value\n\nParmaters:\n fsm      - the fsm definition (see below for syntax)\n fsm-opts - the following options are recognised:\n  :default-acc val - sets the initial value for the accumulator in the single arity version of the function\n  :dispatch - changes the way events are matched, the follow options are accepted:\n    - :event-only (default) - events are matched using the  core.match/match-1 syntax against the event only\n    - :event-and-acc        - events use the default match syntax and are matched against [acc-value event]\n\nFSM definitions:\n fsm's are defined as follows:\n [[state {:is-terminal true/false}?\n   event -> {:action a-fn}? target-state\n   event2 -> ...]\n  [target-state ...]]\n\nWhere\n state  - is a keyword or function\n state  - options (:is-terminal) are optional\n event  - is any legal core.match pattern (see https://github.com/clojure/core.match)\n action - is optional but must be a function if specified and the return value\n          will be used as the new accumulated state.\n\nState and Event Functions:\n State functions are called like so (state-fn acc) where acc is the current accumulated state.\n Event functions are called with (event-fn acc event from-state to-state) where\n   acc        - is the current accumulated state\n   event      - is the event that fired the transition\n   from-state - the state we're transitionin from\n   to-state   - the state we're transitioning to\n\nSee https://github.com/cdorrat/reduce-fsm for examples and documentation",
   :var-type "macro",
   :line 230,
   :file "/home/cnd/src/clojure/reduce-fsm/src/reduce_fsm.clj"}
  {:arglists ([states & fsm-opts]),
   :name "fsm-filter",
   :namespace "reduce-fsm",
   :source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj#L356",
   :raw-source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj",
   :wiki-url
   "http://cdorrat.github.com/reduce-fsm//reduce-fsm-api.html#reduce-fsm/fsm-filter",
   :doc
   "Returns a function that returns fsm filters suitable for use with clojure.core/filter and remove.\nEach state in the fsm definition has a :pass attribute that will be returned from the generated function\nwhen it is in that state.\n\nThe returned function will have the following 2 arities:\n []    - creates a filter with the default accumulator state (nil or the value from :default-acc)\n [val] - accepts an initial value for the accumulator \n\nParmaters:\nfsm      - the fsm definition (see below for syntax)\nfsm-opts - the following options are recognised:\n  :default-acc val - sets the initial value for the accumulator in the single arity version function\n  :dispatch - changes the way events are matched, the follow options are accepted:\n    - :event-only (default) - events are matched using the  core.match/match-1 syntax against the event only\n    - :event-and-acc        - events use the default match syntax and are matched against [acc-value event]\n\nFSM definitions:\nfilters's are defined as follows:\n [[state {:pass true/false}?\n   event -> {:action a-fn}? target-state]\n  [target-state ...]]\n\nWhere\n  state is a keyword \n  state option (:pass) is optional, it defaults to true\n  event is any legal core.match pattern (see https://github.com/clojure/core.match)\n  action is optional but must be a function if specified and their return value\n         will be used as the new accumulated state\n\nEvent functions are called with (event-fn acc event from-state to-state) where\n  acc        - is the current accumulated state\n  event      - is the event that fired the transition\n  from-state - the state we're transitionin from\n  to-state   - the state we're transitioning to\n\nExample:\n  Suppress numbers after seeing a 3 until we see a 6.\n\n  (def f (fsm-filter [[:initial \n \t               3 -> :suppressing]\n \t              [:suppressing {:pass false}\n \t               6 -> :initial]]))\n\n  (= [1 2 6 1 2] (filter (f) [1 2 3 4 5 1 2 6 1 2]))",
   :var-type "macro",
   :line 356,
   :file "/home/cnd/src/clojure/reduce-fsm/src/reduce_fsm.clj"}
  {:arglists ([states & fsm-opts]),
   :name "fsm-seq",
   :namespace "reduce-fsm",
   :source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj#L523",
   :raw-source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj",
   :wiki-url
   "http://cdorrat.github.com/reduce-fsm//reduce-fsm-api.html#reduce-fsm/fsm-seq",
   :doc
   "Returns an fsm function that produces lazy sequences from a finite state machine.\n The state machines can optionally add new values to the lazy sequence on transitions\n (with the :emit option) and may accumulate state in the same way as reduce.\n\nThe returned function will have the following 2 arities:\n [events]     - accepts a sequence of events\n [val events] - accepts an initial value for the accumulator and a sequence of events.\n\nThe generated function will produce a lazy seqnuence that ends when one of the following is true:\n - There are no more events in the event sequence\n - The fsm reaches a terminal state\n - The fsm reaches a state defined by a function and it returns a truthy value\n\nParmaters:\n fsm      - the fsm definition (see below for syntax)\n fsm-opts - the following options are recognised:\n  :default-acc val - sets the initial value for the acculator in the single arity version of the function\n  :dispatch - changes the way events are matched, the follow options are accepted:\n    - :event-only (default) - events are matched using the  core.match/match-1 syntax against the event only\n    - :event-and-acc        - events use the default match syntax and are matched against [acc-value event]\n\nFSM definitions:\n fsm's are defined as follows:\n [[state {:is-terminal true/false}?\n   event -> {:emit emit-fn :action action-fn}? target-state\n   event2 -> ...]\n  [target-state ...]]\n\nWhere\n state  is a keyword or function\n state options (:is-terminal) are optional\n event  is any legal core.match pattern (see https://github.com/clojure/core.match)\n emit   is optional but its value must be a function, the return value will be added to the lazy sequence\n action is optional but its value must be a function if specified and their return value\n        will be used as the new accumulated state\n\nState and Event Functions:\n State functions are called with the current accumulated statelike so (state-fn acc).\n Emit functions are called with  (emit-fn acc event),\n Action functions are called with (action-fn acc event from-state to-state) where\n   acc        - is the current accumulated state\n   event      - is the event that fired the transition\n   from-state - the state we're transitionin from\n   to-state   - the state we're transitioning to\n\nSee https://github.com/cdorrat/reduce-fsm for examples and documentation",
   :var-type "macro",
   :line 523,
   :file "/home/cnd/src/clojure/reduce-fsm/src/reduce_fsm.clj"}
  {:raw-source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj",
   :source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj#L606",
   :wiki-url
   "http://cdorrat.github.com/reduce-fsm//reduce-fsm-api.html#reduce-fsm/save-fsm-image",
   :namespace "reduce-fsm",
   :line 606,
   :file "/home/cnd/src/clojure/reduce-fsm/src/reduce_fsm.clj",
   :var-type "multimethod",
   :doc
   "Save the state transition diagram for an fsm as a png.\nExpects the following parameters:\n  - fsm      - the fsm to render\n  - filename - the output file for the png.",
   :name "save-fsm-image"}
  {:raw-source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj",
   :source-url
   "https://github.com/cdorrat/reduce-fsm/b55ff99cb0510158e838ad9159bb25c64d5aab43/src/reduce_fsm.clj#L605",
   :wiki-url
   "http://cdorrat.github.com/reduce-fsm//reduce-fsm-api.html#reduce-fsm/show-fsm",
   :namespace "reduce-fsm",
   :line 605,
   :file "/home/cnd/src/clojure/reduce-fsm/src/reduce_fsm.clj",
   :var-type "multimethod",
   :doc
   "Display the fsm as a diagram using graphviz (see http://www.graphviz.org/)",
   :name "show-fsm"})}
