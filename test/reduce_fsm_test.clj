(ns reduce-fsm-test
  (:use [reduce-fsm])
  (:use [clojure.test]))

(defn- test-save-line [state evt from-state to-state]
  (conj state evt))

(defsm log-search-fsm-test
  [[:waiting-for-a
    #".*event a" -> :waiting-for-b]
   [:waiting-for-b
    #".*event b" -> :waiting-for-c
    #".*event c" -> {:action test-save-line} :waiting-for-a]
   [:waiting-for-c
    #".*event c" -> :waiting-for-a]])


(deftest simple-fsm-behaviour
  
  (are [res acc events] (= res (log-search-fsm-test acc events))       
       ["5 event c"]  []  ["1 event a" "event x" "2 event b" "3 event c" "4 event a" "event x" "5 event c" "6 event a"]
       nil nil ["0 event c" "1 event a" "2 event b" "3 event c"]
       [] [] ["0 event c" "1 event a" "2 event b" "3 event c" ]))
   

(deftest dispatch-with-acc
  (let [an-fsm (fsm
		[[:waiting-for-a
		  [_ #".*event a"] -> :waiting-for-b]
		 [:waiting-for-b
		  [_ #".*event b"] -> :waiting-for-c
		  [_ #".*event c"] -> {:action test-save-line} :waiting-for-a]
		 [:waiting-for-c
		  [_ #".*event c"] -> :waiting-for-a]] :dispatch :event-and-acc)]

  (are [res acc events] (= res (an-fsm acc events))       
       ["5 event c"]  []  ["1 event a" "event x" "2 event b" "3 event c" "4 event a" "event x" "5 event c" "6 event a"]
       nil nil ["0 event c" "1 event a" "2 event b" "3 event c"]
       [] [] ["0 event c" "1 event a" "2 event b" "3 event c" ])
  ))

(deftest single-dispatch-with-when
  (let [save-to-state (fn [acc evt from to] (conj acc to))
	an-fsm (fsm
		[[:initial 
		  (n :when #(< % 5)) -> {:action save-to-state} :small
		  (n :when even?) -> {:action save-to-state} :even]  ;; match guards take the last value if multiple match?
		 [:small
		  1 -> {:action save-to-state} :initial]
		 [:even
		  (n :when odd?) -> {:action save-to-state} :initial]])]
    (is (= [:even :initial :small :initial] (an-fsm [] [2 2 4 3 1 2 2 1])))))


(deftest display-dorothy-fsm-test
  (let [frame (#'reduce-fsm/show-dorothy-fsm log-search-fsm-test)]
    (is (not (nil? frame)))
    (when frame
      (.dispose frame))
    ))

;; fails - need to investigate vijual
;;(deftest display-vijual-fsm-test
;;  (#'reduce-fsm/show-vijual-fsm log-search-fsm))

 
  
;;(deftest exit-with-state-fn
;;  (is false "write test for fsm that conditionally exits with a state fn")) 


;;(deftest fsm-with-guards
;;  (is false "write test for fsm that guard transitions"))

(deftest simple-fsm-seq
  (let [emit-evt (fn [acc evt] evt)
	log-seq (fsm-seq 
		 [[:waiting-for-a
		   #".*event a" -> :waiting-for-b]
		  [:waiting-for-b
		   #".*event b" -> :waiting-for-c
		   #".*event c" -> {:emit emit-evt} :waiting-for-a]
		  [:waiting-for-c
		   #".*event c" -> :waiting-for-a]])]

    
    (are [res events] (= res (doall (log-seq [] events)))
	 ["5 event c"]  ["1 event a" "event x" "2 event b" "3 event c" "4 event a" "event x" "5 event c" "6 event a"]
	 []  ["1 event a" "2 event b" "3 event c"]
	 ["2 event c" "4 event c"]  ["x na-event" "1 event a" "2 event c" "3 event a" "4 event c"])))
  
;;===================================================================================================
;; fsm-filter tests

(deftest simple-fsm-filter
  (let [a-filter (fsm-filter [[:initial {:pass true}
			       3 -> :suppressing]
			      [:suppressing {:pass false}
			       6 -> :initial]])]
    (is (= [1 2 6 1 2] (filter (a-filter) [1 2 3 4 5 1 2 6 1 2])))))

  

(comment ;; fix this test
(deftest test-create-state-map
	 (let [no-state-parms ['[:from-state :evt -> :to-state]
			       '[:from-state {:foo 7} -> :to-state]
			       '[:from-state :evt -> {:action an-action} :to-state]
			       '[:from-state [:evt _] -> {:action an-action} :to-state]
			       '[:from-state [:evt (n :when even?)] -> {:action an-action} :to-state]]
	       with-state-params ['[:from-state {:pass true} :evt -> :to-state]
				  '[:from-state {:pass true} {:foo 7} -> :to-state]
				  '[:from-state {:pass true} {:foo 7} -> {:action an-action} :to-state]
				  '[:from-state {:pass true} [{:foo 7} _] -> {:action an-action} :to-state]
				  '[:from-state {:pass true} [{:foo 7} (n :when even?)] -> {:action an-action} :to-state]]
	       state-matches (fn [m from-state state-params evt action to-state]
			       (and
				(= from-state (:from-state m))
				(= state-params (:state-params m))
				(= evt (-> m :transitions :evt))
				(= action (-> m :transitions :action))
				(= to-state (-> m :transitions :to-state))))]
					       
			       
	   
	   (doseq [p no-state-parms]
	     (are [from-state state-params evt action to-state] (state-matches (#'reduce-fsm/create-state-map p) from-state state-params evt action to-state)
		  :from-state nil :evt nil :to-state
		  :from-state nil '{:foo 7} nil :to-state
		  :from-state nil :evt  'an-action :to-state
		  :from-state nil '[:evt _] '{:action an-action} :to-state
		  :from-state nil '[:evt (n :when even?)] 'an-action :to-state))))

)		  
     


(comment
  ;; drop all values after we see a 3 until we see a 5
  (deffsm-filter tst-filter  [[:passing-values {:pass true}
			       3 => :suppressing-values]
			      [:suppressing-values {:pass false}
			       5 => :passing-values]]
    :dispatch :case)

  
  
  (is (= [ 1 2 5 6] (filter tst-filter  [1 2 3 1 2 6 7 5 6])))
  
  (defn emit-evt [state evt from to]
    evt)
  
  (deffsm-seq log-seq 
    [[:waiting-for-a
      [#".*event a"] -> :waiting-for-b]
     [:waiting-for-b
      [#".*event b"] -> :waiting-for-c
      [#".*event c"] -> {:emit emit-evt} :waiting-for-a]
     [:waiting-for-c
      [#".*event c"] -> :waiting-for-a]] :dispatch :match)
  
  ;; same as fsm log seq but emits log lines as it finds them
  ;; :action options can still be used to update internal state
  (log-seq [event...])

  (deffsm-filter stateful-filter [[:waiting-for-3 {:pass true}
				   3 => :waiting-for-6]
				  [:waiting-for-6 {:pass false}
				   6 => :waiting-for-3]])
  )

