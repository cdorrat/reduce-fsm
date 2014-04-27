(ns reduce-fsm-test  
  (:use [clojure.test])
  (:use [reduce-fsm])
  (:import [java.awt.Frame]))

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
		  (n :guard #(< % 5)) -> {:action save-to-state} :small
		  (n :guard even?) -> {:action save-to-state} :even]  ;; match guards take the last value if multiple match?
		 [:small
		  1 -> {:action save-to-state} :initial]
		 [:even
		  (n :guard odd?) -> {:action save-to-state} :initial]])]
    (is (= [:even :initial :small :initial] (an-fsm [] [8 2 4 3 1 2 2 1])))))


(deftest display-dorothy-fsm-test
  (let [frame (#'reduce-fsm/show-dorothy-fsm log-search-fsm-test)]
    (is (not (nil? frame)))
    (when frame
      (.dispose ^java.awt.Frame frame))
    ))
 
  
(deftest exit-with-state-fn
  (let [inc-val (fn [val & _] (inc val))
	pong (fn [val] (>= val 3))
	ping-pong (fsm [[:ping  ;; we oscillate between 2 states, pong exits when the number of transitions >= 3
			 _ -> {:action inc-val} pong]
			[pong
			 _ -> {:action inc-val} :ping]])]
    (is (= 3 (ping-pong 0 (range 100))))))
    

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

(deftest fsm-filter-with-default-pass
  (let [a-filter (fsm-filter [[:initial
			       3 -> :suppressing]
			      [:suppressing {:pass false}
			       6 -> :initial]])]
    (is (= [1 2 6 1 2] (filter (a-filter) [1 2 3 4 5 1 2 6 1 2])))))
       

(deftest using-is-terminal
  (let [inc-val (fn [val & _] (inc val))
	the-fsm (fsm [[:start
		       \a -> {:action inc-val} :found-a]
		      [:found-a
		       \b -> {:action inc-val} :found-b
		       \a -> {:action inc-val} :found-a
		       _ -> {:action inc-val} :start]
		      [:found-b {:is-terminal true}
		       _ -> {:action inc-val} :start]]
		     :default-acc 0)]
    (is (= 2 (the-fsm "..ababab")))))


(deftest doc-examples
  ;; make sure all the examples in the documentation actually work
  (let [inc-val (fn [val & _] (inc val))
	count-ab (fsm [[:start
			\a -> :found-a]
		       [:found-a
			\a ->  :found-a
			\b -> {:action inc-val} :start
			_ -> :start]])]
    (is (= [2 0 1] (map (partial count-ab 0) ["abaaabc" "aaacb" "bbbcab"]))))

  
  (let [emit-evt (fn [acc evt] evt)
	log-search (fsm-seq 
		    [[:start
		      #".*event a" -> :found-a]
		     [:found-a
		      #".*event b" -> :found-b
		      #".*event c" -> {:emit emit-evt} :start]
		     [:found-b
		      #".*event c" -> :start]])]
  
    
    (is (= ["5 event c" "5 event c"]
	     (take 2 (log-search (cycle ["1 event a"
					 "2 event b"
					 "3 event c"
					 "another event"
					 "4 event a"
					 "event x"
					 "5 event c"]))))))
			         
    )  


;;===================================================================================================
;; fsm-inc tests

(deftest simple-incremental-fsm
  (let [an-fsm (fsm-inc [[:waiting-for-a
                       \a -> :waiting-for-b]
                      [:waiting-for-b
                       \b -> :done
                       _ -> :waiting-for-a]
                      [:done {:is-terminal true}]])
        fsm-state (atom (an-fsm))]

    (is (= :waiting-for-a (:state @fsm-state)))

    (swap! fsm-state fsm-event \a)
    (is (= :waiting-for-b (:state @fsm-state)))

    (swap! fsm-state fsm-event \b)
    (is (= :done (:state @fsm-state)))))

(defsm-inc inc-log-fsm
  [[:waiting-for-a
    #".*event a" -> :waiting-for-b]
   [:waiting-for-b
    #".*event b" -> :waiting-for-c
    #".*event c" -> {:action test-save-line} :waiting-for-a]
   [:waiting-for-c
    #".*event c" -> :waiting-for-a]])


(deftest simple-fsm-inc-behaviour 
  (are [res acc events] (= res (:value (reduce fsm-event (inc-log-fsm acc) events)))
       ["5 event c"]  []  ["1 event a" "event x" "2 event b" "3 event c" "4 event a" "event x" "5 event c" "6 event a"]
       nil nil ["0 event c" "1 event a" "2 event b" "3 event c"]
       [] [] ["0 event c" "1 event a" "2 event b" "3 event c" ]))
   
(deftest dispatch-inc-with-acc
  (let [an-fsm (fsm-inc
		[[:waiting-for-a
		  [_ #".*event a"] -> :waiting-for-b]
		 [:waiting-for-b
		  [_ #".*event b"] -> :waiting-for-c
		  [_ #".*event c"] -> {:action test-save-line} :waiting-for-a]
		 [:waiting-for-c
		  [_ #".*event c"] -> :waiting-for-a]] :dispatch :event-and-acc)]

  (are [res acc events] (= res (:value (reduce fsm-event (an-fsm acc) events)))
       ["5 event c"]  []  ["1 event a" "event x" "2 event b" "3 event c" "4 event a" "event x" "5 event c" "6 event a"]
       nil nil ["0 event c" "1 event a" "2 event b" "3 event c"]
       [] [] ["0 event c" "1 event a" "2 event b" "3 event c" ])))

(deftest single-inc-dispatch-with-when
  (let [an-fsm (fsm-inc
		[[:initial 
		  (n :guard #(< % 5)) -> :small
		  (n :guard even?) -> :even]  ;; match guards take the last value if multiple match?
		 [:small
		  1 -> :initial]
		 [:even
		  (n :guard odd?) -> :initial]])]

    (is (= [:initial :even :even :even :initial :small :small :small :initial] 
           (map :state (reductions fsm-event (an-fsm) [8 2 4 3 1 2 2 1]))))))

  
(deftest inc-exit-with-state-fn
  (let [inc-val (fn [val & _] (inc val))
	pong (fn [val] (>= val 3))
	ping-pong (fsm-inc [[:ping  ;; we oscillate between 2 states, pong exits when the number of transitions >= 3
			 _ -> {:action inc-val} pong]
			[pong
			 _ -> {:action inc-val} :ping]])]
    (is (= 3
           (:value
            (first 
             (drop-while (complement :is-terminated?) 
                         (reductions fsm-event (ping-pong 0) (range 100)))))))))


(deftest event-acc-vec-dispatch 
  (let [should-transition? (fn [[state event]] (= (* state 2) event))
        event-is-even? (fn [[state event]] (even? event))
        inc-count (fn [cnt & _ ] (inc cnt))
        reset-count (fn [& _]  100)
        even-example (fsm  
	   [[:start
	     [_ :guard should-transition?] -> {:action reset-count} :next-state
	     [_ :guard event-is-even?] -> {:action inc-count} :start]
	    [:next-state ,,,]]
	   :default-acc  0
	   :dispatch :event-acc-vec)]

    (are [events res] (= res (even-example events))
         [1 1 2] 1        ;;  (the number of even events)
         [1 2 2 4] 100)))  ;; 0 (we transitioned to next state)
  
