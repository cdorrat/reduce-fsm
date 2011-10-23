(ns fsm-dispatch-examples
  "Examples illustrating the supported event dispatch styles"
  (:require [reduce-fsm :as fsm]))



;; ============================================================
;; match-1 (default) event dispatch
;; Events are dispatched using the core.match/match-1 syntax.
;; See https://github.com/clojure/core.match for details

(defn found-lisp [num-instances & _]
  (println "Found lisp")
  (inc num-instances))

;; count the number of instances of 'lisp' in a string
(fsm/defsm find-lisp
  [[:start
    \l -> :found-l]
   [:found-l
    \i -> :found-i
    \l -> :found-l
     _ -> :start]
   [:found-i
    \s -> :found-s
    \l -> :found-l
     _ -> :start]
   [:found-s
    \p -> {:action found-lisp} :start
    \l -> :found-l
    _ -> :start]]
  :default-acc 0)


(find-lisp "ablilispghlihilisp")
;; => Found lisp
;;    Found lisp
;;    2
;;


;; ============================================================
;; match dispatch
;; Events are dispatched using the core.match/match syntax
;; with a vector of [acc event] where acc is the current
;; accumulator value.
;; See https://github.com/clojure/core.match for details


;; we'll create an fsm to match the equvalent of the "ab{2,3}c" regex
(defn inc-b-count [acc & _]
  (update-in acc [:repeats] inc))

(defn reset-b-count [acc & _]
  (assoc acc :repeats 0))

(defn count-satisfied? [{:keys [repeats]}]
  (and (>= repeats 2)
       (<= repeats 3)))

(defn matched-event [acc & _]
  (assoc acc :matched true))

(defn done-state [& _]
  true)

(fsm/defsm sample-regex
  [[:start
    [_ \a] -> :found-a]
   [:found-a
    [_ \a] -> :found-a
    [_ \b] -> {:action inc-b-count} :found-b
    [_ _]  -> :start]   
   [:found-b
    [(n :when count-satisfied?) \c] -> {:action matched-event} done-state
    [_ \b] -> {:action inc-b-count} :found-b
    [_ _]  -> {:action reset-b-count} :start]
   [done-state]]
  :default-acc {:matched false :repeats 0}
  :dispatch :event-and-acc)

;; test a series of strings
(map #(-> % sample-regex  :matched)
     ["abc" "abbbbbc" "abbbc"])
;;=>  (false false true)

