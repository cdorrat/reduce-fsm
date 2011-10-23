(ns simple-server
  "An example fsm used for a simple TCP service to that lets users list the contents of directories after authenticating"  
  (:require [reduce-fsm :as fsm]
	    [clojure [string :as str]]
	    [clojure.contrib
	     [duck-streams :as ds]
	     [server-socket :as ss]]
	    )
  (:import java.io.File)
  )

(defn parse-command
  "Parse a single line into a command for our server"
  [line]
  (println (str "read line\"" line "\"")) 
  (let [[cmd & args] (str/split line #"\s+")]
    {:cmd cmd
     :args args}))

(defn password-valid?
  "return true if the user/password identifies a valid user"
  [[user password]]
  (and (= "user" user)
       (= "password" password)))

;; ================================= actions  ================================= 
;; These actions will be called in response to commands
;; with [accumulated-state event from-state to-state]
;; they should return the new accumulated state after the transition 

(defn chdir
  "set the current directory from a command"
  [acc {[new-dir] :args} & _]
  (if (.. (File. new-dir) isDirectory)
    (assoc acc :curr-dir new-dir)
    acc))

(defn list-dir
  "list the contents of the current directory.
   may be tested with: (list-dir {:writer println :curr-dir \"/tmp\"})"
  [{:keys [writer curr-dir] :as acc} & _]
  (writer (format "Contents of %s is:" curr-dir))
  (doseq [f (.. (File. curr-dir) list)]
    (writer (str "\t" f)))
  acc)

(defn invalid-command [{:keys [writer] :as acc} & _]  
  (writer "unrecognised command")
  acc)

;; a state function - returning true will cause the fsm to exit
(defn quit [& _]
  true)

;; ================================= actions  ================================= 
;; define our fsm
;; list-session will be a function with the following arities:
;;   [events]     - a sequence of events
;;   [acc events] - the initial state and a sequence of events
;; It will return when one of the following occurs:
;;   a). it reaches a terminal state
;;   b). there are no more events
;;   c). a state function returns true

(fsm/defsm list-session [[:connected
			  {:cmd "login" :args (a :when password-valid?)} -> :authorised
			  {:cmd _} -> {:action invalid-command} :connected]
			 [:authorised
			  {:cmd "chdir"} -> {:action chdir} :authorised
			  {:cmd "ls"} -> {:action list-dir} :authorised
			  {:cmd "quit"} -> quit
			  {:cmd _} -> {:action invalid-command} :authorised]
			 [quit]])

(comment
  ;; we can show our fsm as a diagram with
  (fsm/show-fsm list-session)
  
  ;; the fsm is just an ordinary function
  ;; we can test our server at the repl like so
  (list-session {:writer println :curr-dir "."}
		(map parse-command
		     ["login user password"
		      "chdir /tmp"
		      "ls"
		      "quit"]))
  )

;; ================================= actions  ================================= 
;; create the tcp server

(defn handle-connection
  "Handle a single client connection to our service"
  [input-stream output-stream]
  (println [input-stream output-stream])
  (with-open [w (ds/writer output-stream)]
    (list-session {:writer #(doto w (.println %) .flush)  :curr-dir "."}
		  (map parse-command (ds/read-lines input-stream)))))
			     
(defn start-server
  "Start a server listening on a specified port"
  [port]
  (println (str "directory listing server listening on port " port))
  (ss/create-server port handle-connection))

;; that's it, run (start-server 5000) from the repl
;; telnet to your port (5000) to test the server
;; there's no mutable state in the whole implementation
