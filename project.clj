
(defproject reduce-fsm "0.1.0.baltar-SNAPSHOT"
  :description "Clojure finite state machines"
  :dependencies [[org.clojure/clojure "1.3.0"]
		 [dorothy "0.0.2"]
		 [match "0.2.0-SNAPSHOT"]]
  :dev-dependencies [[swank-clojure "1.3.3"]
		     [server-socket "1.0.0"] ;; used for examples/simple_server.clj
		     [lein-multi "1.1.0-SNAPSHOT"]
		     [org.clojars.weavejester/autodoc "0.9.0"]]
  :multi-deps {"1.2" [[org.clojure/clojure "1.2.1"]
		      [org.clojure/clojure-contrib "1.2.0"]
		      [dorothy "0.0.2"]
		      [match "0.2.0-SNAPSHOT"]]}  
  :autodoc {:web-src-dir "https://github.com/baltar/reduce-fsm"
	    :web-home "https://github.com/baltar/reduce-fsm"
	    :output-path "./autodoc/api"})
