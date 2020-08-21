(defproject reduce-fsm "0.1.4"
  :description "Clojure finite state machines"
  :dependencies [[org.clojure/clojure "1.7.0"]
		 [dorothy "0.0.6"]
                 [org.clojure/core.match "1.0.0"]]
  :dev-dependencies [[server-socket "1.0.0"] ;; used for examples/simple_server.clj		     
		     [org.clojars.weavejester/autodoc "0.9.0"]]
  :autodoc {:web-src-dir "https://github.com/cdorrat/reduce-fsm/"
	    :web-home "http://cdorrat.github.com/reduce-fsm/"
	    :output-path "./autodoc/api"})
