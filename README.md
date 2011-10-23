# reduce-fsm

## Features
reduce-fsm provides a simple way to specify clojure [finite state machines](http://en.wikipedia.org/wiki/Finite-state_machine), it allows you to:

- Define define state machines that accumulate values (in the same was that reduce does)
- Create lazy sequences from state machines
- Perform stateful filtering with clojures filter/remove functions
- Visualize the resulting state machines with graphviz

All generated state machines are plain clojure functions and read events from clojure sequences.
Events are dispatched with core.match and allow the use of all match features (guards, destructuring, regex matching, etc.)


## Usage
The fastest way to use this library is with Leiningen or Cake. Add the following to your project.clj dependencies:

```clojure
[cdorrat/reduce-fsm "0.1.0-SNAPSHOT"]
```

Use via:

```clojure
(require '[reduce-fsm :as fsm])
```

## Examples

#### Basic FSM 
The following example counts the number of times "ab" occurs in a sequence. 

```clojure
(defn inc-val [val & _] (inc val))

(fsm/defsm count-ab
  [[:start
    \a -> :found-a]
   [:found-a
    \a ->  :found-a
    \b -> {:action inc-val} :start
    _ -> :start]])

;; We can use the generated fsm like any function
(map (partial count-ab 0) ["abaaabc" "aaacb" "bbbcab"])
;; returns => (2 0 1)

(show-fsm count-ab)
;; displays the fsm diagram below

```

![show-fsm output](https://github.com/cdorrat/reduce-fsm/raw/master/images/fsm-count-ab.png)

#### Generating Lazy Sequences

The fsm-seq functions return lazy sequences of values created by the emit function when a state change occurs. 
This example looks for log lines where the sequence of events was (a,c) instead of the expected (a,b,c) and
adds the unexpected event to the output sequence. 


```clojure
(defn emit-evt [val evt] evt)

(defsm-seq log-search
  [[:start
    #".*event a" -> :found-a]
   [:found-a
    #".*event b" -> :found-b
    #".*event c" -> {:emit emit-evt} :start]
   [:found-b
    #".*event c" -> :start]])

;; The resulting function accepts a sequence of events 
;; and returns a lazy sequence of emitted values
(take 2 (log-search (cycle ["1 event a"
                            "2 event b"
                            "3 event c"
                            "another event"
                            "4 event a"
                            "event x"
                            "5 event c"])))

;; returns => ("5 event c" "5 event c")

(show-fsm log-search)
;; displays the image below

```

![show-fsm output](https://github.com/cdorrat/reduce-fsm/raw/master/images/fsm-log-search.png)

#### Stateful Filtering

The following example suppresses values from the time a 3 is encountered until we see a 6.


## Documentation


## License

Copyright (C) 2011 Cameron Dorrat

Distributed under the Eclipse Public License, the same as Clojure.
