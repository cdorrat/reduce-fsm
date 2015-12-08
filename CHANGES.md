*Version 0.1.4*
  - Fix Issue #9 - invalid excpetion call breaks Clojure 1.8 compatability.
  - Bumped dependencies for clojure -> 1.7.0 & core.match -> 0.2.2

*Version 0.1.3*
  - support for specifying the initial state of the fsm at runtime
  
*Version 0.1.0*

- Added support for incremental finite state machines. These allow you to provide events via a function call instead of a sequence.
  Useful for when events are provided by callbacks

- updated dependencies:
  - clojure 1.5.1
  - core.match 0.2.0-rc3
  - dorothy 0.0.3
    
- Fixed a bug that would cause the state machines to overflow the stack on large input sequences

