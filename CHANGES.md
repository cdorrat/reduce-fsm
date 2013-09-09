*Version 0.1.0*

- Added support for incremental finite state machines. These allow you to provide events via a function call instead of a sequence.
  Useful for when events are provided by callbacks

- updated dependencies:
  - clojure 1.5.1
  - core.match 0.2.0-rc3
  - dorothy 0.0.3
    
- Fixed a bug that would cause the state machines to overflow the stack on large input sequences

