# typeddemo

A step-by-step walkthrough of core.typed covering the essential concepts.

*Disclaimer*: I set up these examples in preparation for a workshop and I don't claim
for correctness. If you find a misinterpretation please send a PR so I can fix it.
The goal of this workshop was to experience the basics of core.typed, learn a bit how to
read error messages and finally to get some ideas how you could use core.typed in a practical way. 

## Usage

The following namespaces are intended for REPL usage. 


* repl.clj A step by-step REPL session through core.typed.
* errors.clj. A walkthrough in reading error messages with examples.

The following files are inteded to be checked with `check-ns`

* functions.clj. Examples how to define function signatures.
* sequences.clj. Examples for how we can use the `core.typed` sequences
* maps.clj. Probably the most useful for real life use.
* enums.clj. Some ideas how we could define enums in core.typed
* more_typed.clj. various other ideas from things you can do with `core.typed`.


http://nathanic.org/posts/2013/typed-clojure-tour/

https://frenchy64.github.io/typed/clojure,/core.typed,/clojure/2013/09/03/polymorphic-hof.html
## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
