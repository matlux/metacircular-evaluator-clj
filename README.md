# clj-eval

A Clojure implementation of the Meta-circular Evaluator. It is following as closely as possible the suggested implementation of SICP chapter 4.

Some aspect have been addapted to Clojure, like the use of map instead of the list of pair in Lisp. The implementation of the definitions do not mutate the environment. Instead a new environment is return which I believe is functionally purer.

[![Build Status](https://travis-ci.org/matlux/metacircular-evaluator-clj.svg?branch=master)](https://travis-ci.org/matlux/metacircular-evaluator-clj)

## Usage


Run the repl:
```
lein run
```

or run the tests:
```
lein test
```

## License

Copyright © 2015 Matlux Ltd

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
# metacircular-evaluator-clj
