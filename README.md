# dpll

A naive implementation of the DPLL algorithm to solve
SAT-problems. You can find detailed documentation in `Artificial
Intelligence - A modern approach` by Stuart Russel and Peter Norvig,
section 7.6.

## Usage

```clojure
(require '[dpll.core :refer [dpll-satisfiable?]])

(dpll-satisfiable? [[[:not :B] [:not :C] [:not :A]]  ;; CNF
                   [:B [:not :C] :A]
                   [:B [:not :C] :A]])
```

## License

Copyright © 2016 Christian Weilbach

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
