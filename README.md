
# Hyclops

Clojurey functions for [Hy](https://github.com/hylang/hy) (a lisp on top of Python).

---

I didn't mean to write this.

It just all started with

``` clojure
(defn reverse [x]
  (reversed x))
```

These functions are implemented naively, but I would like to arrive at a lazy-seq abstraction that hides Python's procedural iterators/generators.

---

Only compat with Python 3 for the moment. (I have very little Python experience)
