
(def python3?
  (do (import sys) (= 3 (get sys.version_info 0))))

(def python2?
  (not python3?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when python3?
  (import [functools [reduce]])
  (import [python-helpers [apply]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem: 0 and empty collections get booled into false.
(defn identity [x]
  x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn identical? [a b]
  (is a b))

(assert (identical? 1 1))
(assert (not (identical? [1] (list [1]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def nil None)
(defn nil? [x] (none? x))

(defn not= [&rest args]
  (import operator)
  (not (apply operator.eq args)))

(defn true? [x] (identical? x true))
(defn false? [x] (identical? x false))

(assert (true? true))
(assert (false? false))
(assert (not (true? 1)))
(assert (not (false? nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reverse [seq]
  (reversed seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn count [coll]
  (len (list coll)))  ; TODO: Abstract seq

(assert (= 3 (count [1 2 3])))
(assert (= 2 (count (take 2 [1 2 3]))))  ; Realizes generators.
;; FIXME: Do I still need this?
(eval-when-compile (defn count [coll] (len (list coll))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro when-not [test &rest body]
  `(when (not ~test) ~@body))

(assert (when true true))
(assert (not (when-not true true)))
(assert (= None (when false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unable to redefine built-ins like +.
(defn plus [&rest nums]
  (import operator)
  (if (empty? nums)
    0
    (reduce operator.add nums)))

(assert (= 0 (plus)))                 ; 0 args
(assert (= 1 (plus 1)))
(assert (= 3 (plus 1 2)))
(assert (= 6 (reduce plus [1 2 3])))  ; (Fails with original +)

(defn minus [&rest args]
  (import operator)
  (if (= 1 (count args))
    (reduce operator.sub args 0)
    (reduce operator.sub args)))

(assert (= -1 (minus 1)))
(assert (= -1 (minus 1 2)))
(assert (= 0 (reduce minus [3 2 1])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn quot [a b]
  "Floored quotient"
  (// a b))

(assert (= 3 (quot 10 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn not-empty [x]
  (when (not (empty? x))
    x))

(assert (nil? (not-empty [])))
(assert (nil? (not-empty "")))
(assert (= [1 2 3] (not-empty [1 2 3])))
(assert (= "1 2 3" (not-empty "1 2 3")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I can "redefine" Hy's built-in functions with a macro.
(defmacro first [seq]
  `(if (empty? ~seq)
     nil
     (get ~seq 0)))

;; But not as a function.
(defn first- [seq]
  (when (not-empty seq)
    (get seq 0)))

(assert (nil? (first [])))
(assert (= 1 (first [1])))
(assert (= "a" (first "abc")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn println [&rest s]
  (print (.join " " (map str s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn last [seq]
  (when (not-empty seq)
    (get seq -1)))

(assert (= 3 (last [1 2 3])))
(assert (nil? (last [])))

;; Dunno how to handle functions used in macros.
;; FIXME: Do I still need this?
(eval-when-compile (defn last [seq] (get seq -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro doto
  [x &rest forms]
  "Like `->` threading except it passes the first arg
   through all the forms instead of threading the result
   of each form."
  `(let [[xp ~x]]
     ~@(map (fn [form]
              `(~(first form) xp ~@(rest form)))
            forms)
     xp))

(assert (= [1 2 3] (doto [1 2] (.append 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set? [x]
  (isinstance x set))

(defn list? [x]
  (isinstance x list))

(defn dict? [x]
  (isinstance x dict))

(assert (dict? {:a 1}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Private
(defn conj-dict [dict item]
  (doto (.copy dict)
        (.update (cond
                  [(dict? item)       item]
                  [(= 2 (count item)) [item]]))))

;; - can conj with another dict
(assert (= {:a 1} (conj-dict {} {:a 1})))
;; - overwrites keys
(assert (= {:a 2} (conj-dict {:a 1} {:a 2})))
;; - can conj with a key/val pair
(assert (= {:a 1} (conj-dict {} [:a 1])))

;; Private
(defn conj-list [coll item]
  (doto (list coll) (.append item)))

(assert (= [1 2 3] (conj-list [1 2] 3)))

;; Private
(defn conj-set [coll item]
  (doto (.copy coll) (.add item)))

(assert (= (set [1 2 3 4]) (conj-set (set [1 2 3]) 4)))
(assert (= (set [1 2 3 4]) (conj-set (set [1 2 3]) 4)))

(defn conj [coll &rest items]
  (if (empty? items)
    (throw (TypeError "Can't conj nothin."))
    (cond
     [(nil? coll)  items]
     [(list? coll) (reduce conj-list items coll)]
     [(dict? coll) (reduce conj-dict items coll)]
     [(set? coll)  (reduce conj-set items coll)])))

(assert (= (, 1) (conj nil 1)))  ; conj nil

;; conj &rest
(assert (= {:a 3}        (conj {} {:a 1} {:a 2} {:a 3})))
(assert (= [1 2 3]       (conj [] 1 2 3)))
(assert (= (set [1 2 3]) (conj (set []) 1 2 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro dotimes [times &rest body]
  `(foreach [~(first times) (range ~(last times))]
     ~@body))

(setv test [])
(dotimes [n 3]
  (setv test (conj test n)))
(assert (= [0 1 2] test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn partition [n step pad coll]
;;   (setv state coll)
;;   (while true
;;     (let [[chunk (list (take n state))]]
;;       (if (empty? chunk)
;;         (break)
;;         (let [[pad-length (minus n (count chunk))]]
;;           (yield (concat chunk (take pad-length pad)))
;;           (setv state (drop step state)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro when-let [binding &rest body]
  `(let [[lol ~(last binding)]]
     (when lol
       ;; FIXME: See dotimes.
       (assoc (globals) (str '~(first binding)) lol)
       ~@body)))

(assert (when-let [x true] x))
(assert (not (when-let [x false] x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rand-nth [coll]
  (import random)
  (random.choice coll))

(defn rand-int [n]
  "0 <= (rand-int n) < n"
  (import random)
  (random.randint 0 (dec n)))

(defn rand [&rest n]
  ;; FIXME: Unlike Clojure's rand, this is n inclusive.
  (import random)
  (if (empty? n)
    (random.random)
    (random.uniform n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cons [x seq]
  (if (iterable? seq)
    (doto seq (.insert 0 x))
    [x]))

(assert (= [1 2 3] (cons 1 [2 3])))
(assert (= [1] (cons 1 nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn into [init-coll colls]
  (reduce conj colls init-coll))

(assert (= [1 2] (into [] [1 2])))
(assert (= [1 2 3] (into [1] [2 3])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro doseq [binding &rest body]
  `(foreach [~(first binding) ~(last binding)]
     ~@body))

(setv test 0)
(doseq [n (range 3)]
  (setv test (plus test n)))
(assert (= 3 test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn every? [pred coll]
  (cond
   [(empty? coll)       true]
   [(pred (first coll)) (every? pred (rest coll))]
   [true                false]))

(assert (every? true? []))
(assert (every? true? [true true true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn any? [pred coll]
  (cond
   [(empty? coll)       false]
   [(pred (first coll)) true]
   [true                (any? pred (rest coll))]))

(assert (= false (any? true? [])))
(assert (any? true? [false false true]))
(assert (not (any? true? [false false false])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn complement [func]
  (fn [&rest args]
    (not (apply func args))))

(assert ((complement iterable?) 1))
(assert (not ((complement nil?) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn concat [&rest colls]
  (if (any? (complement iterable?) colls)
    (throw (TypeError "Argument must be iterable.")))
  (if (empty? colls)
    []
    (reduce plus colls)))

(assert (try (concat 1) (catch [_ TypeError] true)))
(assert (empty? (concat)))                           ; 0 args
(assert (empty? (concat [])))                        ; arg is empty
(assert (= [1 2 3 4] (concat [1] [2 3] [4])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn partial [func &rest init-args]
  (fn [&rest args]
    (apply func (concat init-args args))))

(assert (= 1 ((partial inc) 0)))
(assert (= 3 ((partial plus 2) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mapcat [f colls]
  (apply concat (list (map f colls))))

(assert (= [1 0 1 0 1 0] (mapcat (fn [x] [x 0]) [1 1 1])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro if-not [test dotrue dofalse]
  `(if (not ~test)
     ~dotrue
     ~dofalse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Original distinct? fails on non-iterable args.
(defn distinct?- [x]
  (if-not (iterable? x)
    true
    (= (count x) (count (distinct x)))))

(assert (distinct?- nil))           ; arg is nil
(assert (distinct?- 1))             ; arg is not collection
(assert (distinct?- [1 2 3]))       ; arg is collection
(assert (not (distinct?- [1 1 1])))

;; IO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn slurp [path]
  (with [f (open path "r")]
    (.read f)))

;; TODO: add options like append
(defn spit [path contents]
  (with [f (open path "w")]
    (.write f contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn comp [&rest funcs]
  (if (empty? funcs)
    identity
    (fn [&rest args]
      (let [[reordered-funcs (list (reverse funcs))]
            [first-apply (apply (first reordered-funcs) args)]]
        (if (empty? (rest reordered-funcs))
          first-apply
          (reduce (fn [result func] (func result))
                  (rest reordered-funcs)
                  first-apply))))))

(assert (= "3" ((comp str inc inc) 1)))
(assert (= 1 ((comp identity identity identity) 1)))
(assert (= 1 ((comp) 1)))

(let [[add (fn [a b] (plus a b))]]
  (assert (= 3 ((comp identity add) 1 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn children? [node]
  (iterable? node))

(defn children [node]
  (identity node))

(defn walk [node]
  (cons node (when (children? node)
               (mapcat walk (children node)))))

(defn flatten [seq]
  (filter (complement iterable?) (walk seq)))

(assert (= [1 2 3 4 5] (list (flatten [[1] 2 [[3 4]] 5]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn contains? [set member]
  (in member set))

(assert (contains? (set [1 2 3]) 3))
(assert (not (contains? (set [1 2 3]) 4)))

(defn disj [set member]
  (doto (.copy set) (.discard member)))

;; When member exists
(assert (= (set [1 2 3]) (disj (set [1 2 3 4]) 4)))
;; When member doesn't exist
(assert (= (set [1 2 3]) (disj (set [1 2 3]) 99)))

(defn subset? [set other]
  (.issubset set other))

(defn superset? [set other]
  (.issuperset set other))

(defn difference [&rest sets]
  (reduce (fn [a b] (.difference a b)) sets (set [])))

(defn intersection [&rest sets]
  (reduce (fn [a b] (.intersection a b)) sets (set [])))

(defn union [&rest sets]
  (reduce (fn [a b] (.union a b)) sets (set [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maps (Dicts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn zipmap [keys vals]
  (dict (zip keys vals)))

;; Stops at shortest collection
(assert (= {:a 1 :b 2 :c 3} (zipmap [:a :b :c] [1 2 3 4])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Abstract seq
(defn seq [x]
  (cond
   [(list? x)       (list x)]
   [(iterator? x)   (list x)]
   [(dict? x) (iter (.items x))]))

(assert (= [(, :a 1)] (list (seq {:a 1}))))

(defn merge [&rest maps]
  (reduce conj maps {}))

;; Hy's `assoc` mutates the map.
(defn assoc- [m key val]
  (conj m [key val]))

(assert (= {:a 1} (assoc- {} :a 1)))
(assert (= {:a 1 :b 2} (assoc- {:a 1} :b 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hy's `get` throws KeyError when key isn't member
(defn get- [m key]
  (try
    (get m key)
    (catch [_ KeyError])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn merge-with [func &rest maps]
  (let [[pairs (list (mapcat (comp list seq) maps))]]
    (reduce (fn [accum pair]
              (let [[key (first pair)]
                    [val (second pair)]
                    [accum-val (get- accum key)]]
                (if accum-val
                  (assoc- accum key (func accum-val val))
                  (assoc- accum key val))))
            pairs
            {})))

(assert (= {:a 3}
           (merge-with (fn [a b] (plus a b)) {:a 1} {:a 2})))

(assert (= {:a [1 2]}
           (merge-with concat {:a [1]} {:a [2]})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn group-by [func coll]
  (reduce (fn [accum item]
            (let [[key (func item)]]
              (merge-with concat accum {key [item]})))
          coll
          {}))

(assert (= {true [2 4] false [1 3]}
           (group-by even? [1 2 3 4])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn frequencies [coll]
  (reduce (fn [accum pair]
            (assoc- accum
                    (first pair)
                    (count (second pair))))
          (list (seq (group-by identity coll)))
          {}))

(assert (= {:a 4 :b 1 :c 2 :d 3}
           (frequencies [:a :b :a :a :c :d :d :a :d :c])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn select-keys [m keys]
  (reduce (fn [accum key]
            (let [[val (get- m key)]]
              (if val
                (conj accum {key val})
                accum)))
          (set keys)
          {}))

(assert (= {:a 1}
           (select-keys {:a 1} [:a :a :a])))
(assert (= {:a 1}
           (select-keys {:a 1} [:a :b :c])))
(assert (= {:a 1 :b 2 :c 3}
           (select-keys {:a 1 :b 2 :c 3} [:a :b :c])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn map-indexed [func coll]
  (foreach [idx (range (count coll))]
    (yield (func idx (nth coll idx)))))

(assert (= [[0 :a] [1 :b] [2 :c]]
           (seq (map-indexed (fn [idx val] [idx val])
                             [:a :b :c]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
