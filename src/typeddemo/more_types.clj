(ns typeddemo.more-types
  (:require [clojure.core.typed :as t]))

;; This namespace contains a few examples for some special types
;; Like U (union), I (intersection), Difference and Rec (recursive type)
;; as well as some basic types.

;; This namespace is intended to be checked at the REPL via t/cf.


;; Composition of types
;; --------------------------------------------------------------------------------

;; The Types to compose other types are : I, U, Difference, Fn, Rec (not considering the
;; collection types)

;; NOTE: for t/U with nil, you can also use t/Option

;; Examples:


;; TypeA is either string, number or nil
(t/cf (t/defalias TypeA (t/U t/Str t/Num nil)))

;; these vars will type check
(t/cf (t/def ex1 :- TypeA nil))
(t/cf (t/def ex2 :- TypeA "qux"))
(t/cf (t/def ex3 :- TypeA 12))

;; not compatible with TypeA
(t/cf (t/def bad :- TypeA :bad))

;; This type intersects with TypeA and a sequence of Any.
;; The result is only the string.
(t/cf (t/defalias TypeB (t/I TypeA (t/Seqable t/Any))))

;; fails now
(t/cf (t/def ex4 :- TypeB nil))

;;yep, still compatible with string.
(t/cf (t/def ex5 :- TypeB "qux"))

;; fails now.
(t/cf (t/def ex6 :- TypeB 12))


;; This is why we can count the characters here, only the string is left.
(t/cf (t/defn composed  [i :- TypeB] :- t/Num
        (count i)))

;; If you remove nil and str from TypeA, then only Num is left
(t/cf (t/defalias TypeC (t/Difference TypeA nil t/Str)))

;; when only num is left then we can do some arithmetic safely.
(t/cf (t/defn composed2 [i :- TypeC] :- t/Num (+ i 2)))

;; string, nil, not longer part of TypeC
(t/cf (t/def ex7 :- TypeC nil))
(t/cf (t/def ex8 :- TypeC "qux"))

;; this one is ok.
(t/cf (t/def ex9 :- TypeC 12))



;; An Example for a recursive type (XML).

;; in clojure xml attributes are just map of kw/str
(t/cf (t/defalias attr-type (t/Map t/Kw t/Str)))

;; then the xml type is a recursive type. The variable y allows to define
;; where the recursion occurs.
(t/cf (t/defalias xml-type (t/Rec [y]
                                  '{:tag t/Kw
                                    :attrs attr-type
                                    :children (t/Vec y)})))

;; this one is a valid xml
(t/cf (t/def xml-root :- xml-type {:tag :process
                                   :attrs {:valid "abc"}
                                   :children [{:tag :exchange
                                               :attrs {:mean-value "1.2"}
                                               :children []}]}))


;; this fn appends an xml-node to another node.
(t/cf(t/defn add-node [node :- xml-type child :- xml-type] :- xml-type
       (t/let [children (get node :children )]
         (assoc node :children (conj children child)))))

;; Try to make :children optional: e.g. :children (t/Option (t/Vec y))
;;    Then you'll need to check for nil and use vector instead of conj.
(t/cf  (t/defn add-node [node :- xml-type child :- xml-type] :- xml-type
         (t/let [children (get node :children )]
           (if children
             (assoc node :children (conj children child))
             (assoc node :children (vector child))))))



;; Boolean Types

;; A custom user type.
(t/cf (t/defalias BoolType (t/U true false)))

;; A function signature that maps any to bool
(t/cf (t/defalias any-to-bool [t/Any -> BoolType]))

;; function f1 can receive any input and returns bool
(t/cf (t/ann f1 any-to-bool))

(t/cf  (defn f1 [x]
         (if x true false))) 

;; This fn will fail type checking since it returns also nil.
(t/cf  (t/ann f1-bad any-to-bool))
(t/cf (defn f1-bad [x]
        (when x true)))

;; these vars are also checked
(t/cf (def r1 (f1 :x)))
(t/cf (def r2 (f1 nil)))

;; booltype?
(t/cf r1) 

;; let's reuse BoolType

(t/cf (t/defalias bool-to-string [BoolType -> t/Str]))

(t/cf (t/ann f2 bool-to-string))
(t/cf (defn f2 [x]
        (str x))) 

;; this will then not type check because :qux is not BoolType
(t/cf (def r3 (f2 :qux)))

;; This fails to typecheck, since you can't call f2 on Any.
;; That's interesting: Your code is checked even if it's not annotated.
(t/cf (defn this-fn-is-not-annotated [x]
         (f2 x)))

;;This fn will generally fail and should also fail type checking.
;; E.g calling it with seq of strings will not work.
(t/cf (defn transformer [coll]
         (reduce + 0 coll)))

;; Our function can work with a sequence as input and should return a num
(t/cf (t/defalias Accumulator [(t/ASeq t/Num) -> t/Num]))
(t/cf (t/ann transformer Accumulator))

;; now this succeeds type checking.
(t/cf (defn transformer [coll]
       (reduce + 0 coll)))

;; Next example is transforming each value in a map from number to string.

;; signature of a function from num to str.
(t/cf (t/defalias NumToStrFn [t/Num -> t/Str]))

;; a map with keywords and numbers
(t/cf (t/defalias MapOfNums (t/Map t/Kw t/Num)))

;; a map with keywords and string
(t/cf (t/defalias MapOfStr (t/Map t/Kw t/Str)))

;; a mapper is a function that given a map of numbers and a mapping function,
;; applies to each value. the result is a map of str.
(t/cf (t/defalias Mapper (t/IFn [MapOfNums NumToStrFn -> MapOfStr])))

;; when we iterate over a map this Type captures the map entries.
;; pretty much like in java land.
(t/cf (t/defalias MapEntry (clojure.lang.IMapEntry t/Kw t/Num)))
(t/cf (t/ann mapper Mapper))

;; put it all together. This fn passses type checking.
(t/cf (defn mapper [coll, f]
       (reduce
        (t/fn [acc :- MapOfStr
               el :- MapEntry] :- MapOfStr
               (t/let [k (key el)
                       v (val el)]
                 (assoc acc k (f v))))
        {}
        coll)))


;; next example is a function with varargs (rest parameters)
;; the first arg can only be either :sum or :mul
(t/cf (t/ann f-varargs (t/IFn [(t/Val :sum) t/Num * -> t/Num]
                             [(t/Val :mul) Double * -> Double])))

(t/cf (defn f-varargs [op & args]
       (condp = op
         :sum (apply + args)
         :mul (apply * args))))

;; no matching clause problem, this will not type check, but it won't run either.
(t/cf (def no-matching-clause (f-varargs :qux 1)))

(t/cf (def sum-num (f-varargs :sum 1 2 3)))
(t/cf (def mul-double (f-varargs :mul 1.2 1.4 1.5)))

;; ups, has to be called with Double
(t/cf (def mul-double (f-varargs :mul 1)))
(t/cf (def mul-double (f-varargs :mul 1.0)))


;; Example with mutiple clojure arities

;; we define the sig. of an fn with 2 arities
(t/cf (t/ann f-arities (t/IFn [t/Num -> t/Num]
                             [t/Num t/Map -> (t/Option t/Num)])))

(t/cf(defn f-arities
       ([x] (inc x))
       ([x m] (when (:secret m) (inc x)))))


;; Example with keyword params

(t/cf(t/ann kw-params (t/IFn [t/Num &
                              :mandatory {:m t/Num}
                              :optional {:o t/Num} -> t/Num])))

(t/cf(defn kw-params [n & {m :m o :o :or {o 10}}]
       (+ n m o)))


;; Example polymorphic Fn.

;; for each type X we return the a vector of X.
(t/cf(t/ann poly-fn
            (t/All [x] (t/IFn [x -> (t/Vec x)]))))

(t/cf(defn poly-fn [x]
       [x]))

;; Polymorphic Example 2.

;; for each type X we return the a vector of X.
(t/cf(t/ann poly-vec-fn
            (t/All [x]
                   (t/IFn [x -> (t/Vec x)]))))

(t/cf(defn poly-vec-fn [x]
       [x]))

;; Polymorphic example 3.

;; let's infer something. Try to check the type of foobar via cf
;;: (t/All [x] [x -> (t/AVec x)])
(t/cf(t/defn :forall [x]
       foobar [arg :- x]
       ((t/inst vector x) arg)))


;; Polymorphic example 4.

;; we want a function that for each type X it accepts
;; a sequence of X's and returns a map of Nums/X's
;; e.g. [:a :b :c] -> {0 :a 1 :b :2 :c}

;; The following fn passes type checking
(t/cf(t/defn :forall [x] poly-map-fn [& args :- x *]  :- (t/Map t/Num x)
       (into {}
             (map-indexed (t/fn [x :- t/Num y :- x] (vector x y))
                          args))))

;; Granted, the fn poly-map-fn looks much more complicated than the regular
;; clojure code, but type checking also does impose more syntax of course.

;; pure clojure version is cleaner but less safe.
(t/cf  (defn poly-map-fn [& args]
         (into {}
               (map-indexed vector args))))

;; Also in this example we cannot use the t/ann form AFAIK because we need to
;; have access to the polymorphic parameter to instantiate the inner fn's.

;; this example requires some short explanation:

;;This is the signature that we want, but we cannot use it. Reasons explained below
(t/cf (t/ann poly-map-fn (t/All [x] (t/IFn [x * -> (t/Map t/Num x)]))))


;; Polymorphic example 5

;; We can have constrained polymorphic types

(t/cf (t/ann constrained-poly
            (t/All [[x :< t/Num]]
                   [x -> t/Str])))


(t/cf (defn constrained-poly [x] (str x)))

;; ok.
(t/cf (def constraint-int (t/inst constrained-poly Integer)))

;; bad, can only be instantiated for nums
(t/cf (def illegal (t/inst constrained-poly t/Kw)))


;; Example List mapper: transform list of any to list of strings."

(t/cf (t/defalias ListMapper (t/IFn [(t/List t/Any) -> (t/List String)])))

;; Note: we can reuse the signature here, not possible if we use t/defn
(t/cf (t/ann my-mapper1 ListMapper))
(t/cf (t/ann my-mapper2 ListMapper))
(t/cf (t/ann my-mapper3 ListMapper))
(t/cf (t/ann my-mapper4 ListMapper))

(t/cf (defn my-mapper1 [coll]
       (apply list (map str coll))))

(t/cf (defn my-mapper2 [coll]
       (apply list (map #(str "_" % "_") coll))))

;; the bad guy, but core.typed is happy.
(t/cf (defn my-mapper3 [coll]
       (apply list (map #(-> % str count) coll))))
