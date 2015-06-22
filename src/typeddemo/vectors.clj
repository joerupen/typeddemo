(ns typeddemo.vectors
  (:require [clojure.core.typed :as t]))

;; core.typed vectors walkthrough 
;; --------------------------------------------------------------------------------
;; The difficulty of core.typed vectors is that there are a couple of them
;; and they are not necessarily compatible to one another. Here are the vector types:

;; * Vec, e.g (t/Vec t/Num), any IPersitentVector implementation
;; * AVec, e.g. (t/AVec t/Num), the specific clojure PersitentVector (i.e. the one returned by vector fn's)
;; * HVec, e.g. (t/HVec [t/Num *]), a heterogenous vector, i.e. a vector with specific information about each entry.
;; * NonEmptyVec, e.g. (t/NonEmptyVec t/Num). A vector with min. 1 element
;; * NonEmptyAVec, e.g. (t/NonEmptyAVec t/Num). same here, but for clojure PersistentVector


;; I am not sure why AVec exists, normally you will just work with Vec and HVec, but'
;; it's good to know how we convert between these types.

;; Vec - IPersistentVector
;; --------------------------------------------------------------------------------

;; can [1] be passed where vec of num is expected? yep, [1] :< Vec
(t/cf (t/ann-form [1] (t/Vec t/Num)))

;; can an HVec passed where Vec is expected, yep., HVec :< Vec
(t/cf (t/ann-form (t/ann-form [1] (t/HVec [t/Num])) (t/Vec t/Num)))

;; can an AVec passed where Vec is expected, yep., AVec :< Vec
;; NOTE: the reason we instantiate vector here is given later.
(t/cf (t/ann-form ((t/inst vector t/Num) 1) (t/Vec t/Num)))

;; can we pass a list where we expect vector? no
(t/cf (t/ann-form '(1) (t/Vec t/Num)))

;; or a seq? no
(t/cf (t/ann-form (range 10) (t/Vec t/Any)))

;; We can pass HVec to assoc and get back something that's compatible with Vec.
(t/cf assoc)
(t/cf (t/ann-form (assoc [ 1 2 3] 0 1)
                  (t/Vec t/Any)))

;; similar for pop
(t/cf (t/ann-form (pop [ 1 2 3] )
                  (t/Vec t/Any)))

;; Vec is pretty fundamental. When your function accepts a Vec, you can pass in
;; every of the other vector types. (t/U HVec AVec NonEmptyVec NonEmptyAVec) :< Vec

;; you can't however pass Vec directly to a function expecting any of the other types. 

;; HVecs
;; --------------------------------------------------------------------------------

;; HVecs are vectors of fixed or variable length with specific info about each element.

;; few examples of HVecs.
(t/cf (t/ann-form [1] (t/HVec [t/Num])))
(t/cf (t/ann-form [1 1] (t/HVec [t/Num t/Num])))
(t/cf (t/ann-form [:k 1 1] (t/HVec [t/Kw t/Num t/Num])))
(t/cf (t/ann-form [1 2 3] (t/HVec [t/Num *])))

;; What if you have a vector (Vec, AVec) and want to call a function
;; that has an HVec in the signature?

;; sorry, you can't pass a vector of num to a vector of exactly one num.
(t/cf (t/ann-form (t/ann-form [1] (t/Vec t/Num))
                  (t/HVec [t/Num])))

;; sorry, you can't pass a vector of num to a vector of zero or many num.
;; Logically this should work, since the items in the vector will fit well in the target
;; but core.typed prevents this 'upcast'
(t/cf (t/ann-form (t/ann-form [1] (t/Vec t/Num))
                  (t/HVec [t/Num *])))

;; we need to provide a custom function for that to work.
(t/cf (t/defn vec-to-hvec-var-length [v :- (t/Vec t/Num)] :- (t/HVec [t/Num *])
        (reduce (t/fn [acc :- (t/HVec [t/Num *])  el :- t/Num] :- (t/HVec [t/Num *])
                  (conj acc el))
                (t/ann-form [] (t/HVec [t/Num *]))
                v)))

;; your hvec could also contain other types before the t/Num *, how would we solve that
(t/cf (t/defn vec-to-hvec-var-length-kw
        [v :- (t/Vec t/Num), k :- t/Kw] :- (t/HVec [t/Kw t/Num *])
        (reduce (t/fn [acc :- (t/HVec [t/Kw t/Num *])
                       el :- t/Num]
                  :- (t/HVec [t/Kw t/Num *])
                  (conj acc el))
                (t/ann-form [k] (t/HVec [t/Kw t/Num *]))
                v)))

;; convert Vec to HVec of length 2.
;; try to remove the assertions, or to make the result longer/shorterl.
(t/cf (t/defn vec-to-hvec-fixed-length [[f s] :- (t/Vec t/Num), ] :- (t/HVec [t/Num t/Num])
        (assert (number? s))
        (assert (number? f))
        [f s]))

;; when vec is non-empty we can skip one check.
(t/cf (t/defn vec-to-hvec-fixed-length-non-empty
          [[f s] :- (t/NonEmptyVec t/Num), ] :- (t/HVec [t/Num t/Num])
        (assert (number? s))
        [f s]))


;; AVec
;; --------------------------------------------------------------------------------


;; AVec is just the Type of the persistent vector returned by (vector). It's unclear
;; why there is a special case for that. It's also not a Type you will see very often,
;; since (vector) typically returns an HVec and not an AVec. However, you can force it
;; to give you an AVec, which we use for these examples. let's see how:

;; first check the signature of vector. 
(t/cf vector)

;; Hmm, the signature can match both AVec and HVec
;; [b ... b] -> HVec can match if we have both zero or more arguments. In this case the type of HVec is inferred from  the b's
;; [r * ] -> AVec can match if we have zero or more arguments. The polymorphic variable r is bound to the type of argument that is given and that determines the type of AVec (AVec r)
;; The signature is really nice, reads: If we know the types of all values passed to vector, then we return an HVec, otherwise we expect the arguments to be of the same type r and return an AVec of it.

;; No matter what we do in our examples with (vector ...), we implicitly let core.typed know about our args and so it will always choose HVec. To remediate this situation we have to instantiate vector explicity.

;; Try the following expressions:

(t/cf (t/inst vector t/Num))
;; => (t/IFn [-> (t/HVec [])] [t/Num * -> (t/AVec t/Num)])
;; => if we pass no args in this version we get HVec, otherwise we get AVec

(t/cf (t/inst vector t/Num t/Num))
;; => (t/IFn [t/Num -> (t/HVec [t/Num])] [t/Num * -> (t/AVec t/Num)])
;; If we pass one arg in this version we get HVec, we pass 2 or more we get AVec

(t/cf (t/inst vector t/Num t/Num t/Num))
;; (t/IFn [t/Num t/Num -> (t/HVec [t/Num t/Num])] [t/Num * -> (t/AVec t/Num)])
;; if we pass 2 args we get HVec, more than 2 we get AVec

(t/cf ((t/inst vector t/Num t/Num t/Num) 1 2)) ;; => HVec
(t/cf ((t/inst vector t/Num t/Num t/Num) 1 2 3)) ;; => AVec

;; Great, we know how to create AVec's.

;; with this result we can create an fn that returns an ASeq from a sequence
(t/cf (t/defn aseq [x :- (t/Seq t/Num)] :- (t/ASeq t/Num)
        (apply (t/inst vector t/Num) x)))

;; can we pass AVec to Vec, sure
(t/cf (t/ann-form (aseq [1]) (t/Vec t/Num)))

;; can we pass AVec to HVec, sure, no. Content-wise yes, but semantically no: AVec !:< HVec.
;; we'd need a function like above (vec-to-hvec-var-length)
(t/cf (t/ann-form (aseq [1]) (t/HVec [t/Num *])))


;; NonEmptyVec

;; If we know vec is not empty, then the following form has type Number.
(t/cf (first (t/ann-form [1] (t/NonEmptyVec t/Num)) ))


;; Vectors in practice
;; --------------------------------------------------------------------------------

;; Example 1: 3d vector. A 3d vector is both a vector of nums and has excact 3 elements.
(t/cf (t/defalias vec-3d (t/I (t/Vec t/Num) (t/ExactCount 3))))

;; yep validaes
(t/cf (t/def my-point :- vec-3d [1 2 3]))

(t/cf (t/def bad-point :- vec-3d [1 2 3 4]))

(t/cf (t/defn sum-3d [[a1 a2 a3] :- vec-3d
                      [b1 b2 b3] :- vec-3d] :- vec-3d
                      [(+ a1 b1) (+ a2 b2) (+ a3 b3) ]))

;; Given a sequence of numbers, how do we create a vector?
;; correct code should ensure that all 3 numbers are not nil.
(t/cf
 (t/defn to-vec [coll :- (t/ASeq Number)] :- vec-3d
   (let [[x y z  ] coll]
     (assert (number? x))
     (assert (number? y))
     (assert (number? z))
     [x y z])))

(t/cf (t/defn create-vec [] :- vec-3d
        (->> (range) (take 3) to-vec)))

;; create a 3d vec.
(t/cf (create-vec))

;; sum up 2 vectors
(t/cf (sum-3d (create-vec )
              (create-vec)))

(t/cf (sum-3d [1 2 3]
              (create-vec)))

;; core.typed cannot infer this.
(t/cf (sum-3d [1 2 3]
              (take 3 (range))))








