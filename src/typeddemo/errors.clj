(ns typeddemo.errors
  (:require [clojure.core.typed :as t]))

;; Understanding errors in core.typed
;; --------------------------------------------------------------------------------

;; In practice you'll see quite a few errors when you play around with core.typed.
;; The following examples all fail

(t/cf (reduce conj [] (range 10)))

;; like above, but why don't we get vector of Any at least?
(t/cf (reduce #(conj %1 %2) [] (range 10)))

;; this one fails too.
(t/cf (map vector (range 10)))

;; Also in this simple example we get an error.
(t/cf (map identity (range 10)))

;; can't we use comp?
(t/cf (map (comp inc ) (range 10)))


;; At this point it's not clear. Do we have a bug in our code or is it due to core.typed
;; limitations? I don't know core.typed well enough to answer this, but fact is that
;; fn's like reduce, map, vector, comp are pretty generic and have complex signatures.
;; Often I found that we're just missing the necessary hints and expect core.typed to do
;; magic. As a general rule of thumb, I would say: check if your hints are correct.
;; if you don't have any like in the examples above then core.typed will not do the job for
;; you. So, don't expect that your code silently passes type checking.


;; In this chapter we try to understand the error messages. When you use core.typed
;; you have to learn how to read them.

;; try to run the following. You'll get an error 
(t/cf (reduce #(conj %1 %2) [] (range 10)))

;; Type Error .... Polymorphic function conj could not be applied to arguments:
;; Polymorphic Variables:
;; 	x
;; 	y

;; Domains:
;;      .... as I said above.... I start by looking at the last line here, this is the often the general arity of conj and if
;;      core typed cannot infer this one, then of course I get a type error. However, that's not a general rule, your problem might 
;;      have to do with another arity. (when I say arity, I mean the arity of core.typed (t/cf conj) and not (doc conj))
;;
;; 	(t/Coll t/Any) t/Any t/Any *

;; Arguments:
;; 	t/Any t/Any

;; Ranges:
;;      The ranges tried by core.typed. As you see, they have to do only with the first parameter.
;;      
;;      ... all tests done by core.typed.
;; 	(t/Coll t/Any)


;; First thing to solve this is to find the problematic code. In this example it's easy, but if you have a more involved sexpr, it might be harder.
;; in this example the following form gives the same error.
(t/cf #(conj %1 %2))
;; What the error tells you is that the argument [t/Any, t/Any] could not be matched against the domain. The domain is nothing but
;; the remaining possible arities of conj.
;; now read the type of conj.
(t/cf conj)

;; the first argument of conj is constrained. Yep, you can't say (conj "qux" 1), that's what core.typed tells you and also (doc conj).
;; The solution to this form we already discussed in repl.clj, you need to hint the anonymous fn.


;; For the next example, run
(t/cf (map vector (range 10)))

;; You'll see core.typed not yet implemented error. Vector has polymorphic params in core typed, and it seems it cannot bind
;; the actual type (ASeq Num). Check out
(t/cf vector).

;; Good thing is that it seems it will be implemented in the future,
;; so that it will automatically infer the usage. Until we get that, you'll have to hint it yourself.
;; The t/inst funtion is used to make a polymorphic arg more specific.

;; give me an instance of the vector fn, with polymorphic parameter bound to t/Num, then the rest works out of the box.
(t/cf (map (t/inst  vector t/Num) (range 10)))

;; I can even add one more element to it and still have a sequence.
(t/cf (-> (map (t/inst  vector t/Num) (range 10))
          (conj 1)))

;; first element must be a vector
(t/cf (-> (map (t/inst  vector t/Num) (range 10))
          first))

;; now we want the first element of each vector.
;; but get an error. let's check the error.
(t/cf (-> (map (t/inst  vector t/Num) (range 10))
          (map first)))

;; I have pprinted the arguments here.

;; Type Error .... Polymorphic function map could not be applied to arguments:
;; Polymorphic Variables:
;; 	c
;; 	a
;; 	b

;; Domains:
;;      The complaining function has to be map. You can check it with (t/cf map)
;;      you'll see exactyl those 2 aritites.
;;
;; 	[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b
;; 	[a b ... b -> c] (t/U (Seqable a) nil) (t/U (Seqable b) nil) ... b

;; Arguments:
;; This argument is the result from the first map operation (t/cf (map (t/inst  vector t/Num) (range 10)))
;; 1st arg:
;; (t/ASeq
;;  (t/I
;;   IObj
;;   (IPersistentVector t/Num)
;;   (java.util.Collection t/Num)
;;   (Iterable t/Num)
;;   (java.util.List t/Num)))

;; This argument is the type of the first function (t/cf first)
;; 2nd arg:
;; (t/All
;;  [x]
;;  (t/IFn
;;   [(t/HSequential [x t/Any *]) -> x :object {:path [(Nth 0)], :id 0}]
;;   [(t/Option (t/EmptySeqable x)) -> nil]
;;   [(t/NonEmptySeqable x) -> x]
;;   [(t/Option (Seqable x)) -> (t/Option x)]))	 

;; Ranges:
;; 	(t/NonEmptyASeq c)
;; 	(t/ASeq c)

;; Let's simplify it a bit for better readability:

;; this

(t/cf map)

;; => gives

;; (All
;;  [c a b ...]
;;  (t/IFn
;;   [[a b ... b -> c]  (t/NonEmptySeqable a) (t/NonEmptySeqable b)  ...  b -> (t/NonEmptyASeq c)]
;;   [[a b ... b -> c] (t/U (Seqable a) nil) (t/U (Seqable b) nil) ... b -> (t/ASeq c)]))

;; when you have only 2 args, this reads as

;; (All
;;  [c a b ]
;;  (t/IFn
;;   [[a -> c] (t/NonEmptySeqable a) -> (t/NonEmptyASeq c)]
;;   [[a -> c] (t/U (Seqable a) nil) -> (t/ASeq c)]))

;; Easy to read:
;; Either map gets a function a->c and non empty seq of a's, then the result is a non-empty seq of c's
;; or it gets a function a->c and a possibly empty sequence, then it also returns a possibly empty sequence of c's.


;; 1st arg: (t/ASeq TypeA)
;; 2nd arg: (t/All (t/IFn ...))

;; let's put is together:
;; map:  a->c            |   sequence of a
;; arg:  (t/ASeq TypeA)  |   (t/All (t/IFn ...))

;; OH. Is (t/ASeq Type) a function a->c ?
;; is (t/All [] (t/IFn)) a sequence???

;; Hurra, core typed just found a bug. We used the wrong threading macro.

;; Now, should that work? since we now use the good threading macro
(t/cf (->> (map (t/inst  vector t/Num) (range 10))
           (map first)))


;; Hm, we get almost the same error. just with the args swapped.
;; You now need to match it more precisely: The following makes a strong guarantee:

;;   [[a -> c] (t/NonEmptySeqable a) -> (t/NonEmptyASeq c)]
;;   [[a -> c] (t/U (Seqable a) nil) -> (t/ASeq c)]

;; However, we have
;; a seq: (t/ASeq TypeX) (notice TypeX!!!)
;; a function (first) that has polymorphic parameters.
;; As we saw before, when you use higher order fn's with polymorphic parameters you need to hint it.
;; In this case weed to tell core.typed that we want a *first* function that will accept a seq of nums.

;; voila, this time all is fine.
(t/cf (->> (map (t/inst vector t/Num) (range 10))
           (map (t/inst first t/Num))))

;; this version can even infer that result is not empty and that elements are never nil.
(t/cf (->> [1 2 3]
           (map #(do (assert (number? %)) [%]) )
           (map (t/inst first t/Num))))

;; Next example.
;; we want to reduce a function over a collection. simple.

;; The reducing-fn shall be: Give me seq of numbers plus a number and I give you
;; a sequence of numbers back.
(t/cf (t/ann summarize
             (t/IFn [(t/ASeq t/Num) t/Num -> (t/ASeq t/Num)])))

;; conj satisfies this condition.
(t/cf (def summarize conj) )

;; ups, doesn't work???
(t/cf (reduce summarize [] (range 10)))

;; Why?? since it works for conj !!
(t/cf (reduce conj [] (range 10)))

;; Let's check the errors once more.

;; Type Error .... Polymorphic function reduce could not be applied to arguments:
;; Polymorphic Variables:
;; 	a
;; 	c

;; Domains:
;; 	[a c -> (t/U (Reduced a) a)] a (t/Option (Seqable c))

;; Arguments:
;; 	[(t/ASeq t/Num) t/Num -> (t/ASeq t/Num)] (t/HVec []) (t/ASeq t/AnyInteger)

;; Ranges:
;; 	a


;; Simplify it by removing some edge cases. I assume *Reduced* has to do with reducers and
;; Option is typical edgecase, then it becomes
;; 	a
;; 	c

;; Domains:
;; 	[a c -> a] a  (Seqable c)

;; Arguments:
;; 	[(t/ASeq t/Num) t/Num -> (t/ASeq t/Num)] (t/HVec []) (t/ASeq t/AnyInteger)

;; Ranges:
;; 	a

;; READS::

;; [(t/ASeq t/Num) t/Num -> (t/ASeq t/Num)] (t/HVec []) (t/ASeq t/AnyInteger)
;; [      a         c    ->    a          ]     a         (Seqable c)
;;
;; This feels like solving a math equation by inferencing.
;; The first a must be compatible with (t/ASeq t/Num), then the second a equally
;; The third a must be compatible with (HVec [])
;; The first c must be compatible with t/Num
;; The second c on the right hand side (Sequable c) must be compatible with (Seq AnyInteger)
;;
;; Core.typed complains about a, so everything seems fine with b. let's proove it.

;; Can a sequence of integers be used when a seqable of nums is expected => yes
(t/cf (t/ann-form (t/ann-form () (t/ASeq t/AnyInteger))
                  (t/Seqable t/Num)))

;; Can an empty vector of be used when a seq of Nums is expected? => no
(t/cf (t/ann-form (t/ann-form [] (t/HVec []))
                  (t/ASeq t/Num)))

;; not even a vector with a number when we expect a seqable 
(t/cf (t/ann-form (t/ann-form [1] (t/HVec [t/Num]))
                  (t/ASeq t/Num)))

;; A short reminder: A vector is not a sequence in clojure
(seq? [1 2]) ;; => false

;; you can make it a sequence if you want, so it's seqable, but just not a sequence
;; so core typed is consistent here.
(seq? (seq [1 2]))

;; In fact when we start with a list it works, because (seq? ()) is true.
(t/cf (reduce summarize () (range 10)))

;; The signature of our function summarize is just using the wrong type.

;; Here with vector instead.
(t/cf (t/ann summarize
             (t/IFn [(t/Vec t/Num) t/Num -> (t/Vec t/Num)])))

(t/cf (def summarize conj) )

;; yep, all good now.
(t/cf (reduce summarize [] (range 10)))

;; In clojure, conj makes certain guarantees about insertion order of your collection so
;; it might very well be a bug if you pass a list to conj and expect it to have your
;; element appended. core.typed can prevent these kind of errors and the analysis wasn't
;; too hard to read in this case.

;; If you really wanted to allow lists or vectors, then you have the wrong signature
;; for the summarize-fn. Here a signature that allows seq and vectors.

;; Note: when you use Seq intead of ASeq it will not work: reason, the last
;; arity of (t/cf conj) will show you that it's of type (t/Coll Any), so Seq
;; will match Any and not Num.
(t/cf (t/ann summarize
             (t/IFn [(t/Vec t/Num) t/Num -> (t/Vec t/Num)]
                    [(t/ASeq t/Num) t/Num -> (t/ASeq t/Num)]
                    )))


(t/cf (defn summarize [acc x]
        (conj acc x)  ))

;; vector of nums
(t/cf (reduce summarize [] (range 10)))

;; sequential clojure object 
(t/cf (reduce summarize () (range 10)))


;; SUMMARY:
;; You might have a expected less hinting in core.typed but that's how things are
;; at least for now. The not-impl-exception on the example before tells me that we
;; might get it one day, until then you have to learn how to hint it.
;; An intemediary observation could be that I have to use lots of boilerplate hinting code,
;; making core.typed more a disadvantage than an advantage. Now, in practice you can
;;
;; * exclude a form or fn from checking if you really know what you're doing.
;; * define a set of type safe variants of the core functions and reuse them.
;; * keep a set of signatures and reuse them (to define new vars, etc)

;; Examples

;; Here we instantiate versions of vector/first for a particular polymorphic type: Num
(t/cf (t/def vector-num (t/inst vector t/Num)))
(t/cf (t/def first-num (t/inst first t/Num)))

;; now we can just use a type safe version upfront.
(t/cf (->> [1 2 3]
           (map vector-num)
           (map first-num)))

;; In this example we define a reusable type:
(t/cf (t/defalias data-type (t/Map t/Kw t/Num)))

;; yep, {:k 1} is compatible with our type.
(t/cf (t/ann-form {:k 1} data-type))

;; ups, not compatible
(t/cf (t/ann-form {:k "qux"} data-type))

;; now we can define a function reusing our data type.
(t/cf (t/defn assoc-1 [x :- data-type] :- data-type (assoc x :key 1)))

;; won't type check due to bug.
(t/cf (t/defn assoc-qux [x :- data-type] :- data-type (assoc x :key "qux")))

;; A function with data-type in, data-type out.
(t/cf (t/defalias sig1 (t/IFn [data-type -> data-type])))

;; 3 fn's with those signatures.

(t/cf (t/ann f1 sig1))
(t/cf (t/ann f2 sig1))
(t/cf (t/ann f3 sig1))

;; this fn matches the signature.
(t/cf (defn f1 [x] (assoc x :key 1)) )

;; Next fn has a bug. you don't really know if v is a number or not
;; since the map does not neccessarily contain that key.
(t/cf (defn f2 [x]
        (let [v (get x :key)]
          (assoc x :key (inc v)))) )

;; Sad, the assertion trick does not work
(t/cf (defn f2 [x]
        (let [v (get x :key)]
          (assert (number? v))
          (assoc x :key (inc v)))) )

;; Why not? This will print the environment.
(t/cf (defn f2 [x]
        (let [v (get x :key)]
          (assert (number? v))
          (t/print-env "env: ")
          (assoc x :key (inc v)))) )
;; {:env {x__#0 (IPersistentMap t/Kw t/Num)},
;;  :props ((is Number x__#0 [(Key :key)])),
;;  :aliases {v__#0 {:path ({:val :key}), :id x__#0}}}

;; in this environment we see that v is mapped to be a path inside x
;; which is correct, but it doesn't infer it to be a number and hence (inc v) fails.

;; this is a bit frustrating, the assert trick worked before and now it does no longer work
;; I can't tell you why but the in core.typed there's another let form, maybe it has to do with it?
;; we'll look at 2 solutions now:

;; short helper. these kind of fn's can be very helpful.
(t/cf (t/defn cast-num [x :- t/Any] :- t/Num
        {:pre [(number? x)]}
        x))

;; now this does the trick.
(t/cf (defn f2 [x]
        (let [v (get x :key)]
          (assoc x :key (inc  (cast-num v))))))

;; but hey, what if we write it inside the let form?
;; now it pretends that i is still of the type of v which we know is wrong.
;; really odd what happens, but since let is a special form many things can be going on under the hood.
(t/cf (defn f2 [x]
        (let [v (get x :key)
              i (cast-num v)]
          (assoc x :key (inc i)))))


;; In fact, there is a build-in let form, maybe we should use that one instead?
;; nice, probably it's there for a reason. You should check out the core.typed API, there are also
;; other forms, such as loop, for, doseq.
(t/cf (defn f2 [x]
        (t/let [v :- t/Any (get x :key)
                i :- t/Num (cast-num v)]
          (assoc x :key (inc i)))))

;; now that we've seen the special let form, let's try again with our assertion.
(t/cf (defn f2 [x]
        (t/let [v :- t/Any (get x :key)]
          (assert (number? v))
          (assoc x :key (inc v)))) )

;; I can't tell you exactly the difference between the 2 let forms, so if in doubt,
;; use the core.typed form.



