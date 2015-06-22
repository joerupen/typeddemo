(ns typeddemo.functions
  (:require [clojure.core.typed :as t]))

;; When you are used to type checking in other languages, then there is one thing
;; in core.typed that you will miss
;; https://frenchy64.github.io/typed/clojure,/core.typed,/clojure/2013/09/03/polymorphic-hof.html

;; Core.typed cannot infer higher order polymorphic variables, at least not yet.

;; This namespace will show some example of polymorphic Types and some higher order
;; funtions that consume it.

;; First some basics.

;; remember a basic signature of a function of 1 arg to 1 arg (any)
(t/cf (t/defalias T1 (t/IFn [t/Any -> t/Any])))

;; There is no coupling between the input type and the output type.
;; input string -> output number, or
;; input seq -> output string
;; all of these are possible since every type is compatible with any.

(t/cf (t/ann f1 T1))
(t/cf (t/ann f2 T1))
(t/cf (defn f1 [x] x))
(t/cf (defn f2 [x] (str x)))

;; The signature T1 can now be parameterized so we can e.g. constraint the output
;; type based on the input type.

(t/cf (t/defalias T1-poly (t/All [x] (t/IFn [x -> x]))))

;; now we create a fn of this polymorphic signature.
(t/cf (t/ann f1-poly T1-poly))
(t/cf (t/defn f1-poly [x] x))

;; calling with string yields string
(t/cf (f1-poly "str"))

;; calling with value returns value
(t/cf (f1-poly 123))

;; calling with num, return num. all in sync with Java semantics for generics, right?
(t/cf (f1-poly (t/ann-form 123 t/Num)))

;; We can say that core.typed could infer the type fo the polymorphic parameter x in every case. unfortuntely this is not always the case as we shall see. But for now, look at the following direct instantiations of the f1-poly function for particular types.

;; num -> num
(t/cf (t/inst f1-poly t/Num))

;; str -> str
(t/cf (t/inst f1-poly t/Str))

;; the inst function returns a type where x is fixed. we shouldn't be able to misuse it.

;; ok
(t/cf ((t/inst f1-poly t/Str) "str"))
;; not ok.
(t/cf ((t/inst f1-poly t/Str) 12))

;; Using a polymorphic function
;; --------------------------------------------------------------------------------

;; We showed above how easy it is for core.typed to infer that (f1-poly "str")
;; is of type string. now check the following

(t/cf (map f1-poly (range 10)))

;; => core.typed is complaining about the function map

;; Type Error (...) Polymorphic function map could not be applied to arguments:
;; Polymorphic Variables:
;; 	c
;; 	a
;; 	b

;; Domains:
;; 	[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b
;; 	[a b ... b -> c] (t/U nil (Seqable a)) (t/U nil (Seqable b)) ... b

;; Arguments: (
;; 	typeddemo.functions/T1-poly (t/ASeq t/AnyInteger)

;; Ranges:
;; 	(t/NonEmptyASeq c)
;; 	(t/ASeq c)

;; this is one of the limitations of core.typed: polymorphic higher order functions.
;; The way you can fix it is manually, via explicit instantiation.
(t/cf (map (t/inst f1-poly t/Num) (range 10)))

;; or you can instantiate map.
(t/cf ((t/inst map t/Num t/Num) f1-poly (range 10)))

;; you might be curious how the signature of such a map functions looks like.
;; actually even simpler than before.
(t/cf (t/inst map t/Num t/Num))

;; Multiple polymorphic params
;; --------------------------------------------------------------------------------
;; we can also try with more args and a more complex signature:

;; given *a* and a function a->b and a function b->c, return c.
(t/cf (t/defalias T2-poly-multi (t/All [a b c]
                                 (t/IFn [a
                                         (t/IFn [a -> b])
                                         (t/IFn [b -> c]) -> c]))))

;; the following implementation seem to satisfy the signature.
(t/cf (t/ann-form (fn [a fb fc] (-> a fb fc))
                  T2-poly-multi))

(t/cf ((t/ann-form (fn [a fb fc] (-> a fb fc))
                   T2-poly-multi)
       100 str count))


;; N-ary polymorphic parameters
;; --------------------------------------------------------------------------------

;; we can define a type that has N polymorphic parameter using the ... symbol

(t/cf (t/defalias T3-nary (t/All [a b ... ]
                                 (t/IFn [a b ... b -> (t/HSeq [a b ... b])]))))


;; TODO 



;;

;; a few interesting reads:

(t/cf identity) 

(t/cf constantly )

(t/cf comp)

(t/cf complement)

(t/cf partial)

;;(t/cf juxt)

(t/cf memoize)

(t/cf apply)

(t/cf ifn?)




