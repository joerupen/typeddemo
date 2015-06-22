(ns typeddemo.repl
  (:require [clojure.core.typed :as t]))

(def p clojure.pprint/pprint)


;; --------------------------------------------------------------------------------
;; Introduction
;; --------------------------------------------------------------------------------

;; There are 2 essential ways how you can have your code typed checked in core.typed.
;; Either via t/cf (check only a form) or via t/check-ns (check complete namespaces or whole project)

;; This namespace is intended to be checked only at the repl via t/cf.
;; You should evaluate each expression at the repl (in order of this namespace). 

;; The t/cf function will receive a form as input and will return its type. Notice that the genral workflow
;; of the t/cf function is the following:

;; (t/cf form) => read form => analyze > type check => eval form => return inferred type.

;; The evaluation of the form is an important step if the *form* has side effects, like def.
;; If the form was not evaluated, then you'd get a type information for a var wouldn't even exist.

;; Example: The following will query the signature of the str function and return its type:
;; (t/cf str) => [Any * -> Any]

;; This namespace will mainly do (t/cf something-interesting) and we will discuss the results.

;; Let's get started with some simple experiments.


;; Primitives: We start by checking some basic primitives in clojure.
;;--------------------------------------------------------------------------------

;; Notice that the first time you evaluate an expression, the core.typed environment
;; will be initialized (will cause a delay of approx 10 sec)

;; What is the type of 5? Expecting java.lang.Number maybe?
(t/cf 5)

;; => nope, you get the type Val.

;; try with strings.
(t/cf "qux")
;; => also here, you get type Val

;; or keywords
(t/cf :keyword)

;; symbols are types
(t/cf 'qux)

;; no booleans here as you'd expect. type is 'true'
(t/cf true)

;; yep, nil is a type.
(t/cf nil)


;; What's the conclusion?? Are you getting (t/Val "qux") and not java.lang.String?
;; In core.typed t/Val is a type. Strange when you come from languages like Java,
;; where "qux" is an instance and not a type. But you can rest assured that in core.typed
;; (t/Val "qux") is subtype of String. So everywhere you expect a string you can pass
;; a compatible value.

;; Welcome to core.typed. It's a diffent type system!

;; If (t/Val 5) is a Type then we can immagine to define a function signature that
;; only succeeds the type check if its called exactly the value 5 as argument. Isn't that strange?

;; Unusual at least, but later I show some examples how we can make usage of it (enums.clj)

;; What are those {:then tt :else ff}? Those are called filters and can do pretty awesome stuff.
;; Later you'll see them in action, for now you can ignore them.


;; Ann-Form
;; --------------------------------------------------------------------------------

;; We could now just continue our journey and evaluate expressions such as (t/cf (reduce conj [] (range 10))
;; or (t/cf partition-by) but we have to be a bit systematic. Very quickly you will run into
;; trouble when you don't exactly understand how core.typed infers the type (means you can get error messages
;; that are not easy to read when you don't have built up the skills). I found the following
;; function, ann-form to be extremely useful. It lets us quickly check if 2 types are compatible.

;; The ann-form fn is used to annotate an expression. This means if you enter (t/cf expr Type), it will
;; analyze expr and assign Type to it (if that's possible). The type of expr itself is also inferred,
;; so this is an easy way to check type casts.

;; Example (cf 1) => Val
;;         (cf (ann-form 1 Num)) =>
;;
;; Here we ask core.typed to annotate the result of *1* with the Type Num if that's possible. Since
;; (Val 1) is subtype of Num, (Val 1) :< Num, the 'cast' succeeds. (I've not seen the term 'cast' in core.typed
;; but I assume it's an equivalent term)

;; We build up, step by step more and more complex expressions  and check their types.

;; Can you pass 1 when you only expect value 1?
(t/cf (t/ann-form 1 (t/Val 1)))

;; can you pass 1 where you expect value 2?
(t/cf (t/ann-form 1 (t/Val 2)))

;; Can you pass 1 when you expect value 1 or 2? (notice the Union type)
(t/cf (t/ann-form 1 (t/U (t/Val 1) (t/Val 2))))

;; can you pass 1 where you expect num?
(t/cf (t/ann-form 1 t/Num))

;; can we use true as a num? no.
(t/cf (t/ann-form true t/Num))

;; is nil a valid keyword? no
(t/cf (t/ann-form nil t/Kw))

;; Can you pass 2 where you expect a long?
(t/cf (t/ann-form 2 Long))

;; can you pass byte 2 when you expect a byte?
(t/cf (t/ann-form (byte 2) Byte))

;; can you pass an integer where you expect a byte?
(t/cf (t/ann-form (int 0) Byte))

;; Can you pass nil where you expect a string?
(t/cf (t/ann-form nil t/Str))

;; => So far core.typed is really easy, you just need to understand the type hiearchy.
;; Of course the complexity comes with more general types (sequences, polymorphic types aka generics
;; higher-order fn's etc).


;; Collections: Try to evaluate the following expressions.
;; --------------------------------------------------------------------------------

;; would you expect (Vec Num) ?
(t/cf [1 2 3])

;; you get something different, but as long as it's compatible....
(t/cf (t/ann-form [1 2 3] (t/Vec t/Num)))

;; is a vector compatible with a sequence of num? no???
(t/cf (t/ann-form [1 2 3] (t/Seq t/Num)))

;; why not? vectors are not sequences in clojure.
(seq? [1 2 3])

;; this is compatible with a sequence of num?
(t/cf (t/ann-form (seq [1 2 3]) (t/Seq t/Num)))

;; and this? no? because (seq []) is nil, and that's not an Seq of Num
;; the error message might be hard to read now, but we will later how to systematically
;; read errors.
(t/cf (t/ann-form (seq []) (t/Seq t/Num)))

;; However, you can just check the type of (seq []) directly and determine yourself it you
;; could cast it to a seq of num.
;; => (t/Option (t/NonEmptyASeq t/Nothing)). So the Type reads: Either seq returns nil,
;; or it returns an non-emptyseq of nothing. Noway we can pass a non-empty sequence of Nothing
;; to a sequence of Any, so to make this cast possible we must allow nil on the right hand side.
(t/cf (seq [])) 

;; then it must be compatible with an *optional* seq of num.
(t/cf (t/ann-form (seq []) (t/Option (t/Seq t/Num))))


;; maps are inferred as HMaps. Every key and val gets a type.
(t/cf {:a 1 :b 2})

;; is it compatible with a more general map type? yes.
;; this means {:a 2} would be valid input for a function (Map Kw Num)
(t/cf (t/ann-form {:a 1 :b 2} (t/Map t/Kw t/Num)))

;; and an empty one?
(t/cf (t/ann-form {} (t/Map t/Kw t/Num)))

;; can we have nils in values? no.
(t/cf (t/ann-form {:a 1 :b 2 :c nil} (t/Map t/Kw t/Num)))

;; and here? 
(t/cf (t/ann-form {:a 1 :b 2} (t/Map t/Kw (t/Option t/Num))))


;; Have you noticed that core.typed inferred things like HVec and HMap (which are still compatible
;; to their more generic counterparts) ?
;; Core.typed has various version of vector, map or sequences, but here it infers a pretty
;; rich type. A *vector with values 1,2,3* is way more precise than saying a Seq of Numbers. Also for maps
;; we get quite some information. Also the map is much more precise than just a regular map with key/value type.


;; Java types
;; --------------------------------------------------------------------------------


;; here we get a plain old java class.
(t/cf (java.util.Date.))

;; In core.typed there is also a root object. It's called Any. java.lang.Object is not the root here.
(t/cf (java.lang.Object.))

;; Can I pass a date when you expect object?
(t/cf (t/ann-form (java.util.Date.) java.lang.Object))

;; notice that the following does not return java.lang.String. It returns a core.typed string alias.
(t/cf (String. "qux"))

;; can I pass Object when I expect numbers?
(t/cf (t/ann-form (java.util.Object.) t/Num))

;; Vars.
;; --------------------------------------------------------------------------------

;; Vars have either some associated type information or not (in core.typed).
;; Core.typed comes with an extensive coverage of type information over the clojure.core api.
;; When core.typed checks forms, it must have some side information about the vars involved
;; in the form. If it can't resolve the type info for a var, you'll get an error.
;; Core.typed maintains an environment for all annotated vars. The form t/cf alters that enviornment
;; when it encounters definitions like def, defn, defalias or ann.
;; If you simple annotate a form and then check it via t/cf you'll notice it's not annotated. Example:

;; define something
(def a 1)
;; annotate it.
(t/ann a t/Num)
;; check its type. ups doesn't work.
(t/cf a)

;; See, it's not in the environment
(get (:vars (t/envs)) 'typeddemo.repl/a)

;; you need to get the right workflow at the repl!!
;; you must wrap the annotation in t/cf, otherwise the environment is not altered
(t/cf (t/ann a t/Num))

;; Now it is in the environment.
(get (:vars (t/envs)) 'typeddemo.repl/a)

;; now that we know that a is a number
(t/cf a) ;=> num

;; the following will succeed on the REPL. It wouldn't work with check-ns if the annotation was already declared,
;; but in this case core.typed is not involved in checking.
(def a :bad)

;; now core.typed can see it.
(t/cf (def a :bad))

;; notice, you don't necessarily have to annotate. core.typed will then try to infer its type.
(t/cf (def t44 "str"))
(t/cf t44)

;; The following example uses the t/def form from core.typed. It allows you to join annotation and definition.
(t/cf (t/def t :- t/Str "str"))

;; Notice that the previous examples return a var.

;; Types of build-in vars
;; --------------------------------------------------------------------------------

;; Try to check the types of some build-in vars.

;; the following should be familiar
(t/cf *out*)

;; The next one can either be nil or a Throwable. Notice the representation of a union in core.typed.
(t/cf *e)

;; you can play around and check many of the build-in clojure vars. Try to exercise some vars from the
;; clojure api, http://clojure.github.io/clojure/clojure.core-api.html, or you can use the t/envs function
;; to explore which vars are annotated.
;; e.g.  (t/cf *ns*)


;; Clojure.core signatures
;; --------------------------------------------------------------------------------

;; Also in this section we look at built-in vars, but of Type function.

;; The following examples will show give a brief overview how Type information is represented in core.typed.
;; The goal is not to understand systematically all built-in vars, but just to get a coarse grain feeling of how
;; types are represented. You'll see that Type definitions can become quite complex.

;; You'll learn how to read type defintions of functions. Also you'll notice that it's not easy to define
;; a signature for many simple clojure fn's as they are so powerful.
;; The clojure.core/map function works on sequences but can also accept nil
;; as input. Also it can accept more than one collection. And a particular feature of the map function
;; is that it guarantees that the output contains as many elements as the input. The map fn in Java streams
;; accepts an fn and returns a stream. However when you try to get the first element of that stream how
;; do you know if it's null or not? There is no typechecking there. You need to check it by hand.
;; core.typed cannot guarantee that the number of elements is the same but it can infer that if you
;; pass in a non-empty seq, then you also get a non empty seq in the output. Getting the first element
;; of a non-empty seq guarantees to have the type != nil. So that's pretty cool, and it's obvious that
;; the signature will need to tackle that.

;; In order to understand fn signatures, here's the defintion of a function Type.

;; (t/Fn [Type1 Type2 .... -> ResultType]
;;       [... next arity ...] ...)
;;
;; There is ofc more to it. For instance a signature can contain an asterix * to denote varargs.

;; For instance (Fn [Any * -> Str]) accepts a variable number of *anything* and returns a string.
;; If you think about the str function, it returns string "" (empty string) even for nil. 

;; Simple signatures.
;; --------------------------------------------------------------------------------

;; Try to understand the signature of the + function.
;; => Adding 2 longs gives long, mixing double with longs gives long, etc.
;; you'll see that the signature is already pretty detailed about the possible options.
;; Notice that the signature is using varargs *. [Long *] means zero or more Longs.
(t/cf +)

;; At this point it's worthwile to distinguish core.typed arities and clojure arities.

;; (doc +)
;; clojure.core/+
;; ([] [x] [x y] [x y & more])
;;   Returns the sum of nums. (+) returns 0. Does not auto-promote
;; longs, will throw on overflow. See also: +'

 ;; (t/IFn
 ;;  [Long * -> Long]
 ;;  [(t/U Double Long) * -> Double]
 ;;  [t/AnyInteger * -> t/AnyInteger]
 ;;  [Number * -> Number])

;; Both clojure and core.typed have 4 arities, but they are not directly linked together. Each core.typed
;; arity matches all 4 clojure arities for a particular Type. So when core.typed see a function invocation to +,
;; it will first check if all passed arguments are of Type Long. Then it knows the result will be long. Then it
;; tries the next Type. Finally it tries the most general Type (Num) and obviusly can only infer that the result
;; is num.

;; similar for quot. This time it accepts only 2 args
(t/cf quot)

;; This one is easy to read.
(t/cf str)

;; Signatures getting more complex
;; --------------------------------------------------------------------------------


;; The *map* fn.
(t/cf map)

;; => 
;; (All
;;  [c a b ...]
;;  (t/IFn
;;   [[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b -> (t/NonEmptyASeq c)]
;;   [[a b ... b -> c] (t/U nil (Seqable a)) (t/U nil (Seqable b)) ... b -> (t/ASeq c)]))


;; * Map is a polymorphic fn. The arguments are (All [c a b ...])
;; * map will try to first check if the passed arguments are non-empty-seqables, then it can make a guarntee
;;   about the resulting sequence.
;; * otherwise it falls back to a general case, potentially returning an empty sequence.
;; * we will look into polymorphic parameters in the functions.clj namespace.


;; some Types can be very large. But if you're in doubt, start at looking at the last signature
;; which is the most generic one: that's easy
;;[[t/Any -> t/Any] [t/Any -> t/Any]   *      ->       [t/Any * -> t/Any]]
;;      fn1             fn2           more    returns    fn with vargs that returns any.
;; given an fn1 and eventually more fn's, return a fn with vargs. that reads like (doc every-pred)
(t/cf every-pred) 



;; Some types are not annotated yet. The following gives an error.
;; (t/cf frequencies)
;; It's also not easy to define a signature for such an fn. Okay, it returns a map where the
;; vals are Numbers, but what are the keys? the distinct set of elements in coll of course,
;; so if input is string, number, keyword ... you need to define a type that ensures that.
;; Might not even be possible, maybe you can annotate it with [Map t/Any t/Num] but you probably
;; cannot go further than that.

;; other interesing vars to check

;; an easy one, reads: For each tpe X, the two Input args are X and a sequence of X's
;; the result is a seq of X.
(t/cf cons) 

;; Similar fn but more complicated, why? because conj ensures that the type of collection you
;; conj into is preserved. To do that we need multiple arities. I find this really nice.
;; also when you conj into a map your element must be a map entry. It just wrong to conj [1 2 3] into a map.
(t/cf conj) 

;; like above, try to read this.
(t/cf into) 



;; let's analyze the last arity type of the allmighty reduce fn:
;; [[a c -> (t/U a (Reduced a))] a     (t/Option (Seqable c)) -> a]
;;    ---- reducing-fn -------   seed  coll                   returns a
;; * reducing-fn receives 2 args and returns the accumulator type
;; * the seed is of same type as the accumulator typed passed to the reducing-fn
;; * the collection can be a sequence of C's, or nil
;; * The Type C is used is passed as second arg in the reducing-fn
;; * the return type is the same type as the accumulator.
;; The signature also has other arities: if you don't provide a seed and your collection is not empty
;; then you just return the type of the accumulator. However, if your sequence can be empty than you
;; should provide a reducing-fn that also can take zero args. That's exactly what (doc reduce) tells you.
(t/cf reduce)

;; wow, this one seems complicated for a such a simple fn. Given X returns X
;; , but it also does some guarantees in filter.
;; I can't really tell you what the :filters and :id mean here, but you can tell me ;)
(t/cf identity)

;; Given a map, return a map, given a vec return a vec. Easy ;)
(t/cf assoc)

;; Given a map and more args, return a map. k, v are polymorphic.
(t/cf dissoc)

;; Observations
;; Yeah, reading types can take some time to get used to. you can play around and learn to read
;; type info from many build-in functions that you use every day. It also shows how detailed core.typed tackles
;; the clojure.core API. After all, in clojure for all collection functions the inputs are seq's, outputs are seq's
;; and core.typed could just check that. But it does much more than that. It tries to respect the invariants of a
;; richer Type system.


;; Checking expressions
;; --------------------------------------------------------------------------------
;;
;; So far we actually didn't use core.typed to infer much. It infered constant values and build-in vars
;; now its time to do some actual type inference.

;; a small calculation is inferred to Long and not (t/Val 8). After all it's a type checker and not a calculator.
(t/cf (+ 3 5 ))

;; but wait, here the result is *Error*. Did core.typed fail?  Did it evaluate? Otherwise it couldn't fail.
;; So is it a type inferencer or a calculator?
;; Yes, core.typed also evaluates the form as explained in the intro (core.typed workflow). This is
;; essential in clojure because vars live in namespaces and when you type check definitions, then if the
;; form was not evaluated, you'd return a type for a var that doesn't exist. So, you just need to know this detail.
(/ 1 0)

;; other simple expressions.

;; string
(t/cf (str 1 3 3))

;; long
(t/cf (inc 5))

;; long
(t/cf (quot 10 2))

;; lazy seq is inferred with generic type AnyInteger
(t/cf (range))

;; Returns a non-empty sequence type. 
(t/cf (seq [1 2 3]))

;; (Option t) is union of nil or t. Here t is nonemptysequence of nothing.
(t/cf (seq []))


;; Either nil or non-emptysequence of Nothing (expressed using t/Option)
;; well it's nil we know that, but it can never be non-empty sequence of nothing.
;; Not sure why we get that here
(t/cf (seq nil))

;; This one detects a bug. you can't increment strings. Notice that you get a core.typed
;; exception and not a runtime exception.
(t/cf (inc (str 4)))

;; More real-life examples of forms.
;; --------------------------------------------------------------------------------

;; We use *map* a lot, for instance with a str function the result is seq of strings
(t/cf (map str (range 10)))

;; With inc the result is a sequence of some complex union, why that?
(t/cf (map inc (range 10)))

;; because inc has 4 arities, so the result of map is a unition of those 4 arities.
(t/cf inc)

;; sequence of bytes, because byte has single arity 
(t/cf (map byte (range 10)))

;; a sequence of characters. I am impressed how it inferred this!!!
(t/cf (mapcat str (range 10)))

;; let's try reduce
(t/cf (reduce + (range 10)))

;; So far the input forms have been simple expressions, now we want to use fn's.

;; cool. it figures out the type Any -> String
(t/cf (fn [x] (str x)))

;; also here
(t/cf #(str %))


;; why does the following fail?
;; => by default when you don't annotate your fn's, core.typed treats your args as Any.
;; Obviously inc doesn't like Any.  
(t/cf (map #(inc %) (range 10)))

;; Try to verify this. Try to evaluate just this part of the expression.
;; => yep, fails with same error.
(t/cf #(inc %))

;; let's fix it. 
;; A simple precondition can alter the Type that is passed to inc. Here you see core.typed
;; usage of filters in action. Before the assertion, the type is *Any*, after the assertion the
;; type of x is *t/Num*. awesome!!
(t/cf (fn [x] {:pre [(number? x)]} (inc x)))

;; Now, also the map fn works.
(t/cf (map (fn [x] {:pre [(number? x)]} (inc x)) (range 10)))

;; you can of course also be explicit and use the core.typed t/fn syntax.
;; Now you don't need the assertion.
(t/cf (map (t/fn [x :- t/Num] :- t/Num x) (range 10)))

;; You can define an alias for a reusable signature.
(t/cf (t/defalias n-2-n [t/Num -> t/Num]))

;; and now you can annotate your inner fn.
(t/cf (map (t/ann-form (fn [x] x) n-2-n) (range 10)))
(t/cf (map (t/ann-form identity n-2-n) (range 10)))


;; Another example of automatic fn inference:
;; Input is Number, then we return a sum. Result must be number too, right?
 (t/cf (t/fn [a :- t/Num] (+ a 1)))

;; The resulting Type is the union of both branches, so either num or string
(t/cf (if (seq [1 2 3]) 123 "hello"))

;; pop returns a vector
(t/cf (pop [1 2 3]))

;; peek returns the last element. From a type perspective the generic typed is inferred
;; as the union of 1,2,4. Try to check the signature of peek and you'll see that it's not possible
;; to infer just (t/Val 4). But at least the result (4) is compatible with the Type returned.
(t/cf (peek [1 2 4]))

;; The *first* function returns the first element in a seq, so its type here should be string.
(t/cf (first ["str" :qux]))

;; similar for the second fn.
(t/cf (second ["str" :qux]))
;; This is great, but what? How is this possible?.
;; In many common typesystems you are used to have only sequences of a generic type X (int, string etc).
;; in core.typed you can have heterogenous sequences which can implicitly describe the type of each element
;; in a seq. For instance a vector might be [HVec [Str Str Num Num Kw *]]. This looks like a simplified
;; regular expression ;) as a type!



;; result is sequence of numbers ;), type inferred!
(t/cf (filter number? ["qux" 12 :bar]))
(t/cf (filter number? (seq ["qux" 12 :bar])))

;; result is sequence of the union of the first type and the second type.
(t/cf  (interleave (range 10)
                   (repeat 10 "str" )))

;; here we have seq of (U byte Val)
(t/cf  (interleave (map byte (range 10))
                   (repeat 10 "str" )))

;; result is vector of strings. Nice, not just a sequence, a vector indeed!.
(t/cf (into [] (map str (range 10))))

;; Here the result is a coll of Any. Well, that's because it matches the last signature of *into* which is not
;; using polymorphic params.
(t/cf (into () (map str (range 10))))
(t/cf into)

;; Here a map
(t/cf (into {} (map #(vector % %) (range 10))))

;; you might be disappointed that you get [Map Any Any] and not [Map Num Num], why that?
;; by default fn's are Any->Any and your anonymous fn is treated as default. you need to hint it.
;; I am not sure if this is a missing/upcoming feature in core.typed or it is by design.
;; Would be nice if core.typed could infer the usage of our anonymous fn but for now it can't.

;; However, let's hint it.
(t/cf (into {} (map (t/fn [x :- t/Num] :- (t/HVec [t/Num t/Num])
                      (vector x x))
                    (range 10))))

;; Same here. *Collection of Any* is the result, not *Vec of Num*.
(t/cf (reduce conj [] (range 10)))

;; here the trick with the asssertions to get "vector of Num".
(t/cf (reduce #(do (assert (vector? %1))
                   (assert (number? %2))
                   (conj %1 %2)) [] (range 10)))

;; also here, with core.typed t/fn macro that allows us to be more explict.
(t/cf (reduce (t/fn [acc :- (t/Vec t/Num),  x :- t/Num] :- (t/Vec t/Num)
                (conj acc x)) [] (range 10)))


;; Defining fn's in the repl
;; --------------------------------------------------------------------------------

;; Normally you will have annotations in your namespaces and have them type checked
;; with check-ns. Here we explore how to define fn's directly at the repl

;; 1. Make core.typed infer the signature for you.
(t/cf (defn infer-it-for-me [x] (str x)))

;; yep, it infers string now.
(t/cf (infer-it-for-me :qux)) ;; => string

;; 2. you can annotate a var and pretend it's of a given type.
(t/cf (t/ann num-to-num-fn [t/Num -> t/Num]))

;; Now you can define the var and will ensure it matches your annotated sig.
;; ups, does not match.
(t/cf (defn num-to-num-fn [x] (str x)))

;; yeah, now it matches. matches
(t/cf (defn num-to-num-fn [x] (inc x)))

;; result is num of course.
(t/cf (-> 1 inc num-to-num-fn inc num-to-num-fn))

;; 3. A more direct altenative is to use the t/defn macro which does both for you
;; (the def plus the ann.)

;; t/defn variant.
(t/cf (t/defn automagic [x :- t/Num] :- t/Str (-> x inc str)))

;; yep, works.
(t/cf (-> 5 inc automagic count))

;; 4. You can define an expected signature and reuse it multiple times.
(t/cf (t/defalias my-sig [t/Num -> t/Str]))

;; now you can define fn foo to match this signature
;; Note that this version will first analyze the signature of foo and then
;; make sure that it's compatible with my-sig
(t/cf (defn qux [x] (str x)) my-sig)


;; Annotate a var with an existing siguature (Type)
(t/cf (t/ann some-var my-sig))

;; bad version detected due to type checking.
(t/cf (defn some-var [x] (inc x)))

;; good version
(t/cf (defn some-var [x] (str x)))

;; did it resolve the user's type?
(t/cf some-var)

;; returns string.
(t/cf (some-var 34))

;; NOTE!!!
;; in the example above I use the syntax [t/Num -> t/Str]. This is a simiplified
;; version of [Fn [... -> ...]] or (IFn [... -> ...])
;; If you try the *Fn* version you'll get an error
;; https://groups.google.com/forum/#!topic/clojure-core-typed/0zBMCwS4JHI
;; The version with IFn on the other hand will also work. Not sure what is exactly
;; the difference.


