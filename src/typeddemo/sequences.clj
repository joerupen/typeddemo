(ns typeddemo.sequences
  (:require [clojure.core.typed :as t]))

;; Sequences
;; ---------------------------------------------------------------------------------------

;; There are many more options for sequences in core.typed.

;;Seqable, NonEmptySeqable, EmptySeqable
;;Sequential, HSequential
;;Seq, ASeq, HSeq
;;NonEmptySeq, NonEmptyASeq, 
;;NilableNonEmptySeq, NilableNonEmptASeq
;;SequentialSeq, SequentialSeqable
;;NonEmptyCount, ExtactCount, EmptyCount
;;NonEmptyLazySeq 
;;CountRange
;;Reversible

;; Similar to vectors we have heterogenous sequences where the types of the elements is
;; defined. The others sequences are just 'generics'. There are also special types such as
;; CountRange, ExactCount. Will give some examples for most of them in this worksheet.
;; It's intended to run at the repl.

;; NOTE: We're doing a lot of boring tests here to check which type is compatible
;; with which other type. In real life when your functions call other fn's that's
;; exactly what you need to be aware of. The easiest thing to test an incompatibilty
;; is to exercise it at the repl.


;; Sequable
;; --------------------------------------------------------------------------------

;; Sequable is the most basic Type. It's the type used in filter, reduce, map
;; For most of the other sequence types we have T :< Seqable

;; Is [] seqable?, yep
(t/cf (t/ann-form [] (t/Seqable t/Any)))

;; Is () seqable?, yep
(t/cf (t/ann-form () (t/Seqable t/Any)))

;; Is (range) seqable?, yep
(t/cf (t/ann-form (range) (t/Seqable t/Any)))

;; this list is nonempty seqable.
(t/cf (t/ann-form '(1 2 3) (t/NonEmptySeqable t/Num)))

;; this vec is nonempty seqable.
(t/cf (t/ann-form [1 2 3] (t/NonEmptySeqable t/Num)))

;; The following produces a Type with count=0.
(t/cf (t/ann-form [] (t/EmptySeqable t/Num)))

;; is it useful to have EmptySequable???
;; yep, the result type is inferred as nil.
(t/cf (first (t/ann-form [] (t/EmptySeqable t/Num))))

;; strings are seqable
(t/cf (t/ann-form "string" (t/Seqable t/Any)))
(t/cf (t/ann-form "string" (t/Seqable Character)))

;; a seq is sequential
(t/cf (t/ann-form (seq [])
                  (t/Option (t/Seqable t/Any))))

;; can we call rseq on a vector?
(t/cf (t/ann-form [1 2 3]
                  (t/Reversible t/Any)))

;; reversible is not seqable
(t/cf (t/ann-form (t/ann-form [1 2 3]
                              (t/Reversible t/Any))
                  (t/Seqable t/Any)))

;; Ok, seqable is very generic and the most basic Type of a sequence.
;; Everything we can reduce or map is seqable

;; Sequential
;; --------------------------------------------------------------------------------

;; Being sequential guarntees some ordering when we iterate through it. A map might e.g. change
;; its order as we assoc keys

;;is [] sequential?
(t/cf (t/ann-form [ 1 2 3] t/Sequential))

;;is {} sequential? no, since order is undefined
(t/cf (t/ann-form {} t/Sequential))

;;is () sequential? ok
(t/cf (t/ann-form '(1 2 3) t/Sequential))


;;also a byte array is sequential
(t/cf (t/ann-form (byte-array 10) t/Sequential))

;; is sequential seqable?
(t/cf (t/ann-form (t/ann-form [1 2 3]  (t/Sequential t/Any))
                  (t/Seqable t/Any)))

;; but sequential-seqable is seqable
(t/cf (t/ann-form (t/ann-form [1 2 3]  (t/SequentialSeqable t/Any))
                  (t/Seqable t/Any)))

;; strings are not sequential
(t/cf (t/ann-form "string" t/Sequential))

;; Seq
;; --------------------------------------------------------------------------------

;;Persistent collections are no seq's, but they are Sequable, hence they can return a seq.

;;is [] a Seq? no
(t/cf (t/ann-form [] (t/Seq t/Any)))

;; Why
(seq? []) ;; => false

;; vectors are seqable but are no seqs. So we need to call *seq* on a vector
;; to make it a sequence. Try this example. Notice your bug?
(t/cf (t/ann-form (seq (t/ann-form [] (t/Vec t/Any)))
                  (t/Seq t/Any)))

;; seq is not just making a sequence. It's also NOT making a sequence
;; when source is empty. This means it returns 'maybe' a sequence.
;; The previous example fails since we don't handle nil.

(t/cf (t/ann-form (seq (t/ann-form [] (t/Vec t/Any)))
                  (t/Option (t/Seq t/Any))))

;; if we want to ensure that we can make a sequence out of a vector, then it
;; should not be empty
(t/cf (t/ann-form (seq (t/ann-form [1] (t/NonEmptyVec t/Any)))
                  (t/Seq t/Any)))

;; lazy seq are seq's
(t/cf (t/ann-form (lazy-seq (range)) (t/Seq t/Num)))


;; this list is nonempty seqable.
(t/cf (t/ann-form '(1 2 3) (t/NonEmptySeqable t/Num)))

;; HSequential

;; 
(t/cf (t/ann-form [1 ] (t/HSequential [t/Num])))

;; (range) is not heterogenous-sequential, although similar.
(t/cf (t/ann-form (range) (t/HSequential [t/Num *])))

;; When is it useful to know the type of each element in a seq?
;; For instance fn's like first or second.

;; check them out:
(t/cf first)
(t/cf second)

;; Here, since [1 :qux 3 4 5] is HSequential, core.typed can infer that for the fn
;; *second* the result is Keyword!!!! pretty sweet.
(t/cf (second (t/ann-form [1 :qux 3 4 5]
                          (t/HSequential [t/Num t/Kw t/Num *]))))

;; ASeq
;; --------------------------------------------------------------------------------

;; From the docs: A sequential seq returned from clojure.core/seq

;; So what's the difference between Seq and ASeq ?
;; If you read the signatures of fn's like map, filter, butlast, you'll notice
;; that most fn's accept Seqable as input and return ASeq as output.
;; I assume that Seq is 'base type' and is used for general implementation of the ISeq interface.


;; ASeq :< Seq
(t/cf (t/ann-form (t/ann-form '(1) (t/ASeq t/Any))
                  (t/Seq t/Any)))

;; Seq !:< ASeq
(t/cf (t/ann-form (t/ann-form '(1) (t/Seq t/Any))
                  (t/ASeq t/Any)))



;; HSeq
;; -------------------------------------------------------------------------------

;; sorry, I haven't yet figured out how you can create an HSeq.
;; (t/cf (t/ann-form (list 1 )
;;                   (t/HSeq [t/Num])))

;; (t/cf (t/ann-form (seq [1 2 3])
;;                   (t/Option (t/HSeq [t/Num *]))))

;; (t/cf (t/ann-form '(1) (t/HSeq [t/Num])))

(t/cf cons)


;; ExactCount
;; --------------------------------------------------------------------------------

;; A type that know how many elements a Type has.

;; we an cast specific vectors to exactcount
(t/cf (t/ann-form [1 2] (t/ExactCount 2)))

;; But we cannot call a function expecting (Seqable Any) with just an exact count type.
(t/cf (t/ann-form (t/ann-form [1 2] (t/ExactCount 2))
                  (t/Seqable t/Any)))

;; it's more useful when you combine it.
(t/cf (t/ann-form (t/ann-form [1 2] (t/I (t/Seqable t/Any)
                                         (t/ExactCount 2)))
                  (t/Seqable t/Any)))


;; CountRange
;; --------------------------------------------------------------------------------

(t/cf (t/ann-form [1 2 3] (t/CountRange 3 4)))
(t/cf (t/ann-form [1 2 3 4] (t/CountRange 3 4)))
(t/cf (t/ann-form [1 2 3 4 5] (t/CountRange 3 4))) ;; error

;; core.typed knows when we get number
;; => 
(t/cf (->> (t/ann-form [1 2] 
                       (t/I (t/Seqable t/Num) (t/CountRange 2)))
            second))

;; now we get nil or number.
(t/cf (->> (t/ann-form [1 ]
                       (t/I (t/Seqable t/Num) (t/CountRange 1)))
            second))


;; More practical examples
;; --------------------------------------------------------------------------------

;; A function that accepts a non empty sequence of nums.

(t/cf (t/defn sum-it [x :- (t/NonEmptySeqable t/Num)] :- t/Num
        (reduce + 0 x)))

;; bug.
(t/cf (sum-it []))

;; ok.x
(t/cf (sum-it [1 2]))

(t/cf (t/defn sum-it [x :- (t/NonEmptySeqable t/Num)] :- t/Num
        (reduce + 0 x)))

;; binary tree. Note: i use t/Any here but we could better use a recursive type.
;; that is discussed in more-types.clj
(t/cf (t/defn make-node [l :- (t/U
                               t/Str
                               (t/I (t/Seqable t/Any)
                                        (t/CountRange 0 2)))
                         r :- (t/U
                               t/Str
                               (t/I (t/Seqable t/Any)
                                    (t/CountRange 0 2)))]
        :- (t/I (t/Seqable t/Any)
                (t/CountRange 0 2))

        [l r]
        ))

(t/cf (make-node "a" "b"))
(t/cf (make-node ["a" "b"] "c"))
(t/cf (make-node ["a" ] "c"))

;; ups.
(t/cf (make-node ["a" "b" "c"] "d")) 




;; try out some of the clojure.core sequence fn's to see what type they have

(t/cf associative?)
(t/cf sequential?) 
(t/cf empty?)
(t/cf every?) 
(t/cf contains?)
(t/cf distinct?)
(t/cf vector?)  
(t/cf coll?)    
(t/cf seq?)     
(t/cf list?)    
(t/cf map?)     
(t/cf set?)     
(t/cf count )
(t/cf some)
(t/cf conj)
(t/cf cons)
(t/cf into)
(t/cf concat)
(t/cf seq)
(t/cf sequence)
(t/cf repeat)
(t/cf replicate)
(t/cf range)
(t/cf repeatedly)
(t/cf iterate)
(t/cf lazy-seq) 
(t/cf lazy-cat) 
(t/cf cycle)
(t/cf interleave)
(t/cf interpose)
(t/cf sort)
(t/cf sort-by)
(t/cf reverse)
(t/cf flatten)
(t/cf first)
(t/cf second) 
(t/cf last)
(t/cf rest)
(t/cf next)
(t/cf nnext)
(t/cf fnext)
(t/cf nfirst)
(t/cf nth)
(t/cf nthnext)
(t/cf rand-nth)
(t/cf butlast)
(t/cf take)
(t/cf take-last)
(t/cf take-nth)
(t/cf take-while)
(t/cf drop)
(t/cf drop-last)
(t/cf drop-while)
(t/cf keep)
(t/cf cons)
(t/cf conj)
(t/cf concat)
(t/cf distinct)
(t/cf group-by)
(t/cf partition)
(t/cf partition-all)
(t/cf split-at)
(t/cf split-with)
(t/cf filter)
(t/cf remove)
(t/cf shuffle)
(t/cf dorun)
(t/cf map)
(t/cf map-indexed)
(t/cf mapcat)
(t/cf reduce)
(t/cf reductions)
(t/cf max-key)
(t/cf min-key)
