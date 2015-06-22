# core.typed overview

## General Types

    * true, false
    * Any
    * Nothing
    * Nilable
    * AnyValue, Val, Value
    
## Function Types

  * Fn, IFn, TFn, 
  * I, U, Difference, 
  * Pred
  * Option
  * Id

## Primitive Types

  * Number
  * String
  * Num, AnyInteger
  * Keyword, Kw,
  * Symbol, Sym
  * Bool, Str
  * Namespace
  * Int

## Polymorphic
  * All
## Vector Types

  * Vec, AVec, HVec,
  * NonEmptyAVec, NonEmptyVec

## List Types

  * List

##  Sequence Types

  * Seqable, NonEmptySeqable, EmptySeqable
  * Sequential, HSequential
  * Seq, ASeq, HSeq
  * NonEmptySeq, NonEmptyASeq, 
  * NilableNonEmptySeq, NilableNonEmptASeq
  * SequentialSeq, SequentialSeqable
  * NonEmptyCount, ExtactCount, EmptyCount
  * NonEmptyLazySeq 
  * CountRange
  * Reversible
  
## Coll Types (== countable)

  * NonEmptyColl,Coll

## Map Types

  * Map, HMap
  * Assoc, Dissoc, Get
  
## Set Types

  * Set, SortedSet, HSet

## Record Types

## Var types

  * Var1, Var2

## Multi method Types
  * Multi

## Atoms, future and promiseses types
  
  * Atom1, Atom2
  * Future, BlockingDeref, Promise
  * Delay, Deref
  * Ref1, Ref2
  * Agent1, Agent2,
  
## Stack
  * Stack

## Exception
  *  ExInfo

## Hierarchy
  * Hiearchy

## Recursive Types

  * Rec

## Proxy types  
  * Proxy,  

## annotation fn's
ann, ann-datatype, ann-form, ann-interface, ann-nmany, ann-prerecord, ann-protocol, ann-record

## checking fn's
cf, check-form*, check-form-info, check-ns, check-ns-info

## def forms
declare-alias-kind, declare-datatypes, declare-names, declare-protocols, 
def, defalias, defn, defprotocol

##other forms
let, let-fn>, for, loop, ref, doseq

## debugging
envs, print-env, print-filterset,

## Generics
inst, inst-ctor

## Other 
atom, into-array>
load-if-needed, method-type, nilable-param, non-nil-return, overeride-constructor,
pfn>, pred,  ref, reset-caches, statistics, tc-ignore,
typed-deps, var-coverage, var>, warn-on-unannoated-vars
when-let-fail

envs

core.async: go, chan, buffer wrappers.


hole driven cool stuff


ann-form, atom, def, 
Type Examples



()

Type := nil
Type := true
Type := false
Type := (U Type Type ...)
Type := (I Type Type ...)
Type := (Value 5)
Type := (Fn [ Type Type ... -  > Type])
Type := (All [x y z ...] Type)
Type := (Rec [x] Type)
Type := (HMap :mandatory {:key Type ...})
Type := '{:a Num, :b Str}'
Type := (HVec [Type ...])
Type := (Seq* [Type ...])
Type := (List* [Type ...])
Type := '[Type ...]
Type := x



Type := (Var  () (Value 1) () .....)
