## contant types

Type := nil
Type := true
Type := false

## value type.
Type := (Value 5)


## intersection / union
Type := (U Type Type ...)
Type := (I Type Type ...)
Type := (Difference Type1 Type2)

## Function
Type := (Fn [ Type Type ... -  > Type]
             ....next arity...)


## Maps, vectors, aka 'prismatic schema'
Type := (HMap :mandatory {:key Type ...})
Type := (HVec [Type ...])

## syntax for non-complete HMaps and HVecs, quicker to write like prismatic
Type := '[Type ...]
Type := '{:a Num, :b Str}'

## Lists, seq.
Type := (Seq* [Type ...])
Type := (List* [Type ...])

## Polymorphic types, i.e. generics

Type := (All [x y z ...] Type)

## Recursive Types, i.e. XML
Type := (Rec [x] Type)

## symbol
Type := x 

Type := (Var  () (Value 1) () .....)
