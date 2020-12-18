module Domain

type Exp = Plus | Mult | Nr of int64 | Exp of Exp list 
type Expressions = Exp list 