
(* Language Types *)
type CoffeeType = IntType | BoolType | StringType | SetType

(* Language Grammer *)
type CoffeeTerm = 
	| TmInt of int 
	| TmBool of bool 
	| TmString of string 
	| TmSet of set
	| TmAssign of string * CoffeeTerm 
	| TmLessThan of CoffeeTerm * CoffeeTerm
	| TmGreaterThan of CoffeeTerm * CoffeeTerm
	| TmEqualTo of CoffeeTerm * CoffeeTerm
	| TmIf of CoffeeTerm * CoffeeTerm * CoffeeTerm
	| TmWhile of CoffeeTerm * CoffeeTerm
	| TmPlus of CoffeeTerm * CoffeeTerm
	| TmMinus of CoffeeTerm * CoffeeTerm
	| TmMult of CoffeeTerm * CoffeeTerm
	| TmDiv of CoffeeTerm * CoffeeTerm
	| TmUnion of CoffeeTerm * CoffeeTerm
	| TmDifference of CoffeeTerm * CoffeeTerm
	| TmMemberOf of CoffeeTerm * CoffeeTerm
	| TmConcat of CoffeeTerm * CoffeeTerm
	| TmIntersect of CoffeeTerm * CoffeeTerm