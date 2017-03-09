exception LookupError;;

(* Language Types *)
type CoffeeType = IntType | BoolType | StringType | SetType

(* Language Grammer *)
type CoffeeTerm = 
	| TmInt of int 
	| TmBool of bool 
	| TmString of string 
	| TmSet of SetType
	| TmVar of string
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


(* this function is taken from lab 5. I have reasons to believe that it may be useful *)
(* Function to look up the type of a string name variable in a type environment *)
let rec lookup env str = match env with 
   Env [] -> raise LookupError  
  |Env ((name,thing) :: gs) -> 
        ( 
          match (name = str) with 
            true -> thing
           |false -> lookup (Env (gs)) str 
	)
;;