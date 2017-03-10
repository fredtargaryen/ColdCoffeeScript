exception LookupError;;

(* Language Types *)
type coffeeType = IntType | BoolType | StringType | SetType

(* Language Grammar *)
type coffeeTerm = 
	  TmInt of int 
	| TmBool of bool 
	| TmString of string 
	| TmSet of coffeeTerm
	| TmVar of string
	| TmUnit of unit
	| TmAssign of string * coffeeTerm 
	| TmLessThan of coffeeTerm * coffeeTerm
	| TmGreaterThan of coffeeTerm * coffeeTerm
	| TmEqualTo of coffeeTerm * coffeeTerm
	| TmIf of coffeeTerm * coffeeTerm * coffeeTerm
	| TmWhile of coffeeTerm * coffeeTerm
	| TmPlus of coffeeTerm * coffeeTerm
	| TmMinus of coffeeTerm * coffeeTerm
	| TmMult of coffeeTerm * coffeeTerm
	| TmDiv of coffeeTerm * coffeeTerm
	| TmUnion of coffeeTerm * coffeeTerm
	| TmDifference of coffeeTerm * coffeeTerm
	| TmMemberOf of coffeeTerm * coffeeTerm
	| TmConcat of coffeeTerm * coffeeTerm
	| TmIntersect of coffeeTerm * coffeeTerm


(* this function is taken from lab 5. I have reasons to believe that it may be useful *)
(* Function to look up the type of a string name variable in a type environment *)
(*(let rec lookup env str = match env with 
   )   Env [] -> raise LookupError  
  |Env ((name,thing) :: gs) -> 
        ( 
          match (name = str) with 
            true -> thing
           |false -> lookup (Env (gs)) str 
	)*)
;;