exception LookupError;;

(* Language Types *)
type coffeeType = BoolType | IntType | StringType | SetType

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
	
(* Type Checker *) 
let rec typeOf env e = match e with 
    TmBool (b) -> BoolType
  | TmEqualTo (e1, e2) -> BoolType
  | TmInt (i) -> IntType
  | TmGreaterThan (e1, e2) ->
		(match (typeOf env e1), (typeOf env e2) with
			IntType, IntType -> BoolType
			| _ -> raise TypeError)
  | TmLessThan (e1, e2) -> 
		(match (typeOf env e1), (typeOf env e2) with 
			IntType, IntType -> BoolType
			| _ -> raise TypeError)
  | TmPlus (e1, e2) -> 
		(match (typeOf env e1), (typeOf env e2) with 
            IntType, IntType -> IntType
			| StringType, StringType -> StringType
            |_ -> raise TypeError)
  | TmMinus (e1, e2) -> 
		(match (typeOf env e1), (typeOf env e2) with 
            IntType, IntType -> IntType
            |_ -> raise TypeError)
  | TmMult (e1, e2) -> 
		(match (typeOf env e1), (typeOf env e2) with 
            IntType, IntType -> IntType
            |_ -> raise TypeError)
  | TmDiv (e1, e2) -> 
		(match (typeOf env e1), (typeOf env e2) with 
            IntType, IntType -> IntType
            | _ -> raise TypeError)
  | TmString (s) -> StringType
  | TmVar (v) ->  (try lookup env v with LookupError -> raise TypeError)
  | TmWhile (e1, e2) -> 
		(match (typeOf env e1), (typeOf env e2) with
			BoolType, VoidType -> VoidType
			| _ -> raise TypeError)
;;

(* TERRIFYING ENVIRONMENT STUFF BELOW*)


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