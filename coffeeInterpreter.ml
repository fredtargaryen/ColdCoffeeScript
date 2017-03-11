exception LookupError;;
exception TypeError;;
exception StuckTerm;;
exception SomeWeirdType;;

(* Language Types *)
type coffeeType = BoolType | IntType | StringType | SetType | VoidType

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
	
(* START OF TERRIFYING ENVIRONMENT STUFF*)


(* Type of Environments *)
type 'a context = Env of (string * 'a) list 
type typeContext = coffeeType context 
type valContext = coffeeTerm context

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

(* Function to add an extra entry in to an environment *)
let addBinding env str thing = match env with 
      Env(gs) -> Env ( (str, thing) :: gs ) ;;
	  
	  
(* END OF TERRIFYING ENVIRONMENT STUFF *)

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

  (* This is my attempt at a while loop type check. Please review
  	  it incase it actually works :) (which is unlikely probably) *)
  (*|TmWhile(e1, e2) -> (
		let ty1 = typeOf env e1 in
			match ty1 with
				BoolType -> typeOf env e2

		|_ raise TypeError
	)*)


  | TmWhile (e1, e2) -> 
		(match (typeOf env e1), (typeOf env e2) with
			BoolType, VoidType -> VoidType
			| _ -> raise TypeError);;
			
let typeProg e = typeOf (Env []) e ;;

(*Evaluator*)


let rec isValue e = match e with 
  | TmInt(i) -> true
  | TmBool(b) -> true 
  (*| TmSet(x,tT,e') -> true*)
  | TmString(s) -> true
  | _ -> false 
;;

let rec bigEval e = match e with 
  |  (TmVar x) -> raise StuckTerm 
  |  e when (isValue(e)) -> e
  |  TmEqualTo (e1, e2) -> 	let v1 = bigEval e1 in 
								let v2 = bigEval e2 in
									(match (v1, v2) with
										(TmBool (b1), TmBool (b2)) -> TmBool(b1 == b2)
									  | (TmInt (i1), TmInt (i2)) -> TmBool(i1 == i2)
									  | (TmString (s1), TmString (s2)) -> TmBool(s1 == s2)
									  | _ -> raise StuckTerm)
  | TmGreaterThan (e1,e2) -> let v1 = bigEval e1 in 
								let v2 = bigEval e2 in
									(match (v1,v2) with 
										(TmInt(i1), TmInt(i2)) -> TmBool(i1 > i2) 
									  | _ -> raise StuckTerm)									  
  | TmLessThan(e1,e2) -> let v1 = bigEval e1 in 
							let v2 = bigEval e2 in
								(match (v1,v2) with 
									(TmInt(i1), TmInt(i2)) -> TmBool(i1 < i2) 
								  | _ -> raise StuckTerm)
  | TmPlus(e1,e2) -> let v1 = bigEval e1 in 
						let v2 = bigEval e2 in
                            (match (v1,v2) with 
								(TmInt(i1), TmInt(i2)) -> TmInt(i1 + i2) 
							  | (TmString(s1), TmString(s2)) -> TmString(s1 ^ s2)
							  | _ -> raise StuckTerm)
  | TmMinus(e1,e2) -> let v1 = bigEval e1 in 
						let v2 = bigEval e2 in
                            (match (v1,v2) with 
								(TmInt(i1), TmInt(i2)) -> TmInt(i1 - i2) 
							  | _ -> raise StuckTerm)
  | TmMult(e1,e2) -> let v1 = bigEval e1 in 
						let v2 = bigEval e2 in
                            (match (v1,v2) with 
								(TmInt(i1), TmInt(i2)) -> TmInt(i1 * i2) 
							  | _ -> raise StuckTerm)
  | TmDiv(e1,e2) -> let v1 = bigEval e1 in 
						let v2 = bigEval e2 in
                            (match (v1,v2) with 
								(TmInt(i1), TmInt(i2)) -> TmInt(i1 / i2) 
							  | _ -> raise StuckTerm)
(*  |  TmIf(b,e1,e2) -> let bv = bigEval b in (match bv with *)
                                           (*|  (TmBool(true)) -> bigEval e1*) 
                                           (*|  (TmBool(false)) -> bigEval e2*) 
                                           (*| _ -> raise StuckTerm*) 
                                           (*)*)
(*  |   TmLet(x,tT,e1,e2) -> let v = bigEval e1 in (bigEval (subst v x e2))*)
(*  |   TmApp(TmAbs(x,tT,e1), e2) -> let v = bigEval e2 in (bigEval (subst v x e1))*)
;;

let print_res res = match res with
    | (TmInt i) -> print_int i ; print_string " : Int" 
    | (TmBool b) -> print_string (if b then "true" else "false") ; print_string " : Bool"
    | (TmString s) -> print_string s
    | _ -> raise SomeWeirdType;;