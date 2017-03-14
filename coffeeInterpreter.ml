exception LookupError;;
exception TypeError;;
exception StuckTerm;;
exception SomeWeirdType;;

open Language
open Str

(* Language Types *)
type coffeeType = BoolType | IntType | StringType | SetType | VoidType

(* Language Grammar *)
type coffeeTerm = 
	  TmProgram of coffeeTerm list
	| TmInt of int 
	| TmBool of bool 
	| TmString of string 
	| TmSetLiteral of string list
	| TmSet of Language.t
	| TmVar of string
	| TmAssign of coffeeType * string * coffeeTerm 
	| TmLessThan of coffeeTerm * coffeeTerm
	| TmGreaterThan of coffeeTerm * coffeeTerm
	| TmEqualTo of coffeeTerm * coffeeTerm
	| TmNot of coffeeTerm
	| TmAnd of coffeeTerm * coffeeTerm
	| TmOr of coffeeTerm * coffeeTerm
	| TmIfElse of coffeeTerm * coffeeTerm list * coffeeTerm list
	| TmIf of coffeeTerm * coffeeTerm list
	| TmWhile of coffeeTerm * coffeeTerm list
	| TmPlus of coffeeTerm * coffeeTerm
	| TmMinus of coffeeTerm * coffeeTerm
	| TmMult of coffeeTerm * coffeeTerm
	| TmDiv of coffeeTerm * coffeeTerm
	| TmUnion of coffeeTerm * coffeeTerm
	| TmDifference of coffeeTerm * coffeeTerm
	| TmMemberOf of coffeeTerm * coffeeTerm
	| TmConcat of coffeeTerm * coffeeTerm
	| TmIntersect of coffeeTerm * coffeeTerm
	| TmDisplay of coffeeTerm * coffeeTerm
	
(* START OF TERRIFYING ENVIRONMENT STUFF*)


(* Type of Environments *)
type 'a context = Env of (string * 'a) list 
type typeContext = coffeeType context 
type valContext = coffeeTerm context

(* Function to look up the type of a string name variable in a type environment *)
let rec lookup env str = match !env with 
   Env [] -> raise LookupError  
  |Env ((name,thing) :: gs) -> 
        ( 
          match (name = str) with 
            true -> thing
           |false -> lookup (ref (Env (gs))) str 
	)
;;

(* Function to add an extra entry in to an environment *)
let addBinding env str thing = match !env with 
      Env(gs) -> env := Env ( (str, thing) :: gs ); env ;;
	  
	  
(* END OF TERRIFYING ENVIRONMENT STUFF *)
		
(* Type Checker *) 
(* Hacked a little bit: hasn't received inputs yet so K is assumed int and Ln is assumed set*)
let rec typeOf env e = match e with 
    TmProgram (l) -> 
		(match l with
			[] -> VoidType
			| hd :: tl -> let f = env in let _ = (typeOf f hd) in (typeOf f (TmProgram tl))
			| _ -> raise TypeError)
  | TmBool (b) -> BoolType
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
  | TmNot (e1) ->
		(match (typeOf env e1) with
			BoolType -> BoolType
			| _ -> raise TypeError)
  | TmAnd (e1, e2) ->
		(match (typeOf env e1), (typeOf env e2) with
			BoolType, BoolType -> BoolType
			| _ -> raise TypeError)
  | TmOr (e1, e2) ->
		(match (typeOf env e1), (typeOf env e2) with
			BoolType, BoolType -> BoolType
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
  | TmVar (v) -> lookup env v
  | TmWhile (e1, e2) -> 
		(match (typeOf env e1), e2 with
			BoolType, hd :: tl -> (typeOf env (TmProgram e2))
			| _ -> raise TypeError)
  | TmSet (s) -> SetType 
  | TmSetLiteral (s) -> SetType
  | TmAssign (varType, var, value) -> 
		(let type1 = typeOf (addBinding env var varType) value in
			(match (type1 = varType) with
				true -> type1
			  | false -> raise TypeError))
  | TmIf (b, v1) -> 
  		(match (typeOf env b), v1 with
  			BoolType, h1 :: t1 ->
  				(if(typeOf env (TmProgram v1) == VoidType) then VoidType else raise TypeError)
  		| _ -> raise TypeError)
  | TmIfElse (b, v1, v2) -> 
		(match (typeOf env b), v1, v2 with 
			BoolType, h1 :: t1, h2 :: t2 ->
				(if (typeOf env (TmProgram v1) == VoidType) && (typeOf env (TmProgram v2) == VoidType) then VoidType else raise TypeError)
		  | _ -> raise TypeError)
  | TmUnion (s1, s2) ->
		(match (typeOf env s1), (typeOf env s2) with
			SetType, SetType -> SetType
			| _ -> raise TypeError)
  | TmDifference (s1, s2) ->
		(match (typeOf env s1), (typeOf env s2) with
			SetType, SetType -> SetType
			| _ -> raise TypeError)
  | TmConcat (s1, s2) ->
		(match (typeOf env s1), (typeOf env s2) with
			SetType, SetType -> SetType
			| _ -> raise TypeError)
  | TmIntersect (s1, s2) ->
		(match (typeOf env s1), (typeOf env s2) with
			SetType, SetType -> SetType
			| _ -> raise TypeError)
  | TmMemberOf (e, s) -> 
		(match (typeOf env e), (typeOf env s) with
			StringType, SetType -> BoolType
			| _ -> raise TypeError)
  | TmDisplay (s, i) ->
		(match (typeOf env s), (typeOf env i) with
			SetType, IntType -> VoidType
			| _ -> raise TypeError)
;;

let typeProg typeEnv e = typeOf (ref (Env typeEnv)) e ;;

(*Evaluator*)

let rec isValue e = match e with 
  | TmInt(i) -> true
  | TmBool(b) -> true 
  | TmSet(s) -> true
  | TmString(s) -> true
  | _ -> false 
;;

let rec bigEval env e = match e with 
	TmProgram (sl) -> 
		(match sl with
			[] -> e
		  | h :: t -> let _ = bigEval env h in bigEval env (TmProgram t))
  | TmVar(x) -> lookup env x
  | e when (isValue(e)) -> e
  | TmAssign (varType, var, value) -> let v1 = bigEval env value in
										(addBinding env var v1); e
(*EQUALITY*)
  | TmEqualTo (e1, e2) -> 	let v1 = bigEval env e1 in 
								let v2 = bigEval env e2 in
									(match (v1, v2) with
										(TmBool (b1), TmBool (b2)) -> TmBool(b1 == b2)
									  | (TmInt (i1), TmInt (i2)) -> TmBool(i1 == i2)
									  | (TmString (s1), TmString (s2)) -> TmBool(s1 == s2)
									  | _ -> raise StuckTerm)
  | TmGreaterThan (e1,e2) -> let v1 = bigEval env e1 in 
								let v2 = bigEval env e2 in
									(match (v1,v2) with 
										(TmInt(i1), TmInt(i2)) -> TmBool(i1 > i2) 
									  | _ -> raise StuckTerm)									  
  | TmLessThan (e1,e2) -> let v1 = bigEval env e1 in 
							let v2 = bigEval env e2 in
								(match (v1,v2) with 
									(TmInt(i1), TmInt(i2)) -> TmBool(i1 < i2) 
								  | _ -> raise StuckTerm)
(*BOOLEAN SHIT!!!*)
  | TmNot (e1) -> let v1 = bigEval env e1 in
					(match (v1) with
						(TmBool b) -> TmBool (not b)
						| _ -> raise StuckTerm)
  | TmAnd (e1, e2) -> let v1 = bigEval env e1 in
						let v2 = bigEval env e2 in
							(match (v1, v2) with
								(TmBool (b1)), (TmBool (b2)) -> TmBool(b1 && b2)
							  | _ -> raise StuckTerm)
  | TmOr (e1, e2) -> let v1 = bigEval env e1 in
						let v2 = bigEval env e2 in
							(match (v1, v2) with
								(TmBool (b1)), (TmBool (b2)) -> TmBool(b1 || b2)
							  | _ -> raise StuckTerm)
(*ARITHMETIC (and concatenation)*)
  | TmPlus(e1,e2) -> let v1 = bigEval env e1 in 
						let v2 = bigEval env e2 in
                            (match (v1,v2) with 
								(TmInt(i1), TmInt(i2)) -> TmInt(i1 + i2) 
							  | (TmString(s1), TmString(s2)) -> TmString(s1 ^ s2)
							  | _ -> raise StuckTerm)
  | TmMinus(e1,e2) -> let v1 = bigEval env e1 in 
						let v2 = bigEval env e2 in
                            (match (v1,v2) with 
								(TmInt(i1), TmInt(i2)) -> TmInt(i1 - i2) 
							  | _ -> raise StuckTerm)
  | TmMult(e1,e2) -> let v1 = bigEval env e1 in 
						let v2 = bigEval env e2 in
                            (match (v1,v2) with 
								(TmInt(i1), TmInt(i2)) -> TmInt(i1 * i2) 
							  | _ -> raise StuckTerm)
  | TmDiv(e1,e2) -> let v1 = bigEval env e1 in 
						let v2 = bigEval env e2 in
                            (match (v1,v2) with 
								(TmInt(i1), TmInt(i2)) -> TmInt(i1 / i2) 
							  | _ -> raise StuckTerm)
(*CONTROL FLOW*)					
  |  TmIf(b,e1) -> let bv = bigEval env b in (match bv with 
                                            (TmBool true) -> bigEval env (TmProgram e1) 
                                          | (TmBool false) -> e 
                                          | _ -> raise StuckTerm)		  
  |  TmIfElse(b,e1,e2) -> let bv = bigEval env b in (match bv with 
                                            (TmBool true) -> bigEval env (TmProgram e1) 
                                          | (TmBool false) -> bigEval env (TmProgram e2) 
                                          | _ -> raise StuckTerm)
  | TmWhile(b, p) -> let bv = bigEval env b in (match bv with
											(TmBool true) -> let _ = bigEval env (TmProgram p) in bigEval env e
								 		  | (TmBool false) -> e
										  | _ -> raise StuckTerm)
(*SET OPERATIONS*)
  | TmSetLiteral (e1) -> TmSet (of_list_program e1)
  | TmUnion (e1, e2) -> let v1 = bigEval env e1 in
							let v2 = bigEval env e2 in 
								(match (v1, v2) with
									(TmSet s1, TmSet s2) -> TmSet(Language.union s1 s2)
								  | _ -> raise StuckTerm)
  | TmDifference (e1, e2) -> let v1 = bigEval env e1 in
							let v2 = bigEval env e2 in 
								(match (v1, v2) with
									(TmSet s1, TmSet s2) -> TmSet(Language.diff s1 s2)
								  | _ -> raise StuckTerm)
  | TmIntersect (e1, e2) -> let v1 = bigEval env e1 in
							let v2 = bigEval env e2 in 
								(match (v1, v2) with
									(TmSet s1, TmSet s2) -> TmSet(Language.inter s1 s2)
								  | _ -> raise StuckTerm)
  | TmMemberOf (e1, e2) -> let v1 = bigEval env e1 in
							let v2 = bigEval env e2 in
								(match (v1, v2) with
									(TmString el, TmSet s) -> TmBool (Language.mem el s)
								  | _ -> raise StuckTerm)
  | TmConcat (e1, e2) -> let v1 = bigEval env e1 in
							let v2 = bigEval env e2 in
								(match (v1, v2) with
									(TmSet s1, TmSet s2) -> TmSet (concat s1 s2)
								  | _ -> raise StuckTerm)
  | TmDisplay (e1, e2) -> let v1 = bigEval env e1 in
							let v2 = bigEval env e2 in
								(match (v1, v2) with
									(TmSet s, TmInt i) -> print_string (display s i); e
								  | _ -> raise StuckTerm)
;;

let eval initialEnv e = bigEval (ref (Env initialEnv)) e;;

(*PRINTING FINAL VALUE*)
let print_res res = match res with
    | (TmInt i) -> print_int i ; print_string " : Int" 
    | (TmBool b) -> print_string (if b then "true" else "false") ; print_string " : Bool"
    | (TmString s) -> print_string s
    | _ -> raise SomeWeirdType;; 