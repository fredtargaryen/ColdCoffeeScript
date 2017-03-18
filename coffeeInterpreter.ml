exception LookupError;;
exception DecafError of string;;
exception ColdCoffeeError of string;;

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
	| TmSetLiteral of coffeeTerm list
	| TmSetLanguageBuild of coffeeTerm list * Language.t
	| TmSet of Language.t
	| TmVar of string
	| TmAssign of coffeeType * string * coffeeTerm 
	| TmReAssign of string * coffeeTerm
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
let addBinding env str thing = 
	match !env with 
      Env(gs) -> env := Env ( (str, thing) :: gs ); env;;
	  
let addBinding_unit env str thing = 
	match !env with 
      Env(gs) -> env := Env ( (str, thing) :: gs ); ();;

(* END OF TERRIFYING ENVIRONMENT STUFF *)

(*Convert a type to a string representation*)	
let typeToString t = 
	match t with
		  IntType -> "int"
		| BoolType -> "bool"
		| StringType ->	"string"
		| SetType -> "set"
		| VoidType -> "void"   		
	;;
	
(*Ways to fail*)
let decaf1 e1 r1 = 
	raise (DecafError ("What is 'appening? I ordered a "^typeToString e1^" but I received a "^typeToString r1^"!"));;

let decaf2 e1 e2 r1 r2 = 
	raise (DecafError ("What is 'appening? I ordered a "^typeToString e1^" and a "^typeToString e2^" but I received a "^typeToString r1^" and a "^typeToString r2^"!"));;
	
let decaf3 e1 e2 e3 r1 r2 r3 = 
	raise (DecafError ("What is 'appening? I ordered a "^typeToString e1^", a "^typeToString e2^" and a "^typeToString e3^", but I received a "^typeToString r1^
						", a "^typeToString r2^" and a "^typeToString r3^"!"));;
		
(* Type Checker *) 
let rec typeOf env e = match e with 
    TmProgram (l) -> 
		(match l with
			[] -> VoidType
			| hd :: tl -> let f = env in let _ = (typeOf f hd) in (typeOf f (TmProgram tl)))
  | TmBool (b) -> BoolType
  | TmEqualTo (e1, e2) -> BoolType
  | TmInt (i) -> IntType
  | TmGreaterThan (e1, e2) ->
		(match (typeOf env e1), (typeOf env e2) with
			IntType, IntType -> BoolType
			| i, j -> decaf2 i j IntType IntType)
  | TmLessThan (e1, e2) -> 
		(match (typeOf env e1), (typeOf env e2) with 
			IntType, IntType -> BoolType
			| i, j -> decaf2 i j IntType IntType)
  | TmNot (e1) ->
		(match (typeOf env e1) with
			BoolType -> BoolType
			| t -> decaf1 t BoolType)
  | TmAnd (e1, e2) ->
		(match (typeOf env e1), (typeOf env e2) with
			BoolType, BoolType -> BoolType
			| i, j -> decaf2 i j BoolType BoolType)
  | TmOr (e1, e2) ->
		(match (typeOf env e1), (typeOf env e2) with
			BoolType, BoolType -> BoolType
			| i, j -> decaf2 i j BoolType BoolType)
  | TmPlus (e1, e2) -> 
		(match (typeOf env e1), (typeOf env e2) with 
            IntType, IntType -> IntType
			| StringType, StringType -> StringType
            | i, j -> raise (DecafError ("What is 'appening? I ordered two ints or two strings, but I received a "^typeToString i^" and a "^typeToString j^"!")))
  | TmMinus (e1, e2) -> 
		(match (typeOf env e1), (typeOf env e2) with 
            IntType, IntType -> IntType
            | i, j -> decaf2 i j IntType IntType)
  | TmMult (e1, e2) -> 
		(match (typeOf env e1), (typeOf env e2) with 
            IntType, IntType -> IntType
            | i, j -> decaf2 i j IntType IntType)
  | TmDiv (e1, e2) -> 
		(match (typeOf env e1), (typeOf env e2) with 
            IntType, IntType -> IntType
            | i, j -> decaf2 i j IntType IntType)
  | TmString (s) -> StringType
  | TmVar (v) -> lookup env v
  | TmWhile (e1, e2) -> 
		(match (typeOf env e1), e2 with
			BoolType, hd :: tl -> (typeOf env (TmProgram e2))
			| i, j -> raise (DecafError ("What is 'appening? I ordered a boolean and a list but I received a "^typeToString i^" and a non-list value!")))
  | TmSet (s) -> SetType 
  | TmSetLiteral (s) -> 
						(match s with
							[] -> SetType
						  | h :: t -> (match (typeOf env h) with
										StringType -> (typeOf env (TmSetLiteral t))
									  | _ -> raise (DecafError ("What is 'appening? You are trying-a to pour a non-string into a new set!"))))
  | TmSetLanguageBuild _ -> VoidType (*Not used in type checker*)
  | TmAssign (varType, var, value) -> 
		(let type1 = typeOf (addBinding env var varType) value in
			(match (type1 = varType) with
				true -> type1
			  | false -> raise (DecafError ("You are trying-a to pour a "^typeToString type1^" into a "^typeToString varType^"!"))))
  | TmReAssign (var, value) -> 
		(let typeVar = (lookup env var) in
			let typeVal = (typeOf env value) in
				match (typeVar = typeVal) with
				true -> typeVar
			  | false -> raise (DecafError ("You are trying-a to pour a "^typeToString typeVal^" into a "^typeToString typeVar^"!")))
  | TmIf (b, v1) -> 
  		(match (typeOf env b), v1 with
  			BoolType, h1 :: t1 ->
				(let progType = (typeOf env (TmProgram v1)) in
					(if(progType == VoidType) then VoidType else decaf1 progType VoidType))
  		| i, j -> raise (DecafError ("What is 'appening? I ordered a bool and a list, but I received a "^typeToString i^" and a non-list value!")))
  | TmIfElse (b, v1, v2) -> 
		(match (typeOf env b), v1, v2 with 
			BoolType, h1 :: t1, h2 :: t2 ->
				(if (typeOf env (TmProgram v1) == VoidType) && (typeOf env (TmProgram v2) == VoidType) then VoidType else raise (DecafError 
					"What is 'appening? I ordered two lists of statements but I did not receive them!"))
		  | a, b, c -> raise (DecafError ("What is 'appening? I ordered a bool, but I received a "^typeToString a^"!")))
  | TmUnion (s1, s2) ->
		(match (typeOf env s1), (typeOf env s2) with
			SetType, SetType -> SetType
			| a, b -> decaf2 a b SetType SetType)
  | TmDifference (s1, s2) ->
		(match (typeOf env s1), (typeOf env s2) with
			SetType, SetType -> SetType
			| a, b -> decaf2 a b SetType SetType)
  | TmConcat (s1, s2) ->
		(match (typeOf env s1), (typeOf env s2) with
			SetType, SetType -> SetType
			| a, b -> decaf2 a b SetType SetType)
  | TmIntersect (s1, s2) ->
		(match (typeOf env s1), (typeOf env s2) with
			SetType, SetType -> SetType
			| a, b -> decaf2 a b SetType SetType)
  | TmMemberOf (e, s) -> 
		(match (typeOf env e), (typeOf env s) with
			StringType, SetType -> BoolType
			| a, b -> decaf2 a b StringType SetType)
  | TmDisplay (s, i) ->
		(match (typeOf env s), (typeOf env i) with
			SetType, IntType -> VoidType
			| a, b -> decaf2 a b SetType IntType)
;;

let typeProg typeEnv e = typeOf (ref (Env typeEnv)) e ;;

(*Evaluator*)
let rec bigEval env e = match e with 
	TmProgram (sl) -> 
		(match sl with
			[] -> e
		  | h :: t -> let _ = bigEval env h in bigEval env (TmProgram t))
  | TmVar(x) -> lookup env x
  | TmInt(i) -> e
  | TmBool(b) -> e
  | TmString(s) -> e
  | TmSet(s) -> e
  | TmAssign (varType, var, value) -> let v1 = bigEval env value in
										(addBinding_unit env var v1); e
  | TmReAssign (var, value) -> let v1 = bigEval env value in
										(addBinding_unit env var v1); e
(*EQUALITY*)
  | TmEqualTo (e1, e2) -> 	let v1 = bigEval env e1 in 
								let v2 = bigEval env e2 in
									(match (v1, v2) with
										(TmBool (b1), TmBool (b2)) -> TmBool(b1 == b2)
									  | (TmInt (i1), TmInt (i2)) -> TmBool(i1 == i2)
									  | (TmString (s1), TmString (s2)) -> TmBool(s1 == s2)
									  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not compare values in equality")))
  | TmGreaterThan (e1,e2) -> let v1 = bigEval env e1 in 
								let v2 = bigEval env e2 in
									(match (v1,v2) with 
										(TmInt(i1), TmInt(i2)) -> TmBool(i1 > i2) 
									  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not compare values in greater than")))									  
  | TmLessThan (e1,e2) -> let v1 = bigEval env e1 in 
							let v2 = bigEval env e2 in
								(match (v1,v2) with 
									(TmInt(i1), TmInt(i2)) -> TmBool(i1 < i2) 
								  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not compare values in less than")))
(*BOOLEANS*)
  | TmNot (e1) -> let v1 = bigEval env e1 in
					(match (v1) with
						(TmBool b) -> TmBool (not b)
						| _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not not boolean")))
  | TmAnd (e1, e2) -> let v1 = bigEval env e1 in
						let v2 = bigEval env e2 in
							(match (v1, v2) with
								(TmBool (b1)), (TmBool (b2)) -> TmBool(b1 && b2)
							  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not take and of both booleans")))
  | TmOr (e1, e2) -> let v1 = bigEval env e1 in
						let v2 = bigEval env e2 in
							(match (v1, v2) with
								(TmBool (b1)), (TmBool (b2)) -> TmBool(b1 || b2)
							  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not take or of both booleans")))
(*ARITHMETIC (and concatenation)*)
  | TmPlus(e1,e2) -> let v1 = bigEval env e1 in 
						let v2 = bigEval env e2 in
                            (match (v1,v2) with 
								(TmInt(i1), TmInt(i2)) -> TmInt(i1 + i2) 
							  | (TmString(s1), TmString(s2)) -> TmString(CoffeeString.stringConcat s1 s2)
							  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not add integer/string to second integer/string")))
  | TmMinus(e1,e2) -> let v1 = bigEval env e1 in 
						let v2 = bigEval env e2 in
                            (match (v1,v2) with 
								(TmInt(i1), TmInt(i2)) -> TmInt(i1 - i2) 
							  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not substract integer from integer")))
  | TmMult(e1,e2) -> let v1 = bigEval env e1 in 
						let v2 = bigEval env e2 in
                            (match (v1,v2) with 
								(TmInt(i1), TmInt(i2)) -> TmInt(i1 * i2) 
							  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not multiply integer by integer")))
  | TmDiv(e1,e2) -> let v1 = bigEval env e1 in 
						let v2 = bigEval env e2 in
                            (match (v1,v2) with 
								(TmInt(i1), TmInt(i2)) -> TmInt(i1 / i2) 
							  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not divide integer by integer")))
(*CONTROL FLOW*)					
  |  TmIf(b,e1) -> let bv = bigEval env b in (match bv with 
                                            (TmBool true) -> bigEval env (TmProgram e1) 
                                          | (TmBool false) -> e 
                                          | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not evaluate if statement. Is your boolean conditional correct?")))		  
  |  TmIfElse(b,e1,e2) -> let bv = bigEval env b in (match bv with 
                                            (TmBool true) -> bigEval env (TmProgram e1) 
                                          | (TmBool false) -> bigEval env (TmProgram e2) 
                                          | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not eavluate if statement. Is your boolean conditional correct?")))
  | TmWhile(b, p) -> let bv = bigEval env b in (match bv with
											(TmBool true) -> let _ = bigEval env (TmProgram p) in bigEval env e
								 		  | (TmBool false) -> e
										  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not evaluate while statement. Is your boolean conditional correct?")))
(*SET OPERATIONS*)
	(*CoffeeTerm list*)
  | TmSetLiteral (e1) -> (match e1 with
							[] -> TmSet Language.empty
						  | h :: t -> (bigEval env (TmSetLanguageBuild (e1, Language.empty))))
  | TmSetLanguageBuild (exps, lang) -> 
				(match (exps, lang) with
					[], l -> TmSet lang
				  |	h :: t, l -> let head = (bigEval env h) in
								(match head with 
									TmString(s) -> (bigEval env (TmSetLanguageBuild (t, (Language.add s lang))))
								  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Invalid set literal"))))
  | TmUnion (e1, e2) -> let v1 = bigEval env e1 in
							let v2 = bigEval env e2 in 
								(match (v1, v2) with
									(TmSet s1, TmSet s2) -> TmSet(Language.union s1 s2)
								  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not take union of both sets")))
  | TmDifference (e1, e2) -> let v1 = bigEval env e1 in
							let v2 = bigEval env e2 in 
								(match (v1, v2) with
									(TmSet s1, TmSet s2) -> TmSet(Language.diff s1 s2)
								  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not take difference of both sets")))
  | TmIntersect (e1, e2) -> let v1 = bigEval env e1 in
							let v2 = bigEval env e2 in 
								(match (v1, v2) with
									(TmSet s1, TmSet s2) -> TmSet(Language.inter s1 s2)
								  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not take intersection of both sets")))
  | TmMemberOf (e1, e2) -> let v1 = bigEval env e1 in
							let v2 = bigEval env e2 in
								(match (v1, v2) with
									(TmString el, TmSet s) -> TmBool (Language.mem el s)
								  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not check if term is member of the set")))
  | TmConcat (e1, e2) -> let v1 = bigEval env e1 in
							let v2 = bigEval env e2 in
								(match (v1, v2) with
									(TmSet s1, TmSet s2) -> TmSet (concat s1 s2)
								  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not take concatenation of both sets")))
  | TmDisplay (e1, e2) -> let v1 = bigEval env e1 in
							let v2 = bigEval env e2 in
								(match (v1, v2) with
									(TmSet s, TmInt i) -> print_string (display s i); e
								  | _ -> raise (ColdCoffeeError ("What isa wrong? Your coffee as gonea cold!: Could not display set")))
;;

let eval initialEnv e = bigEval (ref (Env initialEnv)) e;;