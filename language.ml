open Set

module String = struct
	type t = string
	let rec compare s1 s2 =
		match s1, s2 with
			(* Empty strings. Lowest value of all *)
		    ":", ":" -> 0
		  | ":", _ -> -1
		  | _, ":" -> 1
			(* Prefixes of longer words have shorter value *)
		  | "", "" -> 0
		  | "", _ -> -1
		  | _, "" -> 1
		  | s1, s2 -> let score = (Pervasives.compare s1.[0] s2.[0]) in
				(if score == 0 then 
					(compare 
						(String.sub s1 1 (String.length s1 - 1))
						(String.sub s2 1 (String.length s2 - 1)))
					else score)
end
		  
module Language = Set.Make(String);;

