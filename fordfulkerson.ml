open Graph

type path = id list
type id = string



let rec print_graph_path = function 
		| None 				-> Printf.printf "Pas de chemin\n"
		| Some [] 			-> Printf.printf "\n"
		| Some (x::[] ) 	-> Printf.printf "%s\n" x
		| Some (x::rest) 	-> Printf.printf "%s - " x ; print_graph_path (Some rest) 
;;


let rec find_path graph forbidden src dest = 
	if src = dest then Some (List.rev (src::forbidden))
	else
		let arcs = out_arcs graph src 
		in
		let rec loop = function
				| (id, _) :: rest 	-> 
					if (not (List.mem id forbidden)) then
						begin match (find_path graph (src::forbidden) id dest) with
							| Some x -> Some x
							| None 	 -> loop rest
						end
					else loop rest
				
				| [] 				-> None
		
		in loop arcs
;;
		


