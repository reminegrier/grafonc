open Graph

type path = id list

let rec find_path graph forbidden src dest = 
	if src = dest then Some forbidden
	else
		let arcs = out_arcs graph src 
		in
		let rec loop = function
				| (id, _) :: rest 	-> 
					begin match (find_path graph (src::forbidden) id dest) with
						| Some x -> Some x
						| None 	 -> loop rest
					end
				| [] 				-> None
		
		in loop arcs
;;
		


