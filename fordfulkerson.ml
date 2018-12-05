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

let minimal_stream graph path = 
	let rec loop path acu = match path with
	| [] | _ :: [] -> acu
	| x :: y :: rest -> 
		begin match (find_arc graph x y) with
			| None -> assert false
			| Some value -> if value < acu then loop (y :: rest) value else loop (y :: rest) acu
		end
	in match path with
		| None -> 0
		| Some x -> loop x max_int
;;


let add_n graph id outs =
	add_node graph id
;;

(*
let add_a graph id outs =
	match outs with
		| [] -> 
		| (id_dest, etiquette) :: rest -> add_arc graph id id_dest etiquette
;;*)

let add_arc_delta graph x y delta = 
	match (find_arc graph x y) with
		| None -> add_arc graph x y delta
		| Some lbl -> add_arc graph x y (lbl + delta)

;;

let build_difference_graph graph lepath flot_min =
	(* let graph1 = v_fold graph add_n empty_graph
	in let graph2 = v_fold graph add_a graph1 in *)
	let rec loop graph path = 
		match path with 
			| x :: [] -> graph
			| [] -> graph
			| x :: y :: rest -> let graph3 = add_arc_delta graph x y (-flot_min) in
								let graph4 = add_arc_delta graph3 y x flot_min in
								loop graph4 (y::rest)
	in 
		match lepath with 
			| None -> assert false
			| Some xPath -> loop graph xPath
;;
			


		


