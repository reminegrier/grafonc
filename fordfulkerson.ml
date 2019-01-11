open Graph

type path = id list
type id = string

type retourAlgo = (int graph * int)


(* Fonction d'affichage d'un chemin *)
let rec print_graph_path = function 
		| None 				-> Printf.printf "Pas de chemin\n"
		| Some [] 			-> Printf.printf "\n"
		| Some (x::[] ) 	-> Printf.printf "%s\n" x
		| Some (x::rest) 	-> Printf.printf "%s - " x ; print_graph_path (Some rest) 
;;

(* Fonction pour trouver un chemin entre deux noeuds *)
(* forbidden correspond à une liste de noeuds déjà explorés, inutile de les retester
dans la recherche de chemin *)
let rec find_path graph forbidden src dest = 
	(* Condition d'arrêt de la récursivité *)
	if src = dest then Some (List.rev (src::forbidden))
	else
		let arcs = out_arcs graph src 
		in
		(* On boucle sur tous les arcs sortants a du noeud src afin de voir s'il existe
		un chemin entre la destination finale et la seconde extrémité de l'arc a *)
		let rec loop = function
				(* On considère qu'il est interdit/impossible de passer par un arc
				ayant une capacité nulle *)
				| (id, 0) :: rest -> loop rest
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

(* Fonction retournant la capacité minimale rencontrée lors du parcours des arcs 
d'un chemin *)
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


(*let add_n graph id outs =
	add_node graph id
;;*)

(*
let add_a graph id outs =
	match outs with
		| [] -> 
		| (id_dest, etiquette) :: rest -> add_arc graph id id_dest etiquette
;;*)

(* Fonction permettant d'ajouter/modifier les arcs parcourus dans le chemin
du graphe d'écart *)
let add_arc_delta graph x y delta = 
	match (find_arc graph x y) with
		| None -> add_arc graph x y delta
		| Some lbl -> add_arc graph x y (lbl + delta)

;;

(* Fonction créant le graphe d'écart d'un graphe associé à un chemin *)
let build_difference_graph graph lepath flot_min =
	(* let graph1 = v_fold graph add_n empty_graph
	in let graph2 = v_fold graph add_a graph1 in *)
	let rec loop graph path = 
		match path with 
			| x :: [] -> graph
			| [] -> graph
			(* Modification de la valeur de la capacité de l'arc allant de x à y 
			ainsi que celle de l'arc allant de y à x 
			Si ce dernier n'existe pas, on l'ajoute au graphe *)
			| x :: y :: rest -> let graph3 = add_arc_delta graph x y (-flot_min) in
								let graph4 = add_arc_delta graph3 y x flot_min in
								loop graph4 (y::rest)
	in 
		match lepath with 
			| None -> assert false
			| Some xPath -> loop graph xPath
;;

(* Fonction faisant tourner l'algorithme de Ford Fulkerson entre les noeuds source et sink *)
(* lepath ne peut être vide, cas traité dans ftest.ml *)
let fordfulkerson graph lepath source sink =
	let rec loop (graph, flot) path iteration =  
		match path with 
			| None -> (graph, flot)
			| Some chemin -> 	let flotMin = minimal_stream graph path in
								let graph2 = build_difference_graph graph path flotMin in
  								let path2 = find_path graph2 [] source sink in
  								Printf.printf "Itération n°%d :\n" iteration;
  								print_graph_path path;
  								Printf.printf "Flot minimal : %d\n" flotMin;
								loop (graph2, flot+flotMin) path2 (iteration+1)
	in loop (graph, 0) lepath 1
;;

(*let flotfinal graph sink =
	let arcs = out_arcs graph sink in
	let rec loop arcs total = 
		match arcs with
			| (id, x) :: rest 	-> loop rest (total+x)
			| [] 				-> total
		
	in loop arcs 0 *)



		


