(* Apply Ford-Fulkerson on Graphs*)

open Graph

(* A path is a list of nodes. *)
type path = id list

type id = string

(* find_path gr forbidden id1 id2 
 *   returns None if no path can be found.
 *   returns Some p if a path p from id1 to id2 has been found. 
 *
 *  forbidden is a list of forbidden nodes (they have already been visited)
 *)
val find_path: int graph -> id list -> id -> id -> path option

val print_graph_path: path option -> unit

val minimal_stream: int graph -> path option -> int

val add_arc_delta: int graph -> id -> id -> int -> int graph

val build_difference_graph: int graph -> path option ->  int -> int graph
