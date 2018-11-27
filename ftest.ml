open Graph
open Fordfulkerson

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;



  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  in

  (* Open file *)
  let graph = Gfile.from_file infile in

  let graphTransform = map graph (fun x -> int_of_string x) in

  let chemin = find_path graphTransform [] _source _sink in
	let flot = minimal_stream graphTransform chemin in
	Printf.printf "%d\n" flot;
	
	print_graph_path chemin ;

  (* Rewrite the graph that has been read. *)
  let () = Gfile.write_file outfile (Graph.map graph (fun x -> string_of_int ((int_of_string x)))) in

    ()
