open Graph
open Fordfulkerson

let () =

  if Array.length Sys.argv <> 6 then
    begin
      Printf.printf "\nUsage: %s infile source sink demand outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;



  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(5)

  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  and demand = Sys.argv.(4)
  in

  (* Open file *)
  let graph = Gfile.from_file infile in

  let graphTransform = map graph (fun x -> int_of_string x) in
  let chemin = find_path graphTransform [] _source _sink in
  match chemin with 
  	| None -> assert false
  	| Some path -> let (graph2, flotFinal) = fordfulkerson graphTransform chemin _source _sink
  in
  Printf.printf "Flot final maximal : %d\n" flotFinal;
  if int_of_string demand <= flotFinal then Printf.printf "La demande du destinataire peut être satisfaite.\n" 
else Printf.printf "La demande du destinataire ne peut pas être satisfaite.\n";
  let finalgraph = map graph2 (fun x -> string_of_int x) in

  (*let flot = minimal_stream graphTransform chemin in
  let graph2 = build_difference_graph graphTransform chemin flot in
  Printf.printf "flot %d%!" flot;*)
	
	Gfile.export outfile finalgraph;;
	

  (* Rewrite the graph that has been read. *)
  (*let () = Gfile.write_file outfile (Graph.map graph (fun x -> string_of_int ((int_of_string x)))) in*)

