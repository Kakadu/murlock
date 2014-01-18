type proving_type = Classic | Liberalised;;
type deriv_tree_type = Brief | Full;;
type prove_config = {
  mutable prove_type: proving_type;
  mutable euristics: bool;
  mutable need_temp_unif: bool;
  mutable max_uni_count: int;
  mutable file_prefix : string;
  mutable tree_format : deriv_tree_type;
  mutable graphviz_on : bool;
  mutable unif_graphs : bool;
};;

let default () =  {
  prove_type= Classic;
  euristics= true;
  need_temp_unif= true;
  max_uni_count= 3;
  file_prefix = "";
  tree_format= Full;
  graphviz_on= false;
  unif_graphs=false;
};;

let print_config c = 
  print_string "{ prove_type=";
  print_endline (match c.prove_type with
    | Classic     -> "classic; "
    | Liberalised -> "liberalised;" );
  print_endline ("euristics="^(string_of_bool c.euristics));
  print_endline ("need_temp_unif="^(string_of_bool c.need_temp_unif));
  print_endline ("max_unif_count="^(string_of_int c.max_uni_count));
  print_endline ("fileprefix="^c.file_prefix);
  print_endline ("graphviz_on="^(string_of_bool c.graphviz_on) );
  print_endline ("unif_graphs="^(string_of_bool c.unif_graphs) );
  print_endline (match c.tree_format with Full -> "tree_format=full };;"
    | ll -> "tree+format=Brief };;")
;;
