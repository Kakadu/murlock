open Proprules
open Bigseq
open Printf
module Aux = Utils;;
module Superseq = Bigseq;;
let max_iters = 2;;
let t2s = Lang.term_to_pretty_string;;

type provingResult = AlwaysTrueFip | DepthLimit 
                   | SubstitutionFounded of (Lang.term*Lang.term)list ;;

type treeItem = | Info of string	
		| Node of nodeContent
and nodeContent = { seq:bigSeq;
		    id:string;
		    mutable fip:(Lang.fip*Superseq.seqHalf) option;
		    mutable rulename:string;
		    mutable sons:treeItem list;
		    mutable temp_unifs:int list;
		    mutable norm_unifs:int list;
		    mutable subs: ((Lang.term*Lang.term) list) option;
		    mutable new_term : Lang.term option
		  };;
open Lang;;
let main_symbol = function
  | QALL(_,_) -> "ALL"
  | QEXZ(_,_) -> "EXZ"
  | BinOp(name,_,_) -> name
  | UnOp(name,_) -> name
  | _ -> "error";;
let get_rule = function
    | (f,Superseq.Anticedent) -> "-->"^(main_symbol f)
    | (f,Superseq.Succedent)  -> (main_symbol f)^ "-->"
;;
let iterate_tree  func root = 
  let rec inner node = 
    match node with
      | Info _ -> ()
      | Node n -> (List.iter func n.sons; List.iter inner n.sons)
  in
    
  func  root;
  inner root
;;
let pderiv_legend ch =
  let metastr = "__[a-z][a-z0-9] are MetaVars\\n" 
  and subjstr = "[a-z][a-z0-9] are SubjVars or LogVars\\n"
  and predstr = "[A-Z] are names of Predicates or FuncSymbols\\n" 
  and ops     = "&=AND, OR, !=NOT is negotiation\\n=> implication, == equevalence\\n"
  and quan    = "ALL - quantifier of generality, EXZ is quantifier of existence"
  in
  Printf.fprintf ch "legend [shape=box,label=\"LEGEND:\\n%s\"];" 
    (metastr^subjstr^predstr^ops^quan)
;;
let print_deriv_list (ch:out_channel) (root:treeItem) : unit =
  let echo = Printf.fprintf ch in
  let my_fold func lst = 
    let lst2 = List.fold_left (fun acc x -> match func x with None -> acc | Some s ->  s :: acc) [] lst in
    Utils.comma_separated_list (fun x -> x) lst2
  in
  let print_func = function
    | Info _ -> ()
    | Node node -> 
        echo "%s. %s\n" node.id 
          (Proprules.string_of_sequence (node.seq#get_normal_seq));
      let sons_str = (my_fold (function Info _ -> None | Node nn ->Some nn.id) node.sons) in
      if (sons_str <> "") then
	fprintf ch " sons are [%s]\n" sons_str;
      let tu_str = Utils.comma_separated_list (string_of_int) node.temp_unifs in
      if tu_str <> "" then
	fprintf ch "  used in temprary unifications: [%s]\n" tu_str;

  in
  iterate_tree print_func root
;;
module IntKey = struct
  type t = int
  let compare = compare
end;;
module IntDic = Map.Make (IntKey);;

let print_deriv_list_TEX (ch:out_channel) (root:treeItem) : unit =
(*  fprintf ch "%s" "\\documentclass[a4paper,10pt]{article}\n";
  fprintf ch "%s" "\\usepackage[T2A]{fontenc}\n";
  fprintf ch "%s" "\\usepackage[utf8]{inputenc}\n";
  fprintf ch "%s" "\\usepackage[russian]{babel}\n";
  fprintf ch "%s" "\\usepackage{multirow}\n";
  fprintf ch "%s" "\\usepackage{textcomp}\n";
  fprintf ch "%s" "\\usepackage{tikz}\n";
  fprintf ch "%s" "\\usepackage{graphicx} \n";
  fprintf ch "%s" "\\begin{document}\n"; *)
  fprintf ch "%s" "\\begin{enumerate}\n";
  let dic = ref IntDic.empty in
  let maxn = ref 0 in
  let nodes_collector node = match node with
    | Info _ -> ()
    | Node n -> (
      let num =  int_of_string n.id in
      if num > !maxn then maxn := num;
      dic := IntDic.add num n !dic
    )
  in  
  let my_fold func lst = 
    let lst2 = List.fold_left (fun acc x -> match func x with None -> acc | Some s ->  s :: acc) [] lst in
    Utils.comma_separated_list (fun x -> x) lst2
  in

  let replace s1 = Str.global_replace (Str.regexp s1) in
  let replace_logic s = 
       let s3 = replace "EXZ" "\\exists " s in
       let s4 = replace "ALL" "\\forall " s3 in
       let s5 = replace "-->" "\\rightarrow " s4 in
       let s6 = replace "OR" " \\vee " s5 in
       let s7 = replace "=>" " \\Rightarrow " s6 in
       let s8 = replace "IMPL" " \\Rightarrow " s7 in
       let s9 = replace "&" "\\&" s8 in
       s9
  in
  let rename_terms_func = function
    | MetaVar "__t1" ->MetaVar  "\\alpha"
    | MetaVar "__t2" ->MetaVar  "\\beta"
    | MetaVar "__t3" ->MetaVar  "\\gamma"
    | MetaVar "__t4" ->MetaVar  "\\delta"
    | MetaVar "__t5" ->MetaVar  "\\varepsilon"
    | MetaVar "__t6" ->MetaVar  "\\zeta"
    | MetaVar "__t7" ->MetaVar "\\eta"
    | MetaVar "__t8" ->MetaVar "\\theta"
    | MetaVar "__t9" ->MetaVar "\\iota"
    | MetaVar "__t10"->MetaVar "\\kappa"
    | MetaVar "__t11"->MetaVar "\\lambda"
    | MetaVar "__t12"->MetaVar "\\mu"
    | MetaVar "__t13"->MetaVar "\\nu"
    | MetaVar "__t14"->MetaVar "\\xi"
    | MetaVar "__t15"->MetaVar "\\rho"
    | MetaVar "__t16"->MetaVar "\\sigma"
    | MetaVar "__t17"->MetaVar "\\tau"
    | MetaVar "__t18"->MetaVar "\\upsilon"
    | MetaVar "__t19"->MetaVar "\\alpha'"
    | MetaVar "__t20"->MetaVar "\\beta'"
    | MetaVar "__t21"->MetaVar "\\gamma'"
    | MetaVar "__t22"->MetaVar "\\delta'"
    | SubjConst "_a" ->SubjConst "c1"
    | SubjConst "_b" ->SubjConst "c2"
    | SubjConst "_c" ->SubjConst "c3"
    | SubjConst "_d" ->SubjConst "c4"
    | t -> t 
  in

  let map_pairs = 
    List.map (fun (a,b) -> (rename_terms_func a,rename_terms_func b)) in
  iterate_tree nodes_collector root;  
  for i=1 to !maxn do begin
    let node = IntDic.find i !dic in
      let old_seq = node.seq#get_normal_seq in
      let new_seq = Proprules.map_sequence_terms (rename_terms_func) old_seq in
      let fip_str1 = (Proprules.string_of_sequence new_seq) in
      let fip_str2 = replace "_" "\\_" fip_str1 in
      let fip_str3 = replace_logic fip_str2 in
      let fip_str_last = replace "#" "," fip_str3 in
        fprintf ch "\\item $%s$" fip_str_last ;

      let tu_str = Utils.comma_separated_list (string_of_int) node.temp_unifs in
      if tu_str <> "" then
	fprintf ch "\\\\\n  used in temporary unifications: [%s]" tu_str;

      let sons_str = 
	my_fold (function Node nn ->Some nn.id | _ -> None) node.sons in
      if (sons_str <> "") then
	fprintf ch "\\\\\n from [%s] " sons_str;
      (match node.fip with
	| Some pair -> fprintf ch "by rule $%s$" 
	  (replace_logic (get_rule pair))
	| None -> () );
      ( match node.new_term with
	| Some t -> fprintf ch " with new term $%s$" (t2s (rename_terms_func t))
	| None -> () );
      (match node.subs with None -> () | Some s ->
	let new_subs = map_pairs s in
	let pair2str (a,b) =  (t2s a) ^ " \\mapsto "^(t2s b) in
	fprintf ch "\\\\ \n unification succeded using [$%s$]"
	  (replace "__" "\\_\\_" (Utils.comma_separated_list (pair2str) new_subs) )
      );
	fprintf ch "\n"
       end done;
  fprintf ch "%s" "\\end{enumerate}\n"
(*  ;
  fprintf ch "%s" "\\end{document}\n" *)

;;

let print_deriv_abstract str_of_seq show_unif show_global_unif 
    plain_leafs show_numbers ch root =
  let start_id = 1 in
  let id_counter  = ref 1 in
  let next_id () = incr id_counter; !id_counter in
  fprintf ch "%sdigraph X {\n" "";
  let print_node parent_id node = 
    let id = next_id () in
    (match node with
    | Info info -> 
      let typeofleaf = if plain_leafs then "shape=plaintext," else "" in
      fprintf ch "\t%d [%slabel=\"%s\"]\n" id typeofleaf info;
      fprintf ch "\t%d -> %d\n" parent_id id;
    | Node node -> (
      (*let num=if show_numbers then (string_of_int id)^": " else "" in*)
      let num="" in
      fprintf ch "\t%d -> %d\n" parent_id id;
      if show_unif then (
	List.iter (fun n -> 
(*	  let edge_str = ("\t"^id^" -> "^ id^"_"^(string_of_int n)^"\n" ) in *)
	  fprintf ch "\t%d -> tu%dof%d\n" id id n;

	  fprintf ch "\ttu%dof%d [label=\"%d\"];" id n n
	) node.temp_unifs
      );
      match node.norm_unifs, show_global_unif with
	| (h::tt, true) ->
             fprintf ch "\tsubgraph cluster%d {\n" h;
             fprintf ch "\t\t%d [shape=record,label=\"%s%s\"];\n" 
	       id num (str_of_seq node);
             fprintf ch "\t}\n";
	| _ ->  
             fprintf ch "\t%d [shape=record,label=\"%s%s\"];\n" id num 
	       (str_of_seq node);

    ));
    id
  in
  (match root with
    | Info info -> (
      let typeofleaf = if plain_leafs then "shape=plaintext" else "" in
      fprintf ch "\t%d [%s,label=\"%s\"]\n" start_id typeofleaf info
    )
    | Node root -> (
      fprintf ch "\t%d [shape=record,label=\"%s%s\"];\n" 
	start_id "" (str_of_seq root);
      let rec tree_fold parent_id node = 
	let id = print_node parent_id node in
	match node with 
	  | Node node -> List.iter (tree_fold id) node.sons
	  | _ -> ()
      in
      List.iter (tree_fold start_id) root.sons
    ));
  fprintf ch "%s}\n" "";
  flush ch
;;

(* add substitutions to lists of derivation *)
let add_subs2deriv leaves (ans_subs: (term*term) list)  = 
  let f2s = Lang.term_to_pretty_string in
  List.iter (function 
    | Info _ -> () 
    | Node item -> 
      let all_metas = List.map (fun x -> Lang.MetaVar x) 
	(Proprules.get_metavars_seq (item.seq#get_normal_seq)) in 
      
      let useful_subs = ( List.filter 
	(fun x -> List.mem (fst x) all_metas) ans_subs) in 
      item.subs <- Some useful_subs;

      let s = match (List.fold_left (fun y x -> 
        (f2s (fst x))^" => "^(f2s (snd x))^"\\n"^y) "" useful_subs) with
	| "" -> "axiom"
	| s -> s
      in
      item.sons <- (Info s):: item.sons
    ) leaves
;;

let string_of_node_light el = 
  let str_of_anti s =
     (Utils.char_separated_list ',' (Lang.fip_to_pretty_string) s.anti)
  and str_of_suc s =
     (Utils.char_separated_list ',' (Lang.fip_to_pretty_string) s.suc)
  in
  let curseq = el.seq#get_normal_seq in
  let anti_str = str_of_anti curseq
  and  suc_str = str_of_suc  curseq in
  let fmt =  format_of_string "{ %s --\\>%s }" in 
  let encode = Str.global_replace (Str.regexp ">") "\\>" in 
  Printf.sprintf fmt (encode anti_str) (encode  suc_str) 
;;
let string_of_node_full el = 
  let str_of_anti s =
    (Utils.char_separated_list ',' (Lang.fip_to_pretty_string) s.anti)
  and str_of_suc s =
     (Utils.char_separated_list ',' (Lang.fip_to_pretty_string) s.suc)
  in
  let curfip = match el.fip with | None -> ""
    | Some(f,Superseq.Succedent ) -> "--> "^(Lang.fip_to_pretty_string f)
    | Some(f,Superseq.Anticedent) -> (Lang.fip_to_pretty_string f)^" -->"
  in
  let curseq = el.seq#get_normal_seq in
  let anti_str = str_of_anti curseq
  and  suc_str = str_of_suc  curseq in
  let fmt =  (
    if (String.length anti_str > 70) || (String.length suc_str > 70) then
      format_of_string "{<seq> %s \\n--\\>\\n%s |<lastfip> %s \\n }"
    else format_of_string "{<seq> %s --\\>%s |<lastfip> %s \\n }") in 
  let encode = Str.global_replace (Str.regexp ">") "\\>" in 
  Printf.sprintf fmt (encode anti_str) (encode  suc_str) (encode curfip)
;;

let print_deriv_light =
  print_deriv_abstract string_of_node_light false false true true;;
let print_deriv_full =
  print_deriv_abstract string_of_node_full true true false true;;



let unif_every_minus part ff = match part,ff with
  | (Anticedent, Lang.QALL(_,_)) 
  | (Succedent,  Lang.QEXZ(_,_)) -> true
  | _                            -> false 
;;
let unif_never _ _ = false;;

exception ProvingError of string
exception Continue
exception ProvingResult of provingResult

let proveAbstr f fileprefix filepostfix get_gamma get_delta doUnif printer maxItersCount need_temp_unif =
  let filename = fileprefix^"."^filepostfix in
  let tree_ch = open_out (filename^(".dot")) in
  let d_list_ch = open_out (filename^".deriv") in  
  let tex_ch = open_out (filename^".tex") in  
  let info_ch = open_out (filename^".info") in
  let build_start_node f =
    Node {sons=[]; fip=None; seq=new bigSeq ({anti=[];suc=[f]},[]);
	  rulename=""; id="1"; subs=None; new_term=None;
	  temp_unifs=[];norm_unifs=[]} in
  let root = build_start_node f in
  let leaves  = ref [root] in
  let nodes_count  = ref 1 in

  let uni_counts = ref maxItersCount in

  let close_streams () = 
    Log.flush;
    close_out d_list_ch;
    close_out tree_ch;
    close_out info_ch;
    close_out tex_ch
  in
  let mainresult = ref DepthLimit in
  let temp_uni_count = ref !uni_counts in
  let id_counter  = ref 1 in
  let next_id () = incr id_counter; string_of_int (!id_counter) in
  let prove_inner () = try 
    let flag = ref true in
    
    while !flag do begin try
      print_endline ("uni_counts = "^(string_of_int !uni_counts)); 
      if !uni_counts = 0 then (raise (ProvingResult DepthLimit)); 

      let newleaves = ref [] in
      let no_rules_applied = ref true in
      
      (*all levaes are in derivation already *)
      let unif_needed = ref false in
      (* let's loop iterate leaves *)

      let rec loop_leaves lll = match lll with
      | []    -> ()
      | (Info _) :: tl -> loop_leaves tl
      | (Node leaf)::tl -> begin
        match (leaf.seq#apply_all_rules get_delta get_gamma) with
        | None -> ( newleaves := (Node leaf) :: !newleaves;
                    loop_leaves tl
                  )
        | Some (lst,part,ff,ruletext, created_term,_) -> (
              no_rules_applied := false;              
              leaf.rulename <- ruletext;
              leaf.fip   <- Some (ff,part);
	      leaf.new_term <- created_term;
              let axioms = ref [] in

              List.iter (fun x -> 
                    let newItem = Node 
		      {seq=x; rulename=""; fip=None; norm_unifs=[]; subs=None; 
		       temp_unifs=[]; id=next_id (); sons=[]; 
		       new_term=None} in
		    leaf.sons <- newItem :: leaf.sons;

		    incr nodes_count;
                    if x#is_axiom then 
                     axioms := newItem :: !axioms
                    else 
                     newleaves := newItem :: !newleaves
              ) lst;
              
              List.iter (function Node x -> x.sons <- (Info "axiom") :: x.sons
		| _ -> ()
	      ) !axioms;


	      let need_unif = need_temp_unif part ff in
              match need_unif with
              | true -> ( unif_needed := true;  
			  newleaves := List.append  !newleaves tl )
              | false-> loop_leaves tl 
        )
      end

      in loop_leaves !leaves;

      leaves:= !newleaves;

      let clear_minus_counts lst =
         List.iter (fun y -> let l = match y with
                   | Node z  -> z
                   | _  -> raise (ProvingError "ProvingError") 
                  in (l.seq)#clr_minus_rule_apply_count
         ) lst
      in

      let perform_unif () = begin
      	  match doUnif !leaves with
          | (true,[] ) -> ( print_endline "Always true fip. No MetaVars substitutions";
	                        raise (ProvingResult AlwaysTrueFip)
          )
      	  | (true,ans) -> ( print_endline "Unification succedeed.";
	          let out_str = List.fold_left (fun acc x  -> acc^
                (Lang.term_to_pretty_string (fst x))^" => "^(Lang.term_to_pretty_string (snd x))^"\n") "" ans
              in
              print_endline out_str;
              raise (ProvingResult (SubstitutionFounded ans))
	      )
          | (false,_) -> print_endline "Unification failed"
      end in
      
	  let kk = List.fold_left (fun acc x ->
	       match x with Info _ -> acc
	                  | Node ls -> (
	                    if ((not ls.seq#is_empty) && (not ls.seq#has_atomic_fips)) then false
	                    else acc       )
      ) true !leaves in

	  match (!no_rules_applied,!unif_needed) with
	  | (true,true) -> raise (ProvingError "no rules has been applied but unification needed")
	  | (true,false)-> (
	      (* set markers for sheduled unification number *)
            List.iter (fun z -> match z with
              | Node lf -> lf.norm_unifs <- (!uni_counts) :: lf.norm_unifs
              | Info _ -> ()
            ) !leaves;
            uni_counts := !uni_counts - 1;
            clear_minus_counts !leaves;
            if not kk then raise Continue;
            perform_unif ()
	  )
	  | (false,true) -> ( if not kk then raise Continue;
		  print_endline "go temp unif";
                  temp_uni_count := !temp_uni_count +1;
                  List.iter (fun z -> match z with
                    | Node lf -> 
		          lf.temp_unifs <- !temp_uni_count :: lf.temp_unifs
                    | Info _ -> ()
                  ) !leaves;
  
                  perform_unif ()
	  )
	  | (false,false) -> ()
      
	with Continue -> ()
    end
  done

  with ProvingResult pres -> ( 
    mainresult := pres;
    let add_text s = match root with
      | Info _ -> () | Node n -> n.sons <- (Info s):: n.sons
    in
    match pres with
      | SubstitutionFounded lst -> add_subs2deriv !leaves lst
      | DepthLimit              -> add_text "No result"
      | AlwaysTrueFip           -> add_text "Proved"
   )       
  in
  let time_start = Unix.gettimeofday () in 
  prove_inner ();
  let duration = (Unix.gettimeofday ()) -. time_start in
  Printf.fprintf info_ch "DURATION=%s\n" (string_of_float duration);
  Printf.fprintf stdout "DURATION=%s\n"  (string_of_float duration);

  Printf.fprintf info_ch "NODESCOUNT=%s\n" (string_of_int !nodes_count);
  Printf.fprintf stdout "NODESCOUNT=%s\n"  (string_of_int !nodes_count);
  printer        tree_ch root;
  print_deriv_list_TEX tex_ch root;
  close_streams ();
  (root, !mainresult, filename^".dot")
;;

let agreesWithForbsNormal forb subs = 
    let is_in_dic k v dic = match Lang.map_assoc k dic with
    | None     -> false
    | Some lst -> List.mem v lst
    in
    let key = Lang.MetaVarM.MetaVar (Lang.term_to_pretty_string (fst subs)) in
    let where = snd subs in
    not (is_in_dic key where forb) 
;;

open Config
let prove_classic (config:Config.prove_config) f = 
  let all_subjvar_names = ref (Lang.get_all_var f) in
  let all_metavars = ref Lang.UniAns.empty in

  let get_new_subj (seq,_) = 
    let metas = Lang.get_metavars_fiplist (List.append seq.anti seq.suc) in
    let rec loop i =
      let new_var = "w"^(string_of_int i) in
      if List.mem new_var !all_subjvar_names then loop (i+1)
      else (
        all_subjvar_names := new_var:: !all_subjvar_names;
        all_metavars:= Lang.UniAns.mapi (fun k v ->
          if List.mem (match k with Lang.MetaVarM.MetaVar n -> n) metas then
            (Lang.SubjVar new_var)::v
          else v
        ) !all_metavars; 
        Lang.SubjVar new_var  
        ) 
    in loop 1  
  in
  let get_new_meta seq =
    let rec loop i  =
      let new_meta = ("__t"^(string_of_int i)) in
      match Lang.map_assoc (Lang.MetaVarM.MetaVar new_meta) !all_metavars with
        | None -> ( all_metavars := Lang.UniAns.add 
		                      (Lang.MetaVarM.MetaVar new_meta) [] 
                    !all_metavars;
                    Lang.MetaVar new_meta )
        | Some x -> loop (i+1)
    in loop 1    
  in
  let doUnif lst = 
    let forb = !all_metavars in
    let targets = List.fold_left (fun acc x -> match x with
    | Info _ -> acc
    | Node lf -> ( match List.hd (lf.seq)#get_seqs with 
                        | {anti=[];suc=[]} -> acc
                        | atoms            -> atoms :: acc )
    ) [] lst in
    Unif.unify_classic config targets forb 
  in

  proveAbstr f (config.file_prefix) "" (get_new_meta) (get_new_subj) 
             (doUnif) 
    (match config.tree_format with Brief -> print_deriv_light | Full -> print_deriv_full) 
    (config.max_uni_count) 
    (match config.need_temp_unif with true -> unif_every_minus | false -> unif_never )
;;


let prove_liberalised config f = 
  let rec find_index prefix target i = 
      if not (List.mem (prefix ^ (string_of_int i)) target) then i 
      else find_index prefix target (i+1)
  in 
  let forbid_term t = match t with 
    | Lang.SubjVar n when ("w" = Str.string_before n 1) -> true 
    | Lang.MetaVar _ -> (true)
    | _ -> false in

  let t2s = Lang.term_to_pretty_string in
  let meta_index = ref ( find_index "__t" (Lang.get_metavars_fip f) (1)) in
  let all_metas = Lang.get_all_terms_fip 
                       (function Lang.MetaVar _ -> true | _ -> false) f in
  let subj_index = ref ( find_index "w" (List.map (t2s) all_metas ) 2) in
  let forbs = ref (Ans_dic.KonevForbs.empty ) in

  let get_delta (seq,fip) = 
    incr subj_index;
    let ans  = Lang.SubjVar ("w"^(string_of_int !subj_index) ) in
    let targets = Lang.get_all_terms_fip forbid_term fip in
    (match Ans_dic.konev_assoc ans !forbs with
    | Some lst -> List.iter (fun x -> forbs :=
                    Ans_dic.KonevForbs.add  ans (x::lst) !forbs) targets
    | None     -> List.iter (fun x -> forbs :=
                    Ans_dic.KonevForbs.add  ans [x] !forbs) targets);
    ans
  in
  let get_gamma (seq,fip) = 
    let ans = Lang.MetaVar ("__t"^(string_of_int !meta_index) )     in
    incr meta_index;
(*    print_endline ("new gamma term `"^(Lang.term_to_pretty_string ans)
                  ^"'  for `"^(Proprules.string_of_sequence seq)^
		   "' and fip `"^(Lang.string_of_fip fip)^ "'"); *)
    ans
  in
    let doUnif lst = begin
      let targets = List.fold_left (fun acc x -> match x with
      | Info _ -> acc
      | Node lf -> ( let atoms = List.hd (lf.seq)#get_seqs in
         match atoms with 
         | {anti=[];suc=[]} -> acc
         | _                -> atoms :: acc
        )
    ) [] lst in
    match     Unif.unify_liberalized config targets !forbs with
    | (true,ans) -> (true,ans)
    | (false,_)  -> (false,[])
  
  end in
  print_endline "proving like Konev";
  proveAbstr f config.file_prefix "" (get_gamma) (get_delta) (doUnif) 
  (match config.tree_format with Brief -> print_deriv_light 
    | Full -> print_deriv_full ) 
  config.max_uni_count 
  (match config.need_temp_unif with true -> unif_every_minus | false -> unif_never )

;;
