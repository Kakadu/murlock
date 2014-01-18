open Lang
open Proprules
open Config

let print_dic dic = print_endline ("" ^ (UniAns.fold (fun k v acc -> 
         acc^(match k with MetaVarM.MetaVar m -> m)^" => "^(List.fold_left (fun acc z -> acc^(term_to_pretty_string z)) "" v)^";\n "
) dic ""));;

let t2s  = term_to_pretty_string;;

exception UnifyError of string
exception UnifAnswer of bool * (Lang.term*Lang.term)list

let get_meta m = match m with Lang.MetaVarM.MetaVar mm -> Lang.MetaVar mm;;

let dic2list map = (UniAns.fold (fun k v l  -> 
   let res = List.map (fun z  -> (get_meta k,z)) v in
      List.append res l 
) map [] );;


open Graph
module Vertex = struct include String let default = "" end
module G = Imperative.Digraph.AbstractLabeled(String)(Vertex);;
module MyDfs = Traverse.Dfs(G);;
open G
module StringDic = Map.Make (String)
exception LiberalisedException of term
let map_assoc_string k map = 
  try Some(StringDic.find k map)
  with Not_found -> None;;

let map_assoc_str key map =
  try Some (StringDic.find key map)
  with Not_found -> None;;
let map_assoc_str_term t map =
  map_assoc_str ( (term_to_pretty_string t)) map;;
let map_assoc_meta_term t map = 
  map_assoc (MetaVarM.MetaVar (term_to_pretty_string t)) map;;

let get_metas_termlist = Lang.get_all_terms_termlist 
                 (function MetaVar m -> true | _ -> false);;
let key2str  = function MetaVarM.MetaVar m -> m;;
let key2term = function MetaVarM.MetaVar m -> MetaVar m;;

let build_graph ans = 
  let all_metas = ref (UniAns.fold (fun k v acc -> 
      (key2term k ) :: (List.append (get_metas_termlist v) acc)
  ) ans []) in
  all_metas := Utils.rem_dublicates !all_metas;
(*  all_metas := List.filter (fun x -> match map_assoc_meta_term x ans with
			      | Some _ -> true | None -> false) !all_metas; *)
(*  print_endline ("all_metas: "^
         (Utils.comma_separated_list (term_to_pretty_string) !all_metas)); *)
   let gr = create () in
   let vertex_dic = ref StringDic.empty in
   List.iter (fun x -> let name = term_to_pretty_string x in
                       let v = V.create name in 
            vertex_dic := StringDic.add name v !vertex_dic) !all_metas;

    UniAns.iter (fun k v -> match (k,v) with (MetaVarM.MetaVar k,v) ->
                   match map_assoc_str k !vertex_dic with 
                     | None -> print_endline "very bad"
                     | Some start -> begin
                         let ttt = Utils.rem_dublicates (get_metas_termlist v) in
                         let targets = List.map (fun x -> 
                           match map_assoc_str_term x !vertex_dic with 
                             | None -> (print_endline ("not found: "^(t2s x)); 
                                        raise Not_found
                                        )
                             | Some y -> y
                         ) ttt in
                         List.iter (fun x -> 
   (*print_endline ("add edge from "^(G.V.label start)^ " to "^(G.V.label x));*)
                                             add_edge gr start x) targets
                      end                   
    ) ans;
     gr
;;

let get_v_graph gr = 
  let all = ref StringDic.empty in
  G.iter_vertex (fun v ->
    all:= StringDic.add (V.label v) v !all) gr;
  !all;;

let add_subs2graph graph (l,r) = 
  let all_vertexes = get_v_graph graph in 
  let get_vertex name = match map_assoc_string name all_vertexes with
    | Some v -> v
    | None -> (  V.create name ) in
 
  let all_metas = Utils.rem_dublicates (get_metas_termlist [r]) in
  let leftname = t2s l in
  let start_vertex = get_vertex leftname in
  List.iter (fun x -> 
	       let target = get_vertex (t2s x) in
               add_edge graph start_vertex target
	    ) all_metas
;;

let has_cycles2 ans = 
  let gr = build_graph ans in (not (MyDfs.has_cycle gr), gr);;
let has_cycles ans = fst (has_cycles2 ans );;

let graph_to_dot ch gr = 
  G.iter_edges (fun v1 v2 ->
    output_string ch ("\t" ^ (G.V.label v1) ^ " -> " ^ (G.V.label v2) ^ "\n" )
  ) gr
;;
exception GraphHasCycles of G.t;;

let subs_ans2pairs (cur_ans:Lang.genans) (lst: (term*term) list) = 
  let good,graph = has_cycles2 cur_ans in
  if not good then raise (GraphHasCycles graph);
  let all_vertexes = ref StringDic.empty in
  G.iter_vertex (fun v -> all_vertexes:= StringDic.add (V.label v) v !all_vertexes) graph;
  
  let iter_count  = ref 0 in
  let rec loop t = 
    let all_metas = List.filter (fun x ->  
      UniAns.mem (MetaVarM.MetaVar (term_to_pretty_string x)) cur_ans
    ) (get_metas_termlist [t]) in
        
    let can_subs = List.fold_left (fun acc t -> 
      match map_assoc_string ( (term_to_pretty_string t)) !all_vertexes with
      Some x -> true | None -> acc
    ) false all_metas in

    if not can_subs then t 
    else loop ( List.fold_left (fun acc x -> 
      let key = MetaVarM.MetaVar (term_to_pretty_string x) in
      match map_assoc key cur_ans with
        | Some [tt] -> begin 
(*                         print_endline (" let's substitute in" ^ (t2s x) ^ ": " ^ (t2s tt) ^ " ===> " ^ (t2s acc)); *)
                         PredRules.substitute_term acc x tt end
	| None 
        | Some []
        | Some (_::_) -> (print_endline "Something wrong"; print_endline ("x= " ^ (t2s x)); acc)

     
      ) t all_metas
    ) 
  in
    List.map (fun (a,b) -> 
      iter_count :=0; let a1 = loop a in iter_count :=0; let b1 =loop b
      in (a1,b1)    
    ) lst
;;

exception UnifyInternalException of string
exception LibAnswer of genans option
let build_lib_graph (gr:G.t) ans = 
  let meta_or_skolem = function Lang.MetaVar _ -> true
  | Lang.SubjVar _  -> true 
  | _ -> false in
  Ans_dic.KonevForbs.iter (fun k v ->
        let dest = Utils.rem_dublicates(List.fold_left (fun acc x -> 
            acc @ (Lang.get_all_terms_term meta_or_skolem x)) [] v) in
        let kver = V.create (t2s k) in
	List.iter (fun x -> let target = V.create (t2s x) in
		     add_edge gr kver target) dest
  ) ans;
    gr
;;

let build_lib_graph2 (gr:G.t) ans = 
  let meta_or_skolem = function Lang.MetaVar _ -> true
  | Lang.SubjVar _ -> true 
  | _ -> false in

  UniAns.iter (fun key value -> match (key,value) with
		 | (Lang.MetaVarM.MetaVar k, v) ->
               let dest = Utils.rem_dublicates(List.fold_left (fun acc x -> 
                 acc @ (Lang.get_all_terms_term meta_or_skolem x)) [] v) in
	       let kver = V.create k in
	List.iter (fun x -> let target = V.create (t2s x) in
		     add_edge gr kver target
		  )dest
	    ) ans;  
    gr
;;


let unif_abstract3 
  use_simplify      
  (unif_func : (term*term) list -> term list UniAns.t -> (term list UniAns.t) option)
  (ok_func : term list UniAns.t -> bool)
  lst  =
  let targets = ref [] in
  let okAtoms = function 
     (Formula(AtomFormula(x,_)),Formula(AtomFormula(y,_))) when x=y 
          -> true | _ -> false  in
 try  
(*  List.iter (print_sequence) lst; *)
  List.iter (fun seq -> if List.mem (Lang.LogConst False) seq.anti then ()
             else if  List.mem (Lang.LogConst True) seq.suc then ()
             else let pairs = Utils.mul_cartesian seq.anti seq.suc in
             let reduced_targets = List.filter (okAtoms) pairs in
             if []=reduced_targets then 
                 raise (LibAnswer None);
             targets := reduced_targets :: !targets
  ) lst;
  let pre_targets = List.map (List.map (function
    (Formula(AtomFormula(n1,lst1)),Formula(AtomFormula(n2,lst2))) ->
        (FuncSymbol(n1,lst1), FuncSymbol(n2,lst2))
		| _ -> raise (UnifyInternalException "")
  ) ) !targets in
  let pre_ans,pre_targets2 =  if not use_simplify then UniAns.empty,pre_targets
    else begin
      let one_pairs                           = ref [] in
      let pairs_last                          = ref [] in 
      let pre_ans = ref UniAns.empty in   
(* 
      print_endline "before simplification;";
 List.iter (fun x -> print_endline (
    Utils.comma_separated_list (fun (a,b) -> "{"^(t2s a)^"; "^(t2s b) ^"}") x
 )  )pre_targets; 
*)
      let rec simplify lst =
        let ostatok = List.map (List.filter (fun y ->
                         match unif_func [y]  !pre_ans with
                           | Some subs when ok_func subs -> true 
			   | _ -> false
                      )) lst in
  
        if List.mem [] ostatok then raise (LibAnswer None);

        let flt = List.partition (fun x -> 1 = List.length x) ostatok in
        if ([] = fst flt) then pairs_last := ostatok else begin
          let new_one_pairs = List.map (List.hd) (fst flt) in
          one_pairs :=  !one_pairs @ (new_one_pairs);
          match unif_func new_one_pairs !pre_ans with
          | Some (map) when (ok_func map) -> (
	    print_endline "set pre_ans";
	    pre_ans := map;          simplify (snd flt) )
          | _ -> raise (LibAnswer None)

        end
      in simplify pre_targets;
      (!pre_ans, !pairs_last )
    end 
  in
  if [] = pre_targets2 then (
    print_endline "maxiters=0. return";
    raise (LibAnswer (Some pre_ans))
  );
  if (List.mem [] pre_targets2) then raise (LibAnswer None);

  let lengthes = List.map (List.length) pre_targets2 in
  let maxiters = List.fold_left(fun acc x -> acc * x) 1 lengthes in 
  print_endline ("maxiters=" ^ (string_of_int maxiters) );

(*  List.iter (fun x -> print_endline (
    Utils.comma_separated_list (fun (a,b) -> "{"^(t2s a)^"; "^(t2s b) ^"}") x
  )  )pre_targets2; *)
  if []=pre_targets2 then print_endline "[]"; 
  print_endline "go iterate";
  let res = Utils.aggr_listlist pre_targets2 pre_ans
    (fun (f1,f2) acc -> unif_func [f1,f2] acc) 
    ok_func in
  match res with
    | None  -> raise (LibAnswer None)
    | Some (ans) -> raise (LibAnswer (Some ans))
 with LibAnswer ans -> match ans with
   | None -> (false,[])
   | Some map -> (true, dic2list map)
;;
let rec unif2fs_lists_lib pairs cur_ans =
(*  print_endline "unif2fs_lists_lib"; 
  let str = 
    Utils.comma_separated_list (fun (a,b) -> (t2s a)^" => "^(t2s b) ) pairs in
    print_endline str; *)
  let this = unif2fs_lists_lib in

  if (List.length pairs = 0) then Some cur_ans else begin
  match (List.hd pairs) with 
    | (a,b) when a=b -> this (List.tl pairs) cur_ans
    | (FuncSymbol(n1,ll1),FuncSymbol(n2,ll2)) ->
      if (n1<>n2 or (List.length ll1 <> List.length ll2)) then 
            (None)
         else this ( (List.combine ll1 ll2) @ (List.tl pairs)) cur_ans 
    | (big, MetaVar m) 
    | (MetaVar m, big)  ->  (
        match map_assoc (MetaVarM.MetaVar m) cur_ans with
          | None -> (      
              let new_ans = UniAns.add (MetaVarM.MetaVar m) [big] cur_ans in
		this (List.tl pairs) new_ans)
	  | Some r -> this ((big, List.hd r) :: List.tl pairs) cur_ans
      )            
    | (a,b) -> ( None )
  end
;; 

let unif_graphs_count = ref 0;;

let unify_liberalized config s forbs = 
  let start_gr = build_lib_graph (G.create ()) forbs in  
  let ok_subs_lib  subs = ( 
(*    print_endline "ok_subs_lib";
    UniAns.iter (fun (MetaVarM.MetaVar k) v ->
      print_endline (k ^ " --- " ^ (t2s (List.hd v) ))
    )   subs; *)
    let new_graph = build_lib_graph2 (G.copy start_gr) subs in
    if config.unif_graphs then (
      incr unif_graphs_count;
      let ch = open_out (config.file_prefix ^ "unifgr"^
			   (string_of_int !unif_graphs_count)^".dot") in
      print_endline "printing temp_unif graph";
      output_string ch "digraph X {\n";
      let seq_str  = Utils.string_separated_list "\\n" (Proprules.string_of_sequence) s in
      output_string ch ("seq [label=\"" ^ (seq_str) ^ 
			   "\"];\n");
      output_string ch "\tseq -> seq\n";
      graph_to_dot ch new_graph;
      output_string ch "}\n";
      close_out ch
    );
    not (MyDfs.has_cycle new_graph) )    
  in
  unif_abstract3 config.euristics unif2fs_lists_lib (ok_subs_lib) s
;;

let rec unif_pairs_classic pairs ans : (term list UniAns.t) option =
(*  print_endline (Utils.comma_separated_list 
		   (fun (a,b) -> "("^(t2s a)^","^(t2s b)^")") pairs); *)
  let is_in_dic k v dic = match map_assoc k dic with
  | None     -> false
  | Some lst -> List.mem v lst
  in
(*  let subs_to_tail term1 term2  = 
    let subs a= PredRules.substitute_term a term1 term2 in
    List.map ( fun (a,b) ->  (subs a,subs b) )
  in *)
  let tail t1 t2 = 
    (*subs_to_tail t1 t2 *) (List.tl pairs)
  in
    
  let compose_subs start meta t = match meta,t with
    | (Lang.MetaVar m,_) -> 
      let dic1 = UniAns.mapi (fun k (v::_) -> [PredRules.substitute_term v meta t]) start in
      UniAns.add (Lang.MetaVarM.MetaVar m) [t] dic1
    | _ -> raise (UnifyInternalException "blabla")
  in
      
  let this = unif_pairs_classic in
      match pairs with
      | []  -> Some ans
      | ((a,b)::_) -> begin 
 (* OCaml has keyword `as' *)
        match a,b with
        | (u,v) when u=v -> this (List.tl pairs) ans
        | (FuncSymbol (n1,_),FuncSymbol (n2,_)) 
	    when n1<>n2 -> None
        | (FuncSymbol (_,args1),FuncSymbol (_,args2)) 
	    when ((List.length args1) <> List.length args2) -> None
        | (FuncSymbol (_,args1),FuncSymbol (_,args2)) -> 
            this ((List.combine args1 args2) @ (List.tl pairs)) ans
 
        | (MetaVar a, MetaVar b) -> begin
           let keya = Lang.MetaVarM.MetaVar a in
           let keyb = Lang.MetaVarM.MetaVar b in
                 if (is_in_dic keya (MetaVar b) ans) or 
                    (is_in_dic keyb (MetaVar a) ans) then 
                      this (List.tl pairs) ans
                 else match (map_assoc keya ans,map_assoc keyb ans) with
                    | (None,None) ->
                        this (tail (MetaVar a)(MetaVar b)) (compose_subs ans (MetaVar a) (MetaVar b))
                    | (Some (u::_), None) -> 
                        this (tail (MetaVar b) u) (compose_subs ans (MetaVar b) (u) )
                    | (None, Some (u::_)) -> 
                        this (tail (MetaVar a) u) (compose_subs ans (MetaVar a) u)
                    | (Some u,Some v) when u=v -> this (List.tl pairs) ans (* bug here *)
                    | _ -> None
               end
            | (MetaVar a, big) 
            | (big, MetaVar a) -> (
                let key  = Lang.MetaVarM.MetaVar a in
(*                if not (agrees_with_forbs (MetaVar a, big)) then
                  None *)
                match map_assoc key ans with
                   | None -> this (tail (MetaVar a) big) (compose_subs ans (MetaVar a) big )
                   | Some lst when (List.mem big lst) (* bug here *)
                          -> this (List.tl pairs) ans
                   | _    -> (None)
             )
            | _  -> None
       end
;;

let t2s = Lang.term_to_pretty_string;;

let unify_classic config s forbs = 
  let agreesWithForbsNormal (meta,what) =
    print_endline ("agreesWithForbsNormal MetaVar "^(t2s meta) ^" and "^
		      (t2s what)); 
    (not (Lang.is_sub_term meta what)  ) &&
    (let is_in_dic k v dic = match Lang.map_assoc k dic with
    | None     -> print_endline "is_in_dic:None"; false
    | Some lst -> List.fold_left (fun acc x -> acc || (Lang.is_sub_term x what) )  false lst
    in
    let key = Lang.MetaVarM.MetaVar (Lang.term_to_pretty_string meta) in
    (not (is_in_dic key what forbs) ) 
    )
  in
  let ok_ans subs = let res = ref true in

(*    print_dic subs; *)
    UniAns.iter (fun (MetaVarM.MetaVar k) -> function
      | (v::_) -> res := !res && (agreesWithForbsNormal  (MetaVar k,v) ) 
      | _      -> raise ( UnifyInternalException "")
    ) subs;
    !res
  in
  unif_abstract3 config.euristics (unif_pairs_classic) (ok_ans)  s
;;
