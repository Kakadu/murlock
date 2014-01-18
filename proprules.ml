open Lang;;
type sequence = { mutable anti: (Lang.fip list);
                  mutable suc : (Lang.fip list) };;

type derive_el = { s:sequence;
                   mutable from:int list;
                   mutable rule:string };; 
type derivation = derive_el list;; 

let string_of_sequence s =
  let func = Utils.char_separated_list '#' fip_to_pretty_string in
   (func s.anti)^" --> "^(func s.suc)
;;

let get_metavars_seq s =
  let res = List.fold_left 
      (fun acc x -> List.append acc (Lang.get_metavars_fip x)) [] (List.append s.anti s.suc)
  in Utils.rem_dublicates res
;;

let print_sequence (s:sequence) =
  print_endline (string_of_sequence s);
;;

let map_sequence_terms (func:term->term) (s:sequence) : sequence =
(*  print_sequence s; *)
  let foo t = (let ans = func t in (ans = t, ans)) in
  let rec map_term t = 
    match foo t with
    | (false, t) -> t
    | (_, MetaVar m) -> MetaVar m
    | (_, SubjVar v) -> SubjVar v
    | (_, SubjConst c) -> SubjConst c
    | (_,FuncSymbol(name,lst)) ->
          FuncSymbol (name,List.map (map_term) lst)
  in

  let rec map_fip func f = 
(*    print_endline (Lang.fip_to_pretty_string f); *)
    match f with
    | LogConst _ | LogVar _ -> f
    | Formula (AtomFormula(name,lst)) -> 
      let new_args = List.map (map_term) lst in
      Formula (AtomFormula(name,new_args))
    | BinOp (name,g,h) -> BinOp(name, map_fip func g, map_fip func h)
    | UnOp (name,g) -> UnOp(name, map_fip func g)
    | QALL(name,g) -> QALL (name, map_fip func g)
    | QEXZ(name,g) -> QEXZ (name, map_fip func g)
  in
      
  let map_lst = List.map (map_fip func) in
  { anti = map_lst s.anti; suc=map_lst s.suc }
;;

(*
let print_derivation d =
  print_endline "***** print_derivation *****";
  let n = ref 1 in
  List.iter (fun el -> (
    print_int !n; print_string ". "; n:=!n+1; 
    print_sequence el.s;
    if el.from = [-1] then
      print_endline "\t no rules to apply"
    else print_endline 
      ("\t from "^(Utils.comma_separated_list string_of_int el.from)^" by rule "^ el.rule) 
  ) ) d;
  print_endline "***** end of print_derivation *****";
;;
*)
