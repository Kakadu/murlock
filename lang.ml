type logConst = True | False;;

type term = SubjVar of string 
          | MetaVar of string 
          | SubjConst of string
          | FuncSymbol of string * term list;;

type atomFormula = AtomFormula of string * term list;;(* string - predicate name *)
type fip = LogConst of logConst 
         | LogVar of string
         | Formula of atomFormula   
	 | BinOp of string*fip*fip 
         | UnOp  of string*fip 
	 | QALL  of string*fip 
         | QEXZ  of string*fip
;;
(* below there are 3 functions for cutting specific terms from fip *)


let rec get_all_terms_term cond t = match t with
    | FuncSymbol (n,lst) -> List.fold_left (fun acc x -> 
                  List.append acc (get_all_terms_term  cond x)) 
          (if cond t then [t] else []) lst
    | a when cond a -> [a]
    | _ -> []
;;

let rec get_all_terms_termlist cond lst = 
  Utils.rem_dublicates (List.fold_left (fun acc x -> 
           List.append acc (get_all_terms_term cond x)
  ) [] lst)
;;

let rec get_all_terms_fip cond f = match f with
  | QEXZ (_,g) | QALL (_,g) 
  | UnOp (_,g) -> get_all_terms_fip cond g
  | BinOp (_,g,h) -> (get_all_terms_fip cond g) @ (get_all_terms_fip cond g)
  | LogConst _ | LogVar _  -> []
  | Formula(AtomFormula (_, lst)) -> get_all_terms_termlist cond lst
;;
      

let rec get_metavars_fip f =
  List.map (function MetaVar m -> m | _ -> "")
  (Utils.rem_dublicates
    (get_all_terms_fip (function MetaVar _ -> true | _ -> false) f )
  )
;;

let get_metavars_fiplist lst = 
   let res = List.fold_left (fun acc x -> List.append acc (get_metavars_fip x)) [] lst in
     Utils.rem_dublicates (res)
;;

let rec getFree_term t : string list =
  match t with 
    | SubjVar var -> [var]
    | MetaVar var -> []
    | FuncSymbol (n,lst)  -> List.concat (List.map (getFree_term) lst );  
    | SubjConst _  -> []
  ;;

let rec getAllFS_fip f =
  let ans1 = get_all_terms_fip (function FuncSymbol(_,_) -> true | _ -> false) f in
  List.map (function FuncSymbol (name,_) -> name | _ -> "") (Utils.rem_dublicates ans1)
;;


exception ThereAreRulesToApply of string*fip list

let rec get_all_vars_in_term t = match t with
  | SubjVar x  
  | MetaVar x -> [x]
  | FuncSymbol (_,lst) -> (
                      let ans =  List.fold_left (fun x y -> List.append x (get_all_vars_in_term y)) [] lst
                      in Utils.rem_dublicates ans
                      )  
  | SubjConst _ -> []
;;  
  
let rec get_all_var f =
  let r = match f with
    | LogVar _  
    | LogConst _ -> []
    | Formula(AtomFormula (_,lst)) -> List.concat (List.map (get_all_vars_in_term) lst)
    | BinOp (_,g,h)  -> List.append (get_all_var g) (get_all_var h)
    | UnOp(_,g) -> get_all_var g
    | QALL (v,f)
    | QEXZ (v,f)  -> v :: (get_all_var f)
  in
    Utils.rem_dublicates r
;;

module MetaVarM = struct
  type t = MetaVar of string
  let compare (MetaVar a:t) (MetaVar b:t) = String.compare a b  
end;;

module UniAns = struct 
  include Map.Make (MetaVarM)
  let print dic foo = iter (fun (MetaVarM.MetaVar key) v ->  
    print_endline (key ^ "=>" ^ (foo v) ) ) dic
end;;
type genans = term list UniAns.t;;

let map_assoc x m =
  try Some (UniAns.find x m)
  with Not_found  -> None;; 

let rec term_to_pretty_string = function
   | SubjVar name
   | SubjConst name
   | MetaVar name -> name
   | FuncSymbol (name, lst) -> name^"("^(Utils.comma_separated_list (term_to_pretty_string) lst)^")"
;;

let rec fip_to_pretty_string  f =
  let tostr = fip_to_pretty_string in
  let norm_op o = match o with
  | "AND" -> " & "
  | "OR"  -> " OR "
  | "IMPL"-> " => "
  | "EQV" -> " == "
  | "NOT" -> "!"
  | _ -> o in
  let prior_f o = match o with
  | LogConst _ | LogVar _ | Formula _ | UnOp (_,_) -> 100
  | BinOp(s,_,_) when (s="IMPL" || s="EQV") -> 5
  | BinOp(s,_,_) when (s="OR" || s="AND") -> 3
  | QALL(_,_) | QEXZ (_,_) -> 0
  | BinOp (_,_,_) -> ( print_endline "Unknown operation. priority=0"; 0 )
  in
  match f with
    | LogConst True  -> "TRUE"
    | LogConst False -> "FALSE"
    | LogVar x  -> x
    | UnOp (name, BinOp(n2,h1,h2)) -> 
         (norm_op name)^"("^(tostr (BinOp(n2,h1,h2)) )^")"
    | UnOp (name, h) -> 
         (norm_op name) ^ (tostr h)
    | Formula (AtomFormula (name,lst)) -> 
         name^"("^(Utils.comma_separated_list (term_to_pretty_string) lst)^")"
    | QALL (v, Formula g) -> 
           " ALL "^v^" "^(tostr (Formula g))
    | QEXZ (v, Formula g) -> 
           " EXZ "^v^" "^(tostr (Formula g))
    
    | QALL (v, g) -> " ALL "^v^" ("^(fip_to_pretty_string g)^")"
    | QEXZ (v, g) -> " EXZ "^v^" ("^(fip_to_pretty_string g)^")"
    
    | BinOp (n,g,h) -> (
       ( if prior_f g =100 then tostr g else ("("^(tostr g)^")") ) ^ (norm_op n) ^
       ( if prior_f h =100 then tostr h else ("("^(tostr h)^")") )
    )
;;
let t2s = term_to_pretty_string;;
let rec is_sub_term what where = 
(*  print_endline ("is_sub_term "^(t2s what) ^ " in " ^ (t2s where) ); *)
  match where with
  | u when u=what -> true
  | SubjVar _ | MetaVar _ | SubjConst _ -> false
  | FuncSymbol (_,lst) 
    -> List.fold_left (fun acc x -> acc || (is_sub_term what x)) false lst
;;
