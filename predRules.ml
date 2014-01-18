open Proprules
open Lang
module Aux = Utils;;

exception GetTermsException of string;;
exception SubstitutionException of string*Lang.term*string

(* get all Subjvars and MetaVars names *)
let rec get_all_terms_of_term t =
  match t with
    | SubjVar v  -> [v]
    | MetaVar m  -> [m]
    | SubjConst _-> []
    | FuncSymbol (_,lst) -> List.concat (List.map (get_all_terms_of_term) lst)
;; 

let rec get_all_terms_of_fip ( f : Lang.fip)  =
  let ans = ref [] in
  let rec get_t = function
      | LogVar _ 
      | LogConst _ -> ()
      | BinOp (_,left,right) -> (get_t left; get_t right)
      | UnOp  (_,arg)        -> get_t arg
      | Formula (AtomFormula (_,lst)) ->
                       List.iter (fun x -> 
                                    ans:=List.append !ans (get_all_terms_of_term x)
                                 ) lst
      | QALL (x, f)  | QEXZ (x, f) -> 
	 ans:= x :: !ans @ (get_all_terms_of_fip f)                       
  in
    get_t f;
    Utils.rem_dublicates !ans
;;

let get_all_terms_of_seq (seq:Proprules.sequence) =
  let ans = ref [] in
  let get_t x = 
    ans:=List.append !ans (get_all_terms_of_fip x)
  in
    List.iter get_t (seq.anti);
    List.iter (get_t) (seq.suc);
    Utils.rem_dublicates !ans
;;

(* t_from  что         *)
(* t_in    вместо чего *)
let rec substitute_term term t_from t_in : Lang.term = 
(*  print_endline ("term: "^ (term_to_pretty_string term));
  print_endline ("old:  "^ (term_to_pretty_string t_from));
  print_endline ("new:  "^ (term_to_pretty_string t_in)); *)
  if term = t_from then t_in
  else  (*здесь есть быдлокод *)
  match term with
    | SubjConst _ -> term
    | SubjVar m 
    | MetaVar m -> if ( m)=(term_to_pretty_string t_from) then
                      t_in
                   else term
    | FuncSymbol (name,lst) -> 
          FuncSymbol(name,List.map (fun x  -> substitute_term x t_from t_in) lst)
;;

(* Substitute to quantifier formula *)
(* t1 - this term will be replaced by t2 *)
(* t2 - new term *)
let rec substitute_q f t1 t2 =
  let is_quan_var = (fun x -> x = term_to_pretty_string t1) in
  match f with
    | QALL (x,g) when is_quan_var x -> f
    | QALL (x,g) -> QALL ( x, substitute_fip g t1 t2)
    | QEXZ (x,g) when is_quan_var x -> f
    | QEXZ (x,g) -> QEXZ ( x, substitute_fip g t1 t2)
    | _  -> raise (SubstitutionException ("in substitute_q: this is not quantor",t1,fip_to_pretty_string f))
  
and substitute_fip f t1 t2  =
  match f with 
    | UnOp (name,g)  -> UnOp (name, substitute_fip g t1 t2)
    | BinOp(name,g,h)-> BinOp(name, substitute_fip g t1 t2, substitute_fip h t1 t2)
    | LogVar v  -> f
    | LogConst c -> f
    | Formula (AtomFormula (name,lst))-> Formula (AtomFormula (name, 
                                         List.map (fun x  -> substitute_term x t1 t2) lst))
    | _ -> substitute_q f t1 t2
;;

let ruleANDsuc (f:Lang.fip)  =
  match f with
    | Lang.BinOp ("AND",a,b) -> 
      ( let s1 = { anti=[]; suc=[a] }
        and s2 = { anti=[]; suc=[b] }
        in
         ( s1::s2::[], "-->&")
      )                                        
    | _  -> ([], "-->&")
;;
let ruleANDanti (f:Lang.fip)  =
  match f with
    | Lang.BinOp ("AND",a,b) -> 
        let s1 = { anti=[a;b]; suc=[] }
        in
         ( [s1],"&-->")
    | _  -> ( [],"&-->")
;;
let ruleNOTanti (f:Lang.fip) =
  match f with
    | Lang.UnOp ("NOT",a) -> 
        let s1 = { anti=[]; suc=[a] }
        in
         ( [s1],"!-->")
    | _  -> ( [],"!-->")
;;
let ruleNOTsuc (f:Lang.fip) =
  match f with
    | Lang.UnOp ("NOT",a) -> 
        let s1 = { anti=[a]; suc=[] }
        in
         (s1::[],"-->!")
    | _  -> ([],"-->!")
;;
let ruleIMPLanti (f:Lang.fip)  =
  match f with
    | Lang.BinOp ("IMPL",a,b) -> 
        let s1 = { anti=[]; suc=[a] }
        in
        let s2 = { anti=[b]; suc=[] }
        in
         (s1::s2::[],"=>-->")
    | _  -> ([],"")
;;
let ruleIMPLsuc (f:Lang.fip) =
  match f with
    | Lang.BinOp ("IMPL",a,b) -> 
        let s1 = { anti=[a]; suc=[b] }
        in
         (s1::[],"-->=>")
    | _  -> ([],"")
;;
let ruleORanti (f:Lang.fip) =
  match f with
    | Lang.BinOp ("OR",a,b) -> 
        let s1 = { anti=[a]; suc=[] }
        in
        let s2 = { anti=[b]; suc=[] }
        in
         (s1::s2::[],"OR-->")
    | _  -> ( [],"")
;;
let ruleORsuc (f:Lang.fip) =
  match f with
    | Lang.BinOp ("OR",a,b) -> 
        let s1 = { anti=[]; suc=[a;b] }
        in
         (s1::[],"-->OR")
    | _  -> ([],"")
;;
let ruleEQVanti (f:Lang.fip)  =
  match f with
    | Lang.BinOp ("EQV",a,b) ->
        let s1 = { anti=[a;b]; suc=[] }
        and s2 = { anti=[]; suc=[a;b] }
        in
         (s1::s2::[],"==-->")
    | _  -> ([],"")
  ;;

let ruleEQVsuc (f:Lang.fip) =
  match f with
    | Lang.BinOp ("EQV",a,b) ->
        let s1 = { anti=[a]; suc=[b] }
        and s2 = { anti=[b]; suc=[a] }
        in
         (s1::s2::[],"-->==")
    | _  -> ([],"")
  ;;


let rules1 = ([ruleNOTanti; ruleANDanti],
              [ruleNOTsuc;  ruleORsuc   ; ruleIMPLsuc]);;
let rules2 = ([ruleORanti;  ruleIMPLanti; ruleEQVanti],
              [ruleANDsuc;                ruleEQVsuc ] )

