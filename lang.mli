
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

val fip_to_pretty_string : fip -> string
val term_to_pretty_string:term -> string

val get_all_var : fip -> string list

val getAllFS_fip : fip -> string list
val get_metavars_fip : fip  -> string list
val get_metavars_fiplist : fip list -> string list

module MetaVarM : sig 
  type t = MetaVar of string 
  val compare : t -> t -> int 
end

module UniAns :
  sig
    type key = MetaVarM.t
    type 'a t = 'a Map.Make(MetaVarM).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val print : 'a t -> ('a -> string) -> unit
  end
;;
type genans = term list UniAns.t;;
val is_sub_term  : term -> term -> bool
exception ThereAreRulesToApply of string*fip list

(*val get_metavar : fip list  -> 'a UniAns.t -> term *)
  
val map_assoc : UniAns.key -> 'a UniAns.t -> 'a option

val get_all_terms_term : (term -> bool) -> term -> term list

val get_all_terms_termlist : (term -> bool) -> term list -> term list

val get_all_terms_fip : (term -> bool) -> fip -> term list

