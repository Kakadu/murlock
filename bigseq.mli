open Proprules
open Lang
exception UnknownOperator of string
exception WrongRulesLevel of int
exception NeedUnification

(* TODO: move this to proprules.ml *)
type seqHalf = Anticedent | Succedent;;

class bigSeq : sequence * (fip * int) list -> 
  object
    method has_rules_to_apply : bool
    method get_atomic_fips    : sequence
    method has_atomic_fips    : bool
    method is_empty           : bool

    method get_seqs_types_count : int 
    method get_normal_seq       : sequence
    method get_minus_rule_apply_count : (fip * int) list
    method clr_minus_rule_apply_count : unit
    method get_dead_minuses : int
  
    method get_seqs       : sequence list
    method set_seqs       : sequence*sequence*sequence*sequence -> unit
    method to_ch          : out_channel -> unit
    method is_axiom : bool
    (** Apply rules of level *) 
    method apply_rules    : int -> (sequence*fip -> term) -> 
                            (sequence*fip -> term) ->
          (bigSeq list * seqHalf * Lang.fip * string *(term option)*sequence list) option
    method apply_all_rules: (sequence*fip -> term) -> (sequence*fip -> term) ->
          (bigSeq list * seqHalf * Lang.fip * string *(term option)* sequence list) option 
    method to_string : string
  end 
 
