
type sequence = { mutable anti: (Lang.fip list);
                  mutable suc : (Lang.fip list) }

open Lang;;
val string_of_sequence : sequence  -> string
val print_sequence : sequence -> unit

val get_metavars_seq:sequence -> string list

val map_sequence_terms: (term->term) -> sequence -> sequence 
