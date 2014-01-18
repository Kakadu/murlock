(* get all Subjvars and MetaVars names *)
val get_all_terms_of_fip : Lang.fip -> string list
(* get all Subjvars and MetaVars names *)
val get_all_terms_of_seq : Proprules.sequence  -> string list

val rules1 : (Lang.fip -> (Proprules.sequence list)*string) list *
             (Lang.fip -> (Proprules.sequence list)*string) list
val rules2 : (Lang.fip -> (Proprules.sequence list)*string) list *
             (Lang.fip -> (Proprules.sequence list)*string) list

val substitute_term: Lang.term -> Lang.term -> Lang.term  -> Lang.term             
val substitute_fip : Lang.fip  -> Lang.term -> Lang.term  -> Lang.fip