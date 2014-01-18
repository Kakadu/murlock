
val print_dic : Lang.term list Lang.UniAns.t -> unit
val t2s : Lang.term -> string
exception UnifyError of string
exception UnifAnswer of bool * (Lang.term * Lang.term) list
val get_meta : Lang.MetaVarM.t -> Lang.term
val dic2list : 'a list Lang.UniAns.t -> (Lang.term * 'a) list
exception LiberalisedException of Lang.term
val map_assoc_meta_term : Lang.term -> 'a Lang.UniAns.t -> 'a option
val get_metas_termlist : Lang.term list -> Lang.term list
val key2str : Lang.MetaVarM.t -> string
val key2term : Lang.MetaVarM.t -> Lang.term
val has_cycles : Lang.term list Lang.UniAns.t -> bool

val subs_ans2pairs :
  Lang.genans -> (Lang.term * Lang.term) list -> (Lang.term * Lang.term) list
exception UnifyInternalException of string
exception LibAnswer of Lang.genans option

val unif2fs_lists_lib :
  (Lang.term * Lang.term) list ->
  Lang.term list Lang.UniAns.t -> Lang.term list Lang.UniAns.t option
val unif_abstract3 :
  bool ->
  ((Lang.term * Lang.term) list ->
   Lang.term list Lang.UniAns.t -> Lang.term list Lang.UniAns.t option) ->

  (Lang.term list Lang.UniAns.t -> bool) ->
  Proprules.sequence list -> bool * (Lang.term * Lang.term) list
val unif_graphs_count : int ref
val unify_liberalized :
  Config.prove_config ->
  Proprules.sequence list ->
  Lang.term list Ans_dic.KonevForbs.t -> bool * (Lang.term * Lang.term) list
val unif_pairs_classic :
  (Lang.term * Lang.term) list ->
  Lang.term list Lang.UniAns.t -> (Lang.term list Lang.UniAns.t) option
val unify_classic :
  Config.prove_config ->
  Proprules.sequence list ->
  Lang.term list Lang.UniAns.t -> bool * (Lang.term * Lang.term) list
