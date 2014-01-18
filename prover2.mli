open Bigseq
open Lang
open Proprules

type provingResult = AlwaysTrueFip | DepthLimit
                   | SubstitutionFounded of (Lang.term*Lang.term)list 

type treeItem = | Info of string	
		| Node of nodeContent
and nodeContent = { seq:bigSeq;
		    id:string;
		    mutable fip:(Lang.fip*Bigseq.seqHalf) option;
		    mutable rulename:string;
		    mutable sons:treeItem list;
		    mutable temp_unifs:int list;
		    mutable norm_unifs:int list;
		    mutable subs: ((term*term) list) option;
		    mutable new_term: Lang.term option

};;

val print_deriv_full : out_channel -> treeItem -> unit
val print_deriv_light: out_channel -> treeItem -> unit

val prove_classic    : Config.prove_config -> fip -> treeItem * provingResult * string
val prove_liberalised: Config.prove_config -> fip -> treeItem * provingResult * string
val unif_every_minus : Bigseq.seqHalf -> fip -> bool
val unif_never       : 'a -> 'b -> bool
