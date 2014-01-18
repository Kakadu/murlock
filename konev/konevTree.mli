open Lang
open Superseq

type subsDic = (term list) UniAns.t;;
type nodeRole   = AND | OR;;
type nodeStatus = Proved | Failed | Pending;;
type answerSystem = {
        pairs : term UniAns.t option;
        pointers : treeNode list
} and treeNode = {
        mutable seq : superSeq;
        mutable role : nodeRole option;
                id  : string;
                father: treeNode option;
                level: int;
        mutable sons: treeNode list;
        mutable forb: subsDic;
        mutable status : nodeStatus;
        mutable ans   : answerSystem option
};;

val newSon : treeNode -> (unit -> string) -> superSeq -> treeNode
val hasFather : (treeNode -> bool) -> treeNode -> bool
val getAnsDicFromList : (term*term) list -> term UniAns.t
val forAllASList : answerSystem list -> ((term*term) list -> unit) -> unit
