open Lang
open Superseq

type subsDic = (term list) UniAns.t;;

type nodeRole   = AND | OR;;
type nodeStatus = Proved | Failed | Pending;;

type answerSystem = {
        pairs : term UniAns.t option;
        pointers : treeNode list
} and treeNode = {
        mutable seq   : superSeq;
        mutable role  : nodeRole option;
                id    : string;
                father: treeNode option;
                level : int;
        mutable sons  : treeNode list;
        mutable forb  : subsDic;
        mutable status: nodeStatus;
        mutable ans   : answerSystem option
};;

let newSon fatherNode newID seqq = 
  let son = {seq=seqq; id=newID (); level=fatherNode.level+1; role=None;ans=None;
             father =Some fatherNode; forb=UniAns.empty; status=Pending; sons=[] }
  in 
  fatherNode.sons <- son :: fatherNode.sons;
  son
;;

let rec hasFather pred node =
  match node.father with
    | None   -> false
    | Some x -> if pred x then true else hasFather pred x
;;

let rec firstSon pred node =
  List.fold_left (fun acc x -> match acc with
                    | Some _ -> acc
                    | None   -> (if pred x then Some x 
                                 else firstSon pred x)
		 ) None node.sons;
;;

let getAnsDicFromList lst = 
  List.fold_left (fun acc (x,y) -> match x with 
                    | MetaVar m -> UniAns.add (MetaVarM.MetaVar m) y acc
                    | _         -> acc
  ) UniAns.empty lst 
;;

let forAllASList lst f =
  let target = Array.make (List.length lst) [] in

  let idx = ref 0 in
  List.iter (fun x -> match x.pairs with None -> () | Some dic ->
    UniAns.iter (fun k v -> (
      target.(!idx) <- (match k with MetaVarM.MetaVar m -> MetaVar m,v) :: target.(!idx)
  ) ) dic; idx := 1 + !idx ) lst;

    let target2 = Array.map (fun x -> Array.of_list x) target in
    Utils.iterAllarrarr target2 (fun x -> f (Array.to_list x))

;;
