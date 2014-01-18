open Superseq
open KonevTree
open Lang
open Proprules

let last_id = ref 0;;
let getNextID () = 
        last_id := !last_id +1;
        string_of_int !last_id
;;
let getStartNode sseq = 
        { id=getNextID (); seq=sseq; level=0; father=None; role=Some AND;
          sons = []; forb = UniAns.empty; status = Pending; ans=None
        }
;;
let nList = ref [];;
let leaves = ref [];;

exception MetaVarsConflict of Lang.term

let simple_combine (lst: (term*term) list)  =
  
  None
;;

let combineOR (lst: answerSystem list) =
  let p1 = ref UniAns.empty in
  let p2 = List.fold_left (fun acc x -> begin
    match x.pairs with None -> List.append acc x.pointers
      | Some dic -> (
      UniAns.iter (fun k v ->
        try match UniAns.find k !p1 with 
          | _ -> raise (MetaVarsConflict (match k with MetaVarM.MetaVar u -> MetaVar u))
        with Not_found -> p1:= UniAns.add k v !p1
      ) dic; 
      List.append acc x.pointers )
    end
  ) [] lst in
  
  {pairs=Some (!p1); pointers=Utils.rem_dublicates p2}
;;

let combineAND (lst: answerSystem list) = 
  let p = List.fold_left (fun acc x -> List.append acc x.pointers) [] lst in
  let s = ref [] in
  forAllASList lst (fun x -> match simple_combine x with None -> () 
                      | Some res -> s:= res :: !s );
  
        {pairs=None; pointers=[]}
;;

exception AnswerSystemNone of treeNode
exception NodeRoleIsUndefined of treeNode

let compute_value node = match node.role with
  | None -> raise (NodeRoleIsUndefined node)
  | Some AND -> (
      let hasFailed = List.fold_left (fun acc x -> x.status=Failed  or acc) false node.sons in
      let hasPending= List.fold_left (fun acc x -> x.status=Pending or acc) false node.sons in
      match (hasFailed,hasPending) with
	| (true,_) -> node.status <- Failed  
        | (_,true) -> node.status <- Pending
        | _ -> begin
            let ansLst = List.map (fun x -> match x.ans with Some y -> y 
                                                           | None -> raise (AnswerSystemNone x) ) node.sons in
            let res =  combineAND ansLst in
            match res with
              | {pairs=None; pointers=[]} -> node.status <- Failed
              | {pairs=None; pointers=a } -> ( node.status<-Pending; nList:=List.append !nList a )
              | {pairs=Some _; pointers=_} -> (node.status <- Proved; node.ans <- Some res )
        end

  )
  | Some OR -> (
      let hasProved = List.fold_left (fun acc x -> x.status=Proved or acc) false node.sons in
      let allFailed = List.fold_left (fun acc x -> acc && x.status=Failed) true  node.sons in
      match (hasProved, allFailed) with
	| (_,true) -> node.status <- Failed
        | (false,_)-> node.status <- Pending
        | _        -> begin
	    let provedNodes = List.filter (fun x -> x.status=Proved) node.sons in
            let pendingNodes= List.filter (fun x -> x.status=Pending)node.sons in
            let targets = List.fold_left (fun acc x -> match x.ans with Some y -> y::acc | None -> acc) [] provedNodes in
            node.ans <- Some (combineOR ({pairs=None;pointers=pendingNodes} :: targets));
	    node.status <- Proved
        end
  )  
;; 

let lastSubjVarN = ref 0;;
let getNewSubj metasNameList =
   lastSubjVarN := 1 + !lastSubjVarN;
   "z"^(string_of_int !lastSubjVarN)
;;

let lastMetaVarN = ref 0;;
let getNewMeta () =
   lastMetaVarN := 1 + !lastMetaVarN;
   "__m"^(string_of_int !lastMetaVarN)
;;

let proofSearch f : treeNode = 
  let selectLeafStrategy () =
    match Utils.selectFirstFromList  (fun x -> (x.status = Pending) &&
                                                hasFather (fun y -> match y.status with 
                                                           | Proved-> true 
							   |  _    -> false ) x
          ) !leaves with
      | Some x -> Some x
      | None -> 
	  Utils.selectFirstFromList (fun x -> hasFather (fun y -> List.mem y !nList) x) !leaves	
  in
  let firstSequence = new superSeq ({anti=[];suc=[f]}, UniAns.empty,[]) in
  let root = getStartNode firstSequence in
  nList := [];
  leaves := [root]; 
  while (root.status = Pending) do 
    leaves := List.sort (fun x y-> compare x.level y.level) !leaves;
    match selectLeafStrategy () with None -> () | Some curleaf -> begin
      match curleaf.seq#apply_all_rules getNewSubj getNewMeta with
	| None -> curleaf.status <- Failed
        | Some (lst,part,ff,rule,_) -> begin
            let newNodes = List.map (fun x -> newSon curleaf (getNextID) x ) lst in
            leaves := List.append newNodes (List.filter (fun x -> x.id <> curleaf.id) !leaves);
            let newLeaves = ref [] in
            (* now let's determine final sequences*)
	      let rec iter_nodes lst = 
		match lst with [] -> () | h::tl -> begin
		   match Unif.fullUnify [h.seq#get_normal_seq] (h.forb) with
                     | (false,_) -> (newLeaves := h :: !newLeaves;
                                     iter_nodes tl)
                     | (true,rr) -> begin
                         let nodeProved  = newSon h (getNextID) h.seq in
                         let nodePending = newSon h (getNextID) h.seq in
                         nodeProved.status <- Proved;
                         nodeProved.ans <- Some ({pairs=Some (getAnsDicFromList rr); pointers=[]});
			 nodePending.status <- Pending;
                         h.role <- Some OR;
                         iter_nodes ( nodePending::tl)
                     end
                end
              in
                iter_nodes newNodes;
		let fathers = (List.map (fun x -> x.father) !newLeaves)in
                let targets = List.fold_left (fun acc x -> match x with Some y -> y::acc | None -> acc) [] fathers in
                List.iter (compute_value) (Utils.rem_dublicates_cmp (fun x y -> compare x.id y.id) targets)
          end
    end    
  done;
  root  
;;


