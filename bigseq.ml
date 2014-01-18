open Proprules 
open Lang
module Aux = Utils;;

type seqHalf = Anticedent | Succedent;;

exception UnknownOperator of string
exception WrongRulesLevel of int
exception NeedUnification 
exception RuleFound
exception Level4Answer of 
  (sequence*seqHalf*fip* (fip*int)list *string*(term )*(Proprules.sequence)) option

exception SequenceInitialization

(* head element is empty rules list cause no rules can be applied for AtomFormulas *)
let rules = [([],[]); PredRules.rules1; PredRules.rules2];;

class bigSeq init =  object (self)
      
      val mutable seqs = [{anti=[]; suc=[] }; (* atomic formulas. No rules to apply *) 
                          {anti=[]; suc=[] }; (* 1-premise rules *)
                          {anti=[]; suc=[] }; (* 2-premise rules *)
                          {anti=[]; suc=[] }; (* EXQ--> and -->ALL*)
                          {anti=[]; suc=[] }  (* minus-rules:   -->EXQ and ALL-->*) 
                         ]

      method get_atomic_fips = List.nth seqs 0
      method has_atomic_fips = not ((List.nth seqs 0) = {anti=[];suc=[]})
      method is_empty = List.fold_left (fun acc x -> acc && (x={anti=[];suc=[]})) true seqs

      method get_seqs_types_count = List.length seqs
      
      val mutable minus_rule_apply_count = []
      method  get_minus_rule_apply_count = minus_rule_apply_count
      method  clr_minus_rule_apply_count = minus_rule_apply_count <- []

      method  get_dead_minuses = List.fold_left 
         (fun acc x -> if 0 = snd x then acc+1 else acc) 0 minus_rule_apply_count

      method is_axiom = List.fold_left (fun acc x -> 
         acc || (List.fold_left (fun acc y-> acc || (List.mem y x.suc)) false x.anti)
      ) false seqs 

      method has_rules_to_apply =
        let minuses = List.nth seqs 4 in
        ({anti=[];suc=[]} <> List.nth seqs 1) ||
        ({anti=[];suc=[]} <> List.nth seqs 2) ||
        ({anti=[];suc=[]} <> List.nth seqs 3) ||
        (List.fold_left (fun acc x -> match Aux.assoc_opt x minus_rule_apply_count with
                                     Some x when x > 0 -> true
                                   | _ -> acc ) false (List.append minuses.anti minuses.suc))


      
      method get_seqs = seqs
      (* Convert to one normal Proprules.sequence  *)
      method get_normal_seq =
        let ans = {anti=[]; suc=[] } in
           List.iter (fun x  -> ans.anti <- List.append ans.anti x.anti;
                                ans.suc  <- List.append ans.suc  x.suc ) seqs;
           ans

      val mutable all_metavar_names : string list = [] 

      method to_ch ch =
        List.iter (fun x  ->  
        Printf.fprintf ch "\t%s --> %s\n" 
             (Aux.comma_separated_list fip_to_pretty_string x.anti) 
             (Aux.comma_separated_list fip_to_pretty_string x.suc) 
        ) seqs;
        Printf.fprintf ch "dead minuses count: %d\n" self#get_dead_minuses;
        Printf.fprintf ch ";;\n"

      method to_string =
        List.fold_left ( fun s x  -> s^
           (Proprules.string_of_sequence x)^"\n"       
        ) "" seqs                               
                                      
      (* *)                                            
      method apply_rules level (get_new_subj:sequence*fip->term) 
                               (get_new_meta:sequence*fip->term)
                       =
          let apply_rules_to_fip rules f = 
            let rec loop i =
               if i>= List.length rules then
                 None
               else 
                 let ans = (List.nth rules i) f in
                 if (List.length (fst ans))>0 then
                   (Some ans)
                 else loop (i+1)
            in loop 0;
          in
          if (level<0) or (level>5) then raise (WrongRulesLevel level)
          else
            let curseq = List.nth seqs level in
            match level with
            | 0 -> None (* no rules can be applied to atomic formulas. *)
            | 1 | 2 -> (
              (* Formula of proposal logic  *)
              let iter_seq rules =
                (* build answer after iteration anticedent *)
                let anti_adder head lst tail =
                  let res = List.map ( fun seqitem  -> 
                    {anti= List.append head (List.append  seqitem.anti tail); 
                     suc = List.append curseq.suc seqitem.suc }
                    ) lst
                  in
                  res
                in
                (* build answer after iteration succedent *)
                let suc_adder head lst tail : Proprules.sequence list =
                  let res = List.map ( fun seqitem  -> 
                    {suc = List.append head (List.append  seqitem.suc tail); 
                     anti= List.append curseq.anti seqitem.anti }
                    ) lst
                  in 
                    res
                in
                
                let rec loop_fips head item tail rules adder  =
                  let next_iteration = 
                    match tail with
                    | [] -> None
                    | _  -> loop_fips (List.append head (item::[])) (List.hd tail) (List.tl tail) rules adder
                  in 
                    let ans = apply_rules_to_fip rules item in
                    match ans with
                    | None -> next_iteration
                    | Some (lst,rule) -> 
                        (* sequence list * fip * string * fip list *)  
                        Some (adder head (lst) tail, item, rule, lst)
                in (
                  let ans = ref None in
                  if (List.length curseq.suc >0) then (
                    let ans_suc = loop_fips [] (List.hd curseq.suc) (List.tl curseq.suc) (snd rules) suc_adder in
                    match ans_suc with
                    | None -> ()
                    | Some (seqlst, f, rule, simpleres) -> 
		      ans:= Some(seqlst, Succedent, f, rule,  simpleres)
                  ) else if (List.length curseq.anti>0) then (
                    match (loop_fips [] (List.hd curseq.anti) (List.tl curseq.anti) 
                                   (fst rules) anti_adder) with
                    | None -> ()
                    | Some(lst, f, rule,simpleres) -> 
                         ans:= Some(lst, Anticedent, f, rule, simpleres )
                  ); 
                  !ans 
                ) 
              in (
                match iter_seq (List.nth rules level) with
                | None -> None
                | Some (ss,part, ff, rule,simpleres) -> begin  
                  (* now we have a list of future leaves*)
                  let ans_seqs = List.map (fun x  -> (
                      let res = ref {anti=[];suc=[]} in
                      (*level = 1 or 2 there *)
                      let rec loop i =
                        if (i>= self#get_seqs_types_count) then () else begin
                        if (i = level) then ( 
                          !res.anti<- List.append !res.anti x.anti;
                          !res.suc <- List.append !res.suc  x.suc;
                        ) else (
                          !res.anti<- List.append !res.anti (List.nth seqs i).anti;
                          !res.suc <- List.append !res.suc  (List.nth seqs i).suc;
                        );
                        loop (i+1)
                        end
                      in 
                        loop 0;
                        !res
                    ) ) ss in
                    let big_res = List.map (fun x  -> new bigSeq (x,minus_rule_apply_count) ) ans_seqs in
                    Some (big_res,part,ff,rule,None,simpleres)
                end
                )
            )
            | 3     -> ( 
              (* rules level 3: simple quantifier rule -->ALL and EXZ--> *)
              (* new unique term will be created.*)
              try
                let rec loop_anti head g tail = 
                  match g with
                    | QEXZ (name, f)  -> (
		      (*                      self#to_ch stdout; *)
                      let new_subjvar = get_new_subj (self#get_normal_seq,g) in
                      let newfip = (PredRules.substitute_fip f (Lang.SubjVar name) new_subjvar)
                      in 
                      	raise (Level4Answer(Some(
                             {anti=List.append head (newfip::tail); suc=curseq.suc},
                             Anticedent, g, minus_rule_apply_count, 
                             "EXZ-->", new_subjvar, 
			     {anti=[newfip];suc=[]} )) )
                    )
                    | _ -> if (List.length tail=0) then ()
                           else loop_anti (List.append head (g::[])) (List.hd tail) (List.tl tail)
                in 
                  if List.length curseq.anti>0 then 
                    loop_anti [] (List.hd curseq.anti) (List.tl curseq.anti);
                    
                let rec loop_suc head g tail = ( 
                  match g with
                    | QALL (name, f)  -> (
(*                      self#to_ch stdout; *)
                      let new_subjvar = get_new_subj (self#get_normal_seq,g) in
                      let newfip = (PredRules.substitute_fip f (Lang.SubjVar name) new_subjvar)
                      in 
                      	raise (Level4Answer(Some(
                           {suc=List.append head (newfip::tail); anti=curseq.anti},
                           Succedent, g, minus_rule_apply_count, 
                           "-->ALL",new_subjvar,{suc=[newfip];anti=[]})) )
                    )
                    | _ -> if (List.length tail=0) then ()
                           else loop_anti (List.append head (g::[])) (List.hd tail) (List.tl tail)
                ) in 
                  if List.length curseq.suc>0 then 
                    loop_suc [] (List.hd curseq.suc) (List.tl curseq.suc);
                raise (Level4Answer None) 
              with Level4Answer None -> None
                 | (Level4Answer (
		   Some (res,part,ff,counts,rule,var, newfip))) ->  (
                  
	           let ans = {anti=[];suc=[]} in
	           for i=0 to 4 do (
	               match i with
	               | 3 -> (
                         ans.suc <- List.append ans.suc res.suc;
                         ans.anti<- List.append ans.anti res.anti;
	               )
		       | _ -> (
		          ans.suc  <- List.append ans.suc  (List.nth seqs i).suc;
		          ans.anti <- List.append ans.anti (List.nth seqs i).anti
		       )
		    ) done;
                    let big_res = [new bigSeq (ans,counts)] in
                    Some (big_res,part,ff, rule, Some var, [newfip])
              )    
            )
            | 4  -> (            
              try
                (*iterate anticedent *)
                List.iter (fun x  -> 
                   match (Aux.assoc_opt x minus_rule_apply_count) with
                    | None -> (
                      (* скорее всего так не будет *)
                      let new_minus_count = (x,0)::
                        (List.filter (fun z -> not (x=fst z)) minus_rule_apply_count) in
                    let new_term = get_new_meta (self#get_normal_seq,x) in
                    match x with
                        | QALL (v,g)  -> begin 
	           	    let newfip  = PredRules.substitute_fip g (SubjVar v) new_term in
                            raise (Level4Answer (Some (
                                     {anti= newfip::curseq.anti; suc = curseq.suc},
                                     Anticedent, x, new_minus_count,
                                     "ALL -->", new_term, 
				     {anti=[newfip];suc=[]}) ) )
			  end
                        | _ -> (Log.put "ERROR in level 4\n";
                                raise SequenceInitialization ) 
                    ) 
                    | Some 0  -> ()
                    | Some z -> (
                    let new_minus_count = (x,z-1)::
                         (List.filter (fun y -> not (fst y=x)) minus_rule_apply_count) in
(*                    self#to_ch stdout;*)
                    let new_meta = get_new_meta (self#get_normal_seq,x) in
		    
                    let postfix = match x with
                    | QALL (v,g)  -> begin 
                        let newfip  = PredRules.substitute_fip g (SubjVar v) new_meta in
                        (newfip, v)
                      end
                    | _ -> (Log.put "ERROR in level 4\n"; raise SequenceInitialization ) 
                        in
                          raise (Level4Answer (Some (
                            {suc =curseq.suc; anti=(fst postfix)::curseq.anti},
                            Anticedent, x,
                            new_minus_count,
                            "ALL -->", new_meta,
			    {anti=[fst postfix];suc=[]} ) ) )
                    )
                ) curseq.anti;
                (* the same for succedent *) 
                List.iter (fun x  -> 
                   match (Aux.assoc_opt x minus_rule_apply_count) with
                    | None -> (
                      (*скорее всего так не будет *)
                    let new_minus_count = (x,0)::
                        ((List.filter (fun z-> not(x=fst z)) minus_rule_apply_count)) in
                    let new_meta = get_new_meta (self#get_normal_seq,x) in
                    let postfix = match x with
                        | QEXZ (v,g)  -> begin
	               	    let newfip = PredRules.substitute_fip g (SubjVar v) new_meta in
                            (newfip,v)
		        end
                        | _ -> (Log.put "ERROR in level 4\n"; raise SequenceInitialization ) 
                      in
                      raise (Level4Answer (Some (
                        {anti=curseq.anti; suc =(fst postfix)::curseq.suc},
                        Succedent, x,       
                        new_minus_count,
                        "--> EXZ", new_meta,
			{suc=[fst postfix];anti=[]} ) ) )
                    ) 
                    | Some 0  -> ()
                    | Some z -> (
                      let new_minus_count = (x,z-1)::
                      (List.filter (fun y  -> not (fst y = x)) minus_rule_apply_count) in
(*                      print_endline "get new metavar for:";
                      self#to_ch stdout; *)
                        
                      let new_meta = get_new_meta (self#get_normal_seq,x) in
                      let postfix = match x with
                          | QEXZ (v,g)  -> (
                             let subst_res  = PredRules.substitute_fip g (SubjVar v) new_meta in
                             (subst_res, v)
                          )
                          | _ -> (Log.put "ERROR in level 4\n";
                                raise SequenceInitialization ) 
                      in
                        raise (Level4Answer (Some (
                          {suc =(fst postfix)::curseq.suc; anti=curseq.anti},
                          Succedent,x,
                          new_minus_count,
                          
                          "--> EXZ", new_meta,
			  {suc=[fst postfix];anti=[]} ) ) )
                    ) 
                ) curseq.suc;
                raise (Level4Answer None) 
              with Level4Answer None -> None
                | (Level4Answer (Some (res,part,f,counts,rule,var, subst_res))) ->  (
                   (
		   let ans = {anti=[];suc=[]} in
		     for i=0 to 3 do (
		       ans.suc  <- List.append ans.suc  (List.nth seqs i).suc;
		       ans.anti <- List.append ans.anti (List.nth seqs i).anti;
		     ) done;
		     ans.suc <- List.append ans.suc res.suc;
		     ans.anti<- List.append ans.anti res.anti;
                     Some ([new bigSeq (ans,counts)],part, f, rule,Some var, [subst_res])
            )  )
     
           )
            | _  -> raise (WrongRulesLevel level);
               
      method apply_all_rules (get_new_subjvar_name : sequence*fip -> term) 
                             (get_new_metavar_name : sequence*fip -> term) =
        let res = ref None in
        let levels = [0;1;3;4;2] in
        let rec loop i =
          if i>=List.length seqs then ()
          else 
            let ans = self#apply_rules (List.nth levels i) get_new_subjvar_name get_new_metavar_name in
            match ans with 
            | None -> loop (i+1)
            | Some (lst,part,f,rule,var, newfip) -> res := ans
          in 
          loop 1;
          !res
            
      (*небезопасный метод. Может нарушить процесс вычислений*)
      method set_seqs (a,b,c,d) = 
        seqs <- a::b::c::d::[]
        
      initializer
        match init with
          | (init_seqs,init_apply_count) -> (
             
            let seq0 = List.nth seqs 0
            and seq1 = List.nth seqs 1
            and seq2 = List.nth seqs 2
            and seq3 = List.nth seqs 3 
            and seq4 = List.nth seqs 4 in

            List.iter (fun x  -> match x with
            | LogVar _ 
            | LogConst _ 
            | Formula _         -> seq0.anti <- x::seq0.anti
            | BinOp ("AND",_,_)   
            | UnOp  ("NOT",_)   -> seq1.anti <- x::seq1.anti
            | BinOp ("OR",_,_)  
            | BinOp ("IMPL",_,_)
            | BinOp ("EQV",_,_) -> seq2.anti <- x::seq2.anti 
            | QEXZ  (_,_)       -> seq3.anti <- x::seq3.anti
            | QALL  (_,_)       -> seq4.anti <- x::seq4.anti
            | _  -> raise (UnknownOperator (fip_to_pretty_string x))
            ) init_seqs.anti;
            List.iter (fun x  -> match x with 
            | LogVar _ 
            | LogConst _ 
            | Formula _         -> seq0.suc <- x::seq0.suc
            | BinOp ("OR",_,_)
            | BinOp ("IMPL",_,_)
            | UnOp  ("NOT",_)   -> seq1.suc <- x::seq1.suc
            | BinOp ("EQV",_,_)
            | BinOp ("AND",_,_) -> seq2.suc <- x::seq2.suc
            | QALL  (_,_)       -> seq3.suc <- x::seq3.suc
            | QEXZ  (_,_)       -> seq4.suc <- x::seq4.suc
            | _  -> raise (UnknownOperator (fip_to_pretty_string x))
            ) init_seqs.suc;
            
            seqs <- List.map (fun x -> {anti=Utils.rem_dublicates x.anti;
                                        suc =Utils.rem_dublicates x.suc}) seqs;         
            minus_rule_apply_count <- init_apply_count;
            List.iter (fun x  -> match Aux.assoc_opt x minus_rule_apply_count with
              | None -> minus_rule_apply_count <- (x,1)::minus_rule_apply_count
              | Some _ -> ()  
            ) (List.append seq4.anti seq4.suc);
            
            all_metavar_names <- (Lang.get_metavars_fiplist 
                 (List.append init_seqs.suc init_seqs.anti) );
         )
end;;
