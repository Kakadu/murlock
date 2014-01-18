let char_separated_list c tostr = function
    | [] -> ""
    | t::ts -> List.fold_left (fun s x -> s ^ (Char.escaped c)^" " ^ tostr x) (tostr t) ts
;;

let string_separated_list delim tostr = function
    | [] -> ""
    | t::ts -> List.fold_left (fun s x -> s ^ delim ^" " ^ tostr x) (tostr t) ts
;;

let comma_separated_list tostr = 
  char_separated_list ',' tostr 
;;

let rem_dublicates_cmp cmp lst = 
  let ans = ref [] in
  let rec rem_loop i = 
    if i>=List.length lst then
      ()
    else
      let cur_el = List.nth lst i in
      let ff = List.filter (fun x -> 0 <> (cmp x cur_el)) !ans in
        ans := ( List.append (cur_el::[]) (ff) );
        rem_loop (i+1);
  in rem_loop 0;
     List.rev (!ans)
;;

let rem_dublicates lst = 
  rem_dublicates_cmp (fun x y -> compare x y) lst
;;


let assoc_opt key alist = 
    try Some (List.assoc key alist) 
    with Not_found -> None
;;

let rec mul_cartesian l1 l2 =
(*        match l1 with
        | []    -> []
        | t::ts -> List.append (List.map (fun x -> (t,x)) l2) (mul_cartesian ts l2) *)
  let acc = ref [] in
  List.iter (fun x -> 
    List.iter (fun y -> acc := (x,y) :: !acc  ) l2
  ) l1;
  !acc
;;

let rec cortes a b = 
    match a with 
    | []    -> []    
    | h::tl -> List.append (List.map (fun x -> h::x) b) (cortes tl b);;
	       
let get_pairs lst =
    List.fold_right (fun x y -> cortes x y ) lst [[]];;

let trim str =   if str = "" then "" else   
  let search_pos init p next =
    let rec search i =
      if p i then raise(Failure "empty") else
      match str.[i] with
      | ' ' | '\n' | '\r' | '\t' -> search (next i)
      | _ -> i
    in
    search init   
  in   
    let len = String.length str in  
    try
      let left = search_pos 0 (fun i -> i >= len) (succ)
      and right = search_pos (len - 1) (fun i -> i < 0) (pred)
      in
        String.sub str left (right - left + 1)       
    with   | Failure "empty" -> "" ;;

let applyF2arrarr   (lst: 'a array array) (f:'a array -> 'b) (cond: 'b -> bool) 
    : 'b option = 

  let lengths = Array.map (Array.length) lst in
  let    n    = Array.length lst in
  let indexes = Array.init n     (fun _ -> 0) in
  let maxcount = Array.fold_left (fun acc x -> acc*x) 1 lengths in
  print_endline ("maxiters=" ^ (string_of_int maxcount));
(*    let _ = read_line () in *)
  let check_end () =
    let rec check_inner i =
      if (i>= n) then true else
      if (indexes.(i) + 1  == lengths.(i) )  then  check_inner (i+1)  else
      false
    in check_inner 0
  in
  let i = ref 0 in 
  let extract_target () = 
     i:=!i + 1;
(*     print_string "Fucking shit"; *)
    if (!i mod 100 = 0) then
     print_endline ("i        = " ^(string_of_int !i));
(*     print_endline ("maxcount = " ^(string_of_int maxcount));
     *)
    if (!i >= 1+  maxcount ) then (
            print_endline "the doors"
            
    );
      
(*    print_endline ("next target "^ string_of_int( !i)); *)
    Array.mapi (fun i j -> lst.(i).(j) ) indexes
  in
  let next_target () = begin
    let rec loop i = 
      if (i<0) then () else
      if (indexes.(i) < lengths.(i) - 1) then
        indexes.(i) <- indexes.(i) + 1
      else ( indexes.(i) <- 0;
             loop (i-1)
           )
    in loop (n-1)
  end
  in
  let rec loop () = 
    let cur_target = extract_target () in
    let res = f cur_target in
    if (!i mod 150 = 0) then print_endline(" "^(string_of_int !i));
    if (!i >= maxcount) then ( 
       let x = ref 5 in 
       x:=1 + !x; 
       () 
    );
    match cond res with 
    | true -> Some (res)
    | false-> if check_end () then None else ( next_target (); loop () )
  in
    loop ()
;;

let applyF2listlist (lst: 'a list list)   (f:'a list  -> 'b) (cond: 'b -> bool) 
    : 'b option = 
  applyF2arrarr
    (Array.of_list (List.map (fun x -> Array.of_list x) lst) )
    (fun x -> f (Array.to_list x) )
    cond
;;
 
let iterAllarrarr aa f = 
  match applyF2arrarr aa f (fun _ -> false) with
    | _ -> ()
;;

let iterAllListList lst f = 
  match applyF2listlist lst f (fun _ -> false) with
    | _ -> ()
;;

exception SelectResult;;

let selectFirstFromList pred lst = 
  let res = ref None in
  try 
    List.iter (fun x -> if pred x then (res:= Some x; raise SelectResult)) lst;
    None
  with (SelectResult) -> ();
  !res
;;

exception MiterEnd
let aggr_arrarr (lst:'a array array) (init:'b) (func:'a -> 'b -> 'b option) 
                (ok_func: 'b -> bool) : 'b option = 
    let lengths = Array.map (Array.length) lst in
(*    Array.iter (fun x -> print_int x) lengths;
    print_endline ""; *)
    let    n    = Array.length lst in
    let curtarget = Array.init n (fun _ -> 0) in
    let tempanses = Array.init n (fun _ -> init) in
    let curindex = ref 0 in
    let curel () = (
      lst.(!curindex).(curtarget.(!curindex)) 
    ) in
    let count =  ref 1 in     
    let rec next_target () = 
      let j = curtarget.(!curindex) in
      if j+1 < lengths.(!curindex) then (
	curtarget.(!curindex)<- 1+ curtarget.(!curindex);
        for k= 1+ !curindex to n-1 do curtarget.(k) <- 0 done;
	count:= 1+ !count; 
        if !count mod 1000 = 0 then
	  print_string (" "^(string_of_int !count));
      ) else (
         if 0= !curindex then raise MiterEnd else (
	   curindex := !curindex -1; next_target ()
         )
      )
    in
    try
      let rec loop () = 
	let newres = func (curel ()) tempanses.(!curindex)  in 
	match newres with
	  | Some subs when (1+ !curindex = n) -> begin
	      if (ok_func subs) then Some subs
	      else (next_target (); loop ())
	    end
	  | Some subs -> (
              curindex := 1+ !curindex; 
	      tempanses.(!curindex) <- subs; 
	      loop ()
          )
	  | _ -> (next_target (); loop () )	    
      in
      loop ()
    with MiterEnd -> None
;;

let aggr_listlist (lst:'a list list) (init:'b) (func:'a -> 'b -> 'b option) 
                  (ok_final:'b -> bool) : 'b option = aggr_arrarr
  (Array.of_list (List.map (fun x -> Array.of_list x) lst) )
  init func ok_final
;;

let aggregate_array = aggr_arrarr;;
