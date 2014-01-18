let t2s = Lang.term_to_pretty_string ;;

module Term = struct
  type t = Lang.term
  let compare a b = String.compare (t2s a) (t2s b)
end;;

module KonevForbs = struct 
  include Map.Make (Term)
  let print str_of_value d = iter 
     (fun k v -> print_endline ((t2s k)^" => "^(str_of_value v) )) d
end;;

let konev_assoc key dic = 
  try Some (KonevForbs.find key dic)
  with Not_found -> None
;;
