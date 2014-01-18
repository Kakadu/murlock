val comma_separated_list : ('a -> string) -> 'a list -> string
val char_separated_list : char ->  ('a -> string) -> 'a list -> string
val string_separated_list : string ->  ('a -> string) -> 'a list -> string
val rem_dublicates : 'a list -> 'a list
val rem_dublicates_cmp : ('a -> 'a -> int) -> 'a list -> 'a list

val mul_cartesian : 'a list -> 'b list -> ('a * 'b) list
val assoc_opt : 'a -> ('a * 'b) list -> 'b option
val cortes : 'a list -> 'a list list -> 'a list list
val get_pairs : 'a list list -> 'a list list

val trim: string -> string

val selectFirstFromList: ('a -> bool) -> 'a list -> 'a option
val iterAllListList    : 'a list list -> ('a list -> unit) -> unit
val iterAllarrarr      : 'a array array -> ('a array -> unit) -> unit
val applyF2arrarr      : 'a array array -> ('a array -> 'b) -> ('b -> bool)  -> 'b option
val applyF2listlist    : 'a list  list  -> ('a list  -> 'b) -> ('b -> bool) -> 'b option

val aggr_listlist : 'a list list -> 'b -> ('a -> 'b -> 'b option) -> 
  ('b -> bool) ->  'b option
