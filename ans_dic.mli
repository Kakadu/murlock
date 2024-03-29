module Term :
  sig type t = Lang.term  
  val compare : Lang.term -> Lang.term -> int
end

module KonevForbs : sig
    type key = Term.t
    type 'a t = 'a Map.Make(Term).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val print : ('a -> string) -> 'a t -> unit
  end

val konev_assoc: KonevForbs.key -> 'a KonevForbs.t -> 'a option
