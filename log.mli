val filename : string ref

val init:string  -> unit

val put:string -> unit

val ch : out_channel ref
(*val putf: (string, out_channel, unit) format -> string*) 

val flush : unit

val close : unit  
