exception AllReturnedNone

val race_n : ((unit -> 'a) * string) list -> 'a
val race_n_opt: ((unit -> 'a option) * string) list -> 'a
val parallel_map : ('a -> 'b) -> 'a list -> 'b list
val race_commands : string -> string -> bool