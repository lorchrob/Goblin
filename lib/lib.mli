val pp_print_list : (Format.formatter -> 'a -> unit) ->
    (unit, Format.formatter, unit) format ->
    Format.formatter ->
    'a list ->
    unit

val all_equal : 'a list -> bool