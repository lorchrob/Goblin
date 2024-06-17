val parse : string -> Ast.ast
val capture_output : (Format.formatter -> 'a -> unit) -> 'a -> string