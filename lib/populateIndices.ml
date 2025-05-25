module A = Ast

let disambiguate_nonterminals (rhss : A.prod_rule_rhs list) : A.prod_rule_rhs list =
  (* First pass: count total occurrences of each Nonterminal name *)
  let total_counts = Hashtbl.create 10 in

  let count_elem = function
    | A.Nonterminal (name, _) ->
        let count = Hashtbl.find_opt total_counts name |> Option.value ~default:0 in
        Hashtbl.replace total_counts name (count + 1)
    | _ -> ()
  in

  List.iter (function
    | A.Rhs (elems, _) -> List.iter count_elem elems
    | StubbedRhs _ -> ()
  ) rhss;

  (* Second pass: assign indices only to repeated Nonterminals *)
  let running_indices = Hashtbl.create 10 in

  let disambiguate_elem = function
    | A.Nonterminal (name, _) ->
        let total = Hashtbl.find total_counts name in
        if total = 1 then
          A.Nonterminal (name, None)
        else
          let idx = Hashtbl.find_opt running_indices name |> Option.value ~default:0 in
          Hashtbl.replace running_indices name (idx + 1);
          Nonterminal (name, Some idx)
    | other -> other
  in

  let disambiguate_rhs = function
    | A.StubbedRhs _ as stub -> stub
    | Rhs (elems, constraints) ->
        let new_elems = List.map disambiguate_elem elems in
        Rhs (new_elems, constraints)
  in

  List.map disambiguate_rhs rhss

let populate_indices ast = 
  List.map (fun element -> match element with 
  | A.TypeAnnotation _ -> element 
  | A.ProdRule (nt, rhss) -> 
    let rhss = disambiguate_nonterminals rhss in 
    ProdRule (nt, rhss)
  ) ast