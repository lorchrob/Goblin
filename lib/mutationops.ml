open Ast

let rec isPresentInList elem lst = 
match lst with 
| [] -> false 
| x::xs -> if x = Nonterminal elem then true else isPresentInList elem xs

let rec isNonTerminalPresent nt_name prod_options = 
match prod_options with 
| [] -> false 
| Rhs(ge_list, _)::_ -> (isPresentInList nt_name ge_list) || (isNonTerminalPresent nt_name prod_options) 
| _ :: ys -> isNonTerminalPresent nt_name ys 

let rec removeFromList nt lst =
    match lst with
    | [] -> []
    | (nt :: xs) -> xs
    | x :: xs -> x :: removeFromList xs

let rec removeFromTupleList nt lst =
    match lst with
    | [] -> []
    | (nt :: xs) -> xs
    | x :: xs -> x :: removeFromList xs

let apply_add_s1_to_rule production_options = 
    (
        List.map 
            (fun rhs_prod_rul -> 
                match rhs_prod_rul with 
                | Rhs(geList, scList) -> 
                    if isPresentInList "REJECTED_GROUPS" geList
                        then Rhs(geList @ [Nonterminal("REJECTED_GROUPS")], scList) 
                    else Rhs(geList, scList)
                | StubbedRhs(s) -> StubbedRhs(s) 
            )
            production_options 
    )
let rec mutation_add_s1 g = 
    match g with 
    | [] -> ([], false) 
    | ProdRule(nonTerminal, production_options):: xs -> 
        if nonTerminal = "SAE_PACKET" 
        then
            let found = isNonTerminalPresent "REJECTED_GROUPS" production_options in 
            if found then  
                let po = apply_add_s1_to_rule production_options in 
                    (ProdRule(nonTerminal, po)::xs, true) 
            else 
                (ProdRule(nonTerminal, production_options)::xs, false)       
        else 
            let (gg, r) = mutation_add_s1 xs 
                    in 
            (ProdRule(nonTerminal, production_options)::gg, r)   
    | TypeAnnotation(v, w, x) :: ys -> 
        let (gg, r) = mutation_add_s1 ys 
                in (TypeAnnotation(v, w, x)::gg, r)

let rec apply_delete_to_rule nt production_options = 
    match production_options with
    | Rhs(geList, scList) -> 
        let deleteFromGrammarElementList = removeFromList nt geList in
        let deleteFromConstraintList = removeFromList nt scList in
        (deleteFromGrammarElementList, deleteFromConstraintList)
    | StubbedRhs(s) -> StubbedRhs(s)

let rec mutation_delete g nt =
    match g with
    | [] -> ([], false)
    | ProdRule(nonTerminal, production_options) :: xs ->
        if nonTerminal = "SAE_PACKET"
        then
            let found = isNonTerminalPresent nt production_options in
            if found then
                let po = apply_delete_to_rule nt production_options in
                    (ProdRule(NonTerminal, po) :: xs, true)
                else 
                    (ProdRule(nonTerminal, production_options)::xs, false)       
            else 
                let (gg, r) = mutation_add_s1 xs 
                        in 
                (ProdRule(nonTerminal, production_options)::gg, r)   
        | TypeAnnotation(v, w, x) :: ys -> 
            let (gg, r) = mutation_add_s1 ys 
                    in (TypeAnnotation(v, w, x)::gg, r)
    