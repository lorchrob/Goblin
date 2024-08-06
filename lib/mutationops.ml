open Ast

let rec isPresentInList elem lst = 
match lst with 
| [] -> false 
| x::xs -> if x =  elem then true else isPresentInList elem xs

let rec isNonTerminalPresent nt_name prod_options = 
match prod_options with 
| [] -> false 
| Rhs(ge_list, _)::_ -> (isPresentInList (Nonterminal nt_name) ge_list) || (isNonTerminalPresent nt_name prod_options) 
| _ :: ys -> isNonTerminalPresent nt_name ys 

let rec removeFromList nt  lst =
    match lst with
    | [] -> []
    | x :: xs -> if x = nt then xs else (x :: removeFromList nt xs)

(* let rec removeFromTupleList nt lst =
    match lst with
    | [] -> []
    | (nt :: xs) -> xs
    | x :: xs -> x :: removeFromList xs *)

let apply_add_s1_to_rule production_options = 
    (
        List.map 
            (fun rhs_prod_rul -> 
                match rhs_prod_rul with 
                | Rhs(geList, scList) -> 
                    if isPresentInList (Nonterminal "REJECTED_GROUPS") geList
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


let rec isPresentInCaseList (nt:string) (caselist : case list) : bool = 
    match caselist with 
    | [] -> false 
    | (nte, e)::xs -> (isPresentInList nt nte) || (isPresentInExpr nt e) || (isPresentInCaseList nt xs)
and 
 isPresentInExpr (nt:string) (e:expr) : bool = 
    match e with 
    | BinOp(e1, _, e2) -> (isPresentInExpr nt e1) || (isPresentInExpr nt e2)
    | UnOp(_, e) -> (isPresentInExpr nt e)
    | CompOp(e1, _, e2) -> (isPresentInExpr nt e1) || (isPresentInExpr nt e2)
    | Length(e) -> (isPresentInExpr nt e)
    | BVCast(_, e) -> (isPresentInExpr nt e)
    | NTExpr(n, _) -> (isPresentInList nt n)
    | CaseExpr(nte, caselist) -> (isPresentInList nt nte) || isPresentInCaseList nt caselist
    | _ -> false 
    


let rec remove_constraints (nt : string) (clist : semantic_constraint list) : semantic_constraint list = 
    match clist with 
    | [] -> [] 
    | x::xs -> 
        match x with 
        | Dependency(nonTerminal, e) -> 
            if nonTerminal = nt || isPresentInExpr nt e 
                then (remove_constraints nt xs) 
            else Dependency(nonTerminal, e)::(remove_constraints nt xs)  
        | SyGuSExpr(e) -> 
            if isPresentInExpr nt e 
                then (remove_constraints nt xs)
            else SyGuSExpr(e)::(remove_constraints nt xs) 

let rec apply_delete_to_rule nt production_options = 
    match production_options with
    | [] -> [] 
    | Rhs(geList, scList) :: xs -> 
        let deleteFromGrammarElementList = removeFromList (Nonterminal nt) geList in
        let deleteFromConstraintList = remove_constraints nt scList in
        Rhs(deleteFromGrammarElementList, deleteFromConstraintList) :: xs 
    | StubbedRhs(s)::xs -> StubbedRhs(s) :: (apply_delete_to_rule nt xs) 

let  mutation_delete g nt =
    match g with
    | [] -> ([], false)
    | ProdRule(nonTerminal, production_options) :: xs ->
        if nonTerminal = "SAE_PACKET"
        then
            let found = isNonTerminalPresent nt production_options in
            if found then
                let po = apply_delete_to_rule nt production_options in
                    (ProdRule(nonTerminal, po) :: xs, true)
                else 
                    (ProdRule(nonTerminal, production_options)::xs, false)       
            else 
                let (gg, r) = mutation_add_s1 xs 
                        in 
                (ProdRule(nonTerminal, production_options)::gg, r)   
        | TypeAnnotation(v, w, x) :: ys -> 
            let (gg, r) = mutation_add_s1 ys 
                    in (TypeAnnotation(v, w, x)::gg, r)
    