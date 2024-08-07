open Ast

let random_element (lst: 'a list) : 'a =
    if lst = [] then failwith "Empty list"
    else begin
      let len = List.length lst in
      let random_index = Random.int len in
      List.nth lst random_index
    end

let rec isPresentInList elem lst = 
match lst with 
| [] -> false 
| x::xs -> if x =  elem then true else isPresentInList elem xs

let rec isNonTerminalPresent nt_name prod_options = 
match prod_options with 
| [] -> false 
| Rhs(ge_list, _)::_ -> (isPresentInList (Nonterminal nt_name) ge_list) || (isNonTerminalPresent nt_name prod_options) 
| _ :: ys -> isNonTerminalPresent nt_name ys 

let rec removeFromList nt lst =
    match lst with
    | [] -> []
    | x :: xs -> if x = nt then xs else (x :: removeFromList nt xs)

(* let rec removeFromTupleList nt lst =
    match lst with
    | [] -> []
    | (nt :: xs) -> xs
    | x :: xs -> x :: removeFromList xs *)

let apply_add_s1_to_rule production_options nt = 
    (
        List.map 
            (fun rhs_prod_rul -> 
                match rhs_prod_rul with 
                | Rhs(geList, scList) -> 
                    if isPresentInList (Nonterminal nt) geList
                        then Rhs(geList @ [Nonterminal(nt)], scList) 
                    else Rhs(geList, scList)
                | StubbedRhs(s) -> StubbedRhs(s) 
            )
            production_options 
    )
let rec mutation_add_s1 g nt = 
    match g with 
    | [] -> ([], false) 
    | ProdRule(nonTerminal, production_options):: xs -> 
        if nonTerminal = "SAE_PACKET" 
        then
            let found = isNonTerminalPresent nt production_options in 
            if found then  
                let po = apply_add_s1_to_rule production_options nt in 
                    (ProdRule(nonTerminal, po)::xs, true) 
            else 
                (ProdRule(nonTerminal, production_options)::xs, false)       
        else 
            let (gg, r) = mutation_add_s1 xs nt
                    in 
            (ProdRule(nonTerminal, production_options)::gg, r)   
    | TypeAnnotation(v, w, x) :: ys -> 
        let (gg, r) = mutation_add_s1 ys nt
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

let rec mutation_delete g nt =
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
                let (gg, r) = mutation_delete xs nt
                        in 
                (ProdRule(nonTerminal, production_options)::gg, r)   
        | TypeAnnotation(v, w, x) :: ys -> 
            let (gg, r) = mutation_delete ys nt 
                    in (TypeAnnotation(v, w, x)::gg, r)

let update_constraint (nt : string) (cList : semantic_constraint list) : semantic_constraint list =
    match cList with 
    | [] -> []
    | x :: xs ->
        match x with
        | Dependency(nonTerminal, (UnOp(LNot, e))) -> 
            if nonTerminal = nt
                then Dependency(nonTerminal, e) :: xs
            else
                Dependency(nonTerminal, (UnOp(LNot, e))) :: xs
        | Dependency(nonTerminal, e) -> 
            if nonTerminal = nt 
                then Dependency(nonTerminal, (UnOp(LNot, e))) :: xs
            else Dependency(nonTerminal, e) :: xs
        | SyGuSExpr(UnOp(LNot, e)) -> 
            if isPresentInExpr nt e 
                then SyGuSExpr(e) :: xs
            else SyGuSExpr(UnOp(LNot, e)) :: xs
        | SyGuSExpr(e) -> 
            if isPresentInExpr nt e 
                then SyGuSExpr(UnOp(LNot, e)) :: xs
            else SyGuSExpr(e) :: xs


let rec apply_update_to_rule nt production_options =
    match production_options with
    | [] -> []
    | Rhs(geList, scList) :: xs -> 
        let updated_constraints = update_constraint nt scList in
        Rhs(geList, updated_constraints) :: xs
    | StubbedRhs(s) :: xs -> StubbedRhs(s) :: (apply_update_to_rule nt xs)


let rec mutation_update g nt =
    match g with
    | [] -> ([], false)
    | ProdRule(nonTerminal, production_options) :: xs ->
        if nonTerminal = "SAE_PACKET"
        then
            let found = isNonTerminalPresent nt production_options in
            if found then
                let po = apply_update_to_rule nt production_options in
                    (ProdRule(nonTerminal, po) :: xs, true)
                else 
                    (ProdRule(nonTerminal, production_options)::xs, false)       
            else 
                let (gg, r) = mutation_update xs nt 
                        in 
                (ProdRule(nonTerminal, production_options)::gg, r)   
        | TypeAnnotation(v, w, x) :: ys -> 
            let (gg, r) = mutation_update ys nt
                    in (TypeAnnotation(v, w, x)::gg, r)

let rec replace_element geList nt1 nt2 =
    match geList with
    | [] -> failwith "error crossover"
    | x :: xs -> if x = nt1 then nt2 :: xs
                 else x :: (replace_element xs nt1 nt2)


let mutation_crossover (rhs1 : prod_rule_rhs) (rhs2 : prod_rule_rhs) : (prod_rule_rhs * prod_rule_rhs) =
    match rhs1, rhs2 with
    | Rhs([],[]), Rhs([],[]) -> Rhs([],[]), Rhs([],[])
    | Rhs(geList1, scList1), Rhs (geList2, scList2) -> 
        let randomGe1 = random_element geList1 in
        let randomGe2 = random_element geList2 in
        let crossoverList1 = replace_element geList1 randomGe1 randomGe2 in
        let crossoverList2 = replace_element geList2 randomGe2 randomGe1 in (
            match randomGe1, randomGe2 with
            | (Nonterminal a), (Nonterminal b) -> 
                (Rhs(crossoverList1, (remove_constraints a scList1)), Rhs(crossoverList2, (remove_constraints b scList2)))
            | (Nonterminal _, (NamedNonterminal (_, _)|StubbedNonterminal (_, _))) -> failwith "unexpected crossover"
            | ((NamedNonterminal (_, _)|StubbedNonterminal (_, _)), _) -> failwith "unexpected crossover"
        )
    | (Rhs (_, _), StubbedRhs _) -> failwith "unexpected crossover"
    | (StubbedRhs _, _) -> failwith "unexpected crossover"
