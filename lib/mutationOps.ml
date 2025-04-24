open Ast

let random_element (lst: 'a list) : 'a =
    if lst = [] then Utils.crash "Empty list"
    else (
      let len = List.length lst in
      let random_index = Random.int len in
      List.nth lst random_index
    )

let rec isNonTerminalPresent nt_name prod_options = 
    match prod_options with 
    | [] -> false 
    | Rhs(ge_list, _) :: xs -> (List.mem (Nonterminal nt_name) ge_list) || (isNonTerminalPresent nt_name xs) 
    | _ :: ys -> isNonTerminalPresent nt_name ys 

let rec removeFromList nt lst =
    match lst with
    | [] -> []
    | x :: xs -> if x = nt then xs else (x :: removeFromList nt xs)

let apply_add_s1_to_rule production_options nt = 
    List.map (fun rhs_prod_rul -> match rhs_prod_rul with 
    | Rhs(geList, scList) -> 
        if List.mem (Nonterminal nt) geList
            then Rhs(geList @ [Nonterminal(nt)], scList) 
        else Rhs(geList, scList)
    | StubbedRhs(s) -> StubbedRhs(s) 
    ) production_options 

let rec find_random_production_rule (grammar : ast) : element option = 
    let candidate = random_element grammar in
    match candidate with
    | ProdRule (x, y) -> Some (ProdRule (x, y))
    | _ -> find_random_production_rule grammar

let rec grammar_element_addition (geList : grammar_element list) (nt : string) (insertion_index : int) : grammar_element list = 
    match insertion_index, geList with
    | _, [] -> [Nonterminal nt]
    | 0, xs -> (Nonterminal nt) :: xs
    | count, x :: xs -> x :: (grammar_element_addition xs nt (count - 1))
     
let rec mutation_add_s1 (g : ast) (nt : string) (pr : element option) : ast * bool = 
    match pr with 
    | Some (ProdRule (nt_name, _)) -> (
        match g with
        | [] -> [], false
        | ProdRule (nonterminal, pr_rhs) :: xs ->
            if nonterminal = nt_name then ( 
                match pr_rhs with 
                | [] -> ([], false)
                | Rhs (geList, scList) :: ys -> 
                    let list_length = List.length geList in
                    let insertion_index = Random.int list_length in
                    (ProdRule (nonterminal, Rhs ((grammar_element_addition geList nt insertion_index), scList) :: ys) :: xs), true
                (* | StubbedRhs x :: ys -> StubbedRhs x :: ys, false *)
                | StubbedRhs(x) :: ys -> (ProdRule (nonterminal, StubbedRhs(x) :: ys) :: xs), false
            )
            else
                let (gg, r) = mutation_add_s1 xs nt pr in 
                    (ProdRule (nonterminal, pr_rhs) :: gg, r)
        | TypeAnnotation(x,y,z) :: xs -> 
            let (gg, r) = mutation_add_s1 xs nt pr in 
                ((TypeAnnotation(x,y,z) :: gg), r)
            (* x :: mutation_add_s1 (xs nt pr) *)
    )
    | Some _ -> g, false
    | None -> g, false
    

    (* match g with 
    | [] -> ([], false) 
    | ProdRule (nonTerminal, production_options) :: xs -> 
        (* if nonTerminal = "COMMIT" || nonTerminal = "CONFIRM"
        then *)
            let found = isNonTerminalPresent nt production_options in 
            if found then  
                let po = apply_add_s1_to_rule production_options nt in 
                (ProdRule (nonTerminal, po) :: xs, true) 
            (* else 
                (ProdRule(nonTerminal, production_options)::xs, false)        *)
            else 
                let (gg, r) = mutation_add_s1 xs nt in 
                (ProdRule (nonTerminal, production_options) :: gg, r)   
    | TypeAnnotation(v, w, x) :: ys -> 
        let (gg, r) = mutation_add_s1 ys nt in
        (TypeAnnotation(v, w, x)::gg, r) *)

let rec isPresentInCaseList (nt:string) (caselist : case list) : bool = 
    match caselist with 
    | [] -> false 
    | Case (nte, e) :: xs -> (List.mem nt (List.map (fun (_, (b, _)) -> b) nte)) || (isPresentInExpr nt e) || (isPresentInCaseList nt xs)
    | CaseStub nte :: xs -> (List.mem nt (List.map (fun (_, (b, _)) -> b) nte)) || (isPresentInCaseList nt xs)

and isPresentInExpr (nt:string) (e:expr) : bool = 
    match e with 
    | BinOp (e1, _, e2) -> (isPresentInExpr nt e1) || (isPresentInExpr nt e2)
    | UnOp (_, e) -> (isPresentInExpr nt e)
    | CompOp (e1, _, e2) -> (isPresentInExpr nt e1) || (isPresentInExpr nt e2)
    | Length (e) -> (isPresentInExpr nt e)
    | BVCast (_, e) -> (isPresentInExpr nt e)
    | NTExpr (_, n) -> (List.mem nt (List.map fst n))
    | Match (_, (nt2, _), caselist) -> (nt = nt2) || isPresentInCaseList nt caselist
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
        if (List.length geList) > 1 then
            let deleteFromGrammarElementList = removeFromList (Nonterminal nt) geList in
            let deleteFromConstraintList = remove_constraints nt scList in
            Rhs(deleteFromGrammarElementList, deleteFromConstraintList) :: xs 
        else Rhs(geList, scList) :: xs
    | StubbedRhs(s)::xs -> StubbedRhs(s) :: (apply_delete_to_rule nt xs) 

let rec mutation_delete g nt =
    match g with
    | [] -> ([], false)
    | ProdRule(nonTerminal, production_options) :: xs ->
        (* if (nonTerminal = "SAE_PACKET")
        then *)
            let found = isNonTerminalPresent nt production_options in
            if found then
                let po = apply_delete_to_rule nt production_options in
                    (ProdRule(nonTerminal, po) :: xs, true)
            else 
                let (gg, r) = mutation_delete xs nt in
                (ProdRule(nonTerminal, production_options) :: gg, r) 
                (* (ProdRule(nonTerminal, production_options)::xs, false)        *)
            (* else 
                let (gg, r) = mutation_delete xs nt
                        in 
                (ProdRule(nonTerminal, production_options) :: gg, r)    *)
    | TypeAnnotation(v, w, x) :: ys -> 
        let (gg, r) = mutation_delete ys nt 
                in (TypeAnnotation(v, w, x) :: gg, r)

let update_constraint (nt : string) (cList : semantic_constraint list) (operation : bin_operator) : semantic_constraint list =
    match cList with 
    | [] -> []
    | x :: xs ->
        match x with
        | Dependency(nonTerminal, (BVCast(i, expr))) -> 
            if operation = Plus then
                Dependency(nonTerminal, BVCast(i, BinOp(expr, Plus, IntConst 1))) :: xs
            else if operation = Minus then
                Dependency(nonTerminal, BVCast(i, BinOp(expr, Minus, IntConst 1))) :: xs
            else Dependency(nonTerminal, BVCast(i, expr)) :: xs
        | SyGuSExpr(UnOp(LNot, e)) -> 
            if isPresentInExpr nt e 
                then SyGuSExpr(e) :: xs
            else SyGuSExpr(UnOp(LNot, e)) :: xs
        | SyGuSExpr(e) -> 
            if isPresentInExpr nt e 
                then SyGuSExpr(UnOp(LNot, e)) :: xs
            else SyGuSExpr(e) :: xs
        | anything -> anything :: xs

let rec apply_update_to_rule nt production_options operation =
    match production_options with
    | [] -> []
    | Rhs(geList, scList) :: xs -> 
        let updated_constraints = update_constraint nt scList operation in
        Rhs(geList, updated_constraints) :: xs
    | StubbedRhs(s) :: xs -> StubbedRhs(s) :: (apply_update_to_rule nt xs operation)

let rec mutation_update g nt operation =
    match g with
    | [] -> ([], false)
    | ProdRule(nonTerminal, production_options) :: xs ->
        let found = isNonTerminalPresent nt production_options in
            if found then
                let po = apply_update_to_rule nt production_options operation in
                    (ProdRule(nonTerminal, po) :: xs, true)
            else 
                let (gg, r) = mutation_update xs nt operation
                    in 
            (ProdRule(nonTerminal, production_options) :: gg, r)
    | TypeAnnotation(v, w, x) :: ys -> 
        if v = nt then
            let po = update_constraint nt x operation in
                (TypeAnnotation(v, w, po) :: ys, true)
        else
            let (gg, r) = mutation_update ys nt operation
                in 
                (TypeAnnotation(v, w, x) :: gg , r)

let rec replace_element geList nt1 nt2 =
    match geList with
    | [] -> Utils.crash "error crossover"
    | x :: xs -> if x = nt1 then nt2 :: xs
                 else x :: (replace_element xs nt1 nt2)

let rec get_production_rules_for_crossover g =
    let r1 = random_element g in
    let r2 = random_element g in
    match r1, r2 with
    | ProdRule(a, _), ProdRule(c, _) -> 
        if a = "SAE_PACKET" || c = "SAE_PACKET" then get_production_rules_for_crossover g
        else r1, r2
    | _, _ -> get_production_rules_for_crossover g

let rec replace_Rhs production_options rhs1 crossoverRhs = 
    match production_options with
    | [] -> []
    | x :: xs -> if x = rhs1 then crossoverRhs :: xs
                    else x :: (replace_Rhs xs rhs1 crossoverRhs)

let rec replace_geList b rhs1 rhs2 crossoverPRs =
    match b with
    | [] -> []
    | x :: xss -> 
        if x = rhs1 
        then (replace_Rhs b rhs1 (fst crossoverPRs)) @ (replace_geList xss rhs1 rhs2 crossoverPRs)
        else if x = rhs2
        then (replace_Rhs b rhs2 (snd crossoverPRs)) @ (replace_geList xss rhs1 rhs2 crossoverPRs)
        else x :: (replace_geList xss rhs1 rhs2 crossoverPRs)

let rec grammarUpdateAfterCrossover (nt : string) (g : ast) (rhs1 : prod_rule_rhs) (rhs2 : prod_rule_rhs) (crossoverPRs : (prod_rule_rhs * prod_rule_rhs)) : ast = 
    match g with
    | [] -> []
    | ProdRule(a, b) :: xs -> 
    if a = nt then  
        let newPR = replace_geList b rhs1 rhs2 crossoverPRs in
        ProdRule(a, newPR) :: (grammarUpdateAfterCrossover nt xs rhs1 rhs2 crossoverPRs)
    else ProdRule(a, b) :: (grammarUpdateAfterCrossover nt xs rhs1 rhs2 crossoverPRs)
    | TypeAnnotation(x,y,z) :: xs -> TypeAnnotation(x,y,z) :: (grammarUpdateAfterCrossover nt xs rhs1 rhs2 crossoverPRs)

let extract_nt_po pr1 pr2 =
match pr1, pr2 with
| ProdRule(a, b), ProdRule(c, d) -> a, c, b, d
| _, _ -> Utils.crash "bad random for crossover"

let log_grammar msg =
let oc = open_out_gen [Open_append; Open_creat] 0o666 "../../failed_grammar.grammar" in
pp_print_ast (Format.formatter_of_out_channel oc) msg;
close_out oc;
()

let mutation_crossover (rhs1 : prod_rule_rhs) (rhs2 : prod_rule_rhs) : (prod_rule_rhs * prod_rule_rhs) =
    Random.self_init () ;
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
            | (Nonterminal _, (StubbedNonterminal (_, _))) -> Utils.crash "unexpected crossover"
            | ((StubbedNonterminal (_, _)), _) -> Utils.crash "unexpected crossover"
        )
    | (Rhs (_, _), StubbedRhs _) -> Utils.crash "unexpected crossover"
    | (StubbedRhs _, _) -> Utils.crash "unexpected crossover"
