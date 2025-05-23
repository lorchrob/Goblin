open Sbf.Pipeline
open Sbf
open Alcotest
module SA = SygusAst

(* TODO
  1. For "infeasible" tests, create non-infeasible counterpart
  2. Add tests from main.ml

3. Try something like 
A -> B C { B.F < B.H } 
B -> F G | H I
_ :: Int


*)

(* 
TODO: support this test case 

let test_dpll_unsat_constraint () =
  let filename = "../../../test/test_cases/test_dpll_unsat_constraint" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in
  let sygus_ast, _ = Pipeline.main_pipeline filename in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg *)


(* TODO: support this test case  *)

(* let test_dpll_unsat_constraint_2 () =
  let input = "../../../test/test_cases/test_dpll_unsat_constraint_2" in
  let _, output = main_pipeline input in
  check string "test_another_ambiguous_reference_1" output "unsat\n" *)

let test_check_sygus_ast () =
  let filename = "../../../test/test_cases/test_check_sygus_ast" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in
  let sygus_ast = SygusAst.Node ("A", [SygusAst.Node ("B", [SygusAst.VarLeaf ""]); SygusAst.Node ("C", [SygusAst.VarLeaf ""])]) in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let test_check_sygus_ast_2 () =
  let filename = "../../../test/test_cases/test_check_sygus_ast" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let sygus_ast = SygusAst.Node ("A", [SygusAst.Node ("D", [SygusAst.VarLeaf ""]); SygusAst.Node ("C", [SygusAst.VarLeaf ""])]) in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> fail "Expected error"
  | Error _ -> ()

let test_check_sygus_ast_3 () =
  let filename = "../../../test/test_cases/test_check_sygus_ast_2" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let sygus_ast = SygusAst.Node ("A", [SygusAst.Node ("B", [SygusAst.IntLeaf 3]); SygusAst.Node ("C", [SygusAst.IntLeaf 2])]) in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()
  | Error msg -> fail msg

let test_check_sygus_ast_4 () =
  let filename = "../../../test/test_cases/test_check_sygus_ast_2" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let sygus_ast = SygusAst.Node ("A", [SygusAst.Node ("B", [SygusAst.IntLeaf 1]); SygusAst.Node ("C", [SygusAst.IntLeaf 2])]) in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> fail "Expected failure"
  | Error _ -> ()

let test_check_sygus_ast_5 () =
  let filename = "../../../test/test_cases/test_check_sygus_ast_2" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let sygus_ast = SygusAst.Node ("C", [SygusAst.Node ("G", [SygusAst.IntLeaf 1])]) in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> fail "Expected failure"
  | Error _ -> ()

let test_another_ambiguous_reference_1 () =
  (* TODO: Fix ugly paths. The test_cases directory is currently not included in the build directory, 
           so we have to reference it from the build directory. *)
  let input = "../../../test/test_cases/test_another_ambiguous_reference_1" in
  let _, output = main_pipeline input in
  check string "test_another_ambiguous_reference_1" output "infeasible\n"

let test_another_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference_2" in
  let _, output = main_pipeline input in
  check string "test_another_ambiguous_reference_2" output "infeasible\n"

let test_another_ambiguous_reference () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference" in
  let _, output = main_pipeline input in
  check string "test_another_ambiguous_reference" output "00-1-1\n"

let test_dot_notation_2 () =
  let input = "../../../test/test_cases/test_dot_notation_2" in
  let _, output = main_pipeline input in
  check string "test_dot_notation_2" output "0-1\n"

let test_cyclic_dependencies () =
  let input = "../../../test/test_cases/test_cyclic_dependencies" in
  let _, output = main_pipeline input in
  check string "test_cyclic_dependencies" output "0000\n"

let test_horizontal_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_horizontal_ambiguous_reference_1" in
  let _, output = main_pipeline input in
  check string "test_horizontal_ambiguous_reference_1" output "00-1-1\n"

let test_vertical_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_1" in
  let _, output = main_pipeline input in
  check string "test_vertical_ambiguous_reference_1" output "000-1000\n"

let test_vertical_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_2" in
  let _, output = main_pipeline input in
  check string "test_vertical_ambiguous_reference_2" output "00-100\n"

(* Semantic constraint example *)
let test_sc () =
  let input = "../../../test/test_cases/test_sc" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> fail "Expected failure"
  | Error _ -> ()

let test_placeholder () =
  let input = "../../../test/test_cases/test_placeholder" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> fail "Expected failure"
  | Error _ -> ()

(* Dependent term calculation example *)
let test_dt () =
  let input = "../../../test/test_cases/test_dt" in
  let _, output = main_pipeline input in
  check string "test_dt" output "00000000000011000000000000000000\n"

let test_dt2 () =
  let input = "../../../test/test_cases/test_dt2" in
  let _, output = main_pipeline input in
  check string "test_sc" output "00000000000001110000000000000000\n"

(* Divide and conquer example *)
let test_dc () = 
  let input = "../../../test/test_cases/test_dc" in
  let _, output = main_pipeline input in 
  check string "test_dc" output "00000000000000010000000000000000\n"

let test_bl () = 
  let input = "../../../test/test_cases/test_bl" in
  let _, output = main_pipeline input in 
  check bool "test_bl" (String.length output >= 8) true

let test_ty_annot_sc2 () = 
  let input = "../../../test/test_cases/test_ty_annot_sc2" in
  let _, output = main_pipeline input in 
  check string "test_ty_annot_sc2" output "00000000000001110000000000000000\n"

let test_mult_prod_rules () = 
  let input = "../../../test/test_cases/test_mult_prod_rules" in
  let _, output = main_pipeline input in 
  check string "test_mult_prod_rules" output "00000000000001110000000000000011\n"

let test_bv_len () = 
  let input = "../../../test/test_cases/test_bv_len" in
  let _, output = main_pipeline input in 
  check string "test_bv_len" output "00000101\n"

let test_dt3 () = 
  let input = "../../../test/test_cases/test_dt3" in
  let _, output = main_pipeline input in 
  check string "test_dt3" output "0101\n" 

let test_dt4 () = 
  let input = "../../../test/test_cases/test_dt4" in
  let _, output = main_pipeline input in 
  check string "test_dt4" output "330\n" 

let test_dt5 () = 
  let input = "../../../test/test_cases/test_dt5" in
  let _, output = main_pipeline input in 
  check string "test_dt4" output "010101\n" 

let test_dt6 () = 
  let input = "../../../test/test_cases/test_dt6" in
  let _, output = main_pipeline input in 
  check string "test_dt4" output "010101\n" 

let test_recombine () =
  let input = "../../../test/test_cases/test_recombine" in
  let _, output = main_pipeline input in 
  check string "test_recombine" output "00000000000000110000100000000000\n" 
  
let test_dynamic_typing () = 
  let input = "../../../test/test_cases/test_dynamic_typing" in
  let output = String.trim (main_pipeline input |> snd) in
  check bool "test_dynamic_typing" (String.length output >= 10) true

let test_dot_notation () = 
  let input = "../../../test/test_cases/test_dot_notation" in
  let output = String.trim (main_pipeline input |> snd) in
  check string "test_dot_notation" output "-40" 

let overlapping_constraints () = 
  let input = "../../../test/test_cases/overlapping_constraints" in
  let output = String.trim (main_pipeline input |> snd) in
  check string "overlapping_constraints" output "54" 

let overlapping_constraints_2 () = 
  let input = "../../../test/test_cases/overlapping_constraints_2" in
  let output = String.trim (main_pipeline input |> snd) in
  check string "overlapping_constraints_2" output "5343" 

let repeated_nt_dependency () = 
  let input = "../../../test/test_cases/repeated_nt_dependency" in
  let output = String.trim (main_pipeline input |> snd) in
  check string "repeated_nt_dependency" output "33" 

let test_strings () = 
  let input = "../../../test/test_cases/test_strings" in
  let output = String.trim (main_pipeline input |> snd) in
  check string "strings" output "A" 

let () = 
  run "My_module" [
    "test_sc", [test_case "Semantic constraint" `Quick test_sc];
    "test_placeholder", [test_case "Placeholder" `Quick test_placeholder];
    "test_dt", [test_case "Dependent term" `Quick test_dt];
    "test_dt2", [test_case "Dependent term 2" `Quick test_dt2];
    "test_dc", [test_case "Divide and conquer" `Quick test_dc];
    "test_bl", [test_case "Bit list" `Quick test_bl];
    "test_mult_prod_rules", [test_case "Test example with nonterminal with multiple prod rules, with semantic constraints" `Quick test_mult_prod_rules];
    "test_ty_annot_sc2", [test_case "Top level type annotation with semantic constraint 2" `Quick test_ty_annot_sc2];
    "test_bv_len", [test_case "Top length function on bitvector" `Quick test_bv_len];
    "test_dt3", [test_case "Dependent term 3" `Quick test_dt3];
    "test_dt4", [test_case "Dependent term 4" `Quick test_dt4];
    "test_dt5", [test_case "Dependent term 5" `Quick test_dt5];
    "test_dt6", [test_case "Dependent term 6" `Quick test_dt6];
    "test_dynamic_typing", [test_case "Dynamic typing" `Quick test_dynamic_typing];
    "test_recombine", [test_case "Recombine" `Quick test_recombine];
    "test_dot_notation", [test_case "Test dot notation" `Quick test_dot_notation];
    "test_vertical_ambiguous_reference_1", [test_case "test_vertical_ambiguous_reference_1" `Quick test_vertical_ambiguous_reference_1];
    "test_vertical_ambiguous_reference_2", [test_case "test_vertical_ambiguous_reference_2" `Quick test_vertical_ambiguous_reference_2];
    "test_horizontal_ambiguous_reference_1", [test_case "test_horizontal_ambiguous_reference_1" `Quick test_horizontal_ambiguous_reference_1];
    "test_cyclic_dependencies", [test_case "test_cyclic_dependencies" `Quick test_cyclic_dependencies];
    "test_dot_notation_2", [test_case "test_dot_notation_2" `Quick test_dot_notation_2];
    "test_another_ambiguous_reference", [test_case "test_another_ambiguous_reference" `Quick test_another_ambiguous_reference];
    "test_another_ambiguous_reference_1", [test_case "test_another_ambiguous_reference_1" `Quick test_another_ambiguous_reference_1];
    "test_another_ambiguous_reference_2", [test_case "test_another_ambiguous_reference_2" `Quick test_another_ambiguous_reference_2];
    "overlapping_constraints", [test_case "overlapping_constraints" `Quick overlapping_constraints];
    "overlapping_constraints_2", [test_case "overlapping_constraints_2" `Quick overlapping_constraints_2];
    "repeated_nt_dependency", [test_case "repeated_nt_dependency" `Quick repeated_nt_dependency];
    "test_check_sygus_ast", [test_case "test_check_sygus_ast" `Quick test_check_sygus_ast];
    "test_check_sygus_ast_2", [test_case "test_check_sygus_ast_2" `Quick test_check_sygus_ast_2];
    "test_check_sygus_ast_3", [test_case "test_check_sygus_ast_3" `Quick test_check_sygus_ast_3];
    "test_check_sygus_ast_4", [test_case "test_check_sygus_ast_4" `Quick test_check_sygus_ast_4];
    "test_check_sygus_ast_5", [test_case "test_check_sygus_ast_5" `Quick test_check_sygus_ast_5];
    (* "test_dpll_unsat_constraint", [test_case "test_dpll_unsat_constraint" `Quick test_dpll_unsat_constraint]; *)
    "test_strings", [test_case "test_strings" `Quick test_strings];
  ]