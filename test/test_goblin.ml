open Goblin.Pipeline
open Goblin 
open Alcotest
module SA = SolverAst

(* 

TODO: XML, CSV, WIFI test cases 

TODO: support this test case 

let test_dpll_unsat_constraint () =
  let filename = "../../../test/test_cases/test_dpll_unsat_constraint" in
  let input = Utils.read_file filename in _, ast = Pipeline.main_pipeline filename in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg *)


(* TODO: support this test case  *)

(* let test_dpll_unsat_constraint_2 () =
  let input = "../../../test/test_cases/test_dpll_unsat_constraint_2" in
  let _, output, _ = main_pipeline input in
  check string "test_another_ambiguous_reference_1" output "unsat\n" *)

let bug1 () =
  let input = "../../../test/test_cases/bug1.gbl" in
  try 
    let _ = main_pipeline input in
    fail "expected error"
  with _ -> () 

let bug4 () =
  let input = "../../../test/test_cases/bug4.gbl" in
  try 
    let _ = main_pipeline input in
    fail "expected error"
  with _ -> () 

let bug3 () =
  let input = "../../../test/test_cases/bug3.gbl" in
  try 
    let _ = main_pipeline input in
    fail "expected error"
  with _ -> () 

let bug2 () =
  let input = "../../../test/test_cases/bug2.gbl" in
  try 
    let _ = main_pipeline input in
    fail "expected error"
  with _ -> () 

let test_check_solver_ast () =
  let filename = "../../../test/test_cases/test_check_solver_ast" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in
  let solver_ast = SolverAst.Node (("A", None), [SolverAst.Node (("B", None), [SolverAst.VarLeaf ""]); SolverAst.Node (("C", None), [SolverAst.VarLeaf ""])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let test_check_solver_ast_2 () =
  let filename = "../../../test/test_cases/test_check_solver_ast" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let solver_ast = SolverAst.Node (("A", None), [SolverAst.Node (("D", None), [SolverAst.VarLeaf ""]); SolverAst.Node (("C", None), [SolverAst.VarLeaf ""])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> fail "Expected error"
  | Error _ -> ()

let test_check_solver_ast_3 () =
  let filename = "../../../test/test_cases/test_check_solver_ast_2" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let solver_ast = SolverAst.Node (("A", None), [SolverAst.Node (("B", None), [SolverAst.IntLeaf 3]); SolverAst.Node (("C", None), [SolverAst.IntLeaf 2])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()
  | Error msg -> fail msg

let test_check_solver_ast_4 () =
  let filename = "../../../test/test_cases/test_check_solver_ast_2" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let solver_ast = SolverAst.Node (("A", None), [SolverAst.Node (("B", None), [SolverAst.Node (("G", None), [SolverAst.IntLeaf 1])]); SolverAst.Node (("C", None), [SolverAst.Node (("G", None), [SolverAst.IntLeaf (2)])])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> fail "Expected failure"
  | Error _ -> ()

let test_check_solver_ast_5 () =
  let filename = "../../../test/test_cases/test_check_solver_ast_2" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let solver_ast = SolverAst.Node (("C", None), [SolverAst.Node (("G", None), [SolverAst.IntLeaf 1])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> fail "Expected failure"
  | Error _ -> ()

let test_check_solver_ast_6 () =
  let filename = "../../../test/test_cases/test_check_solver_ast_3" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let solver_ast = SolverAst.Node (("A", None), [SolverAst.Node (("B", None), [SolverAst.IntLeaf (-1)]); SolverAst.Node (("C", None), [SolverAst.IntLeaf (-2)])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()
  | Error msg -> fail msg

let test_check_solver_ast_7 () =
  let filename = "../../../test/test_cases/test_check_solver_ast_3" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let solver_ast = SolverAst.Node (("A", None), [SolverAst.Node (("B", None), [SolverAst.Node (("G", None), [SolverAst.IntLeaf (2)])]); SolverAst.Node (("C", None), [SolverAst.Node (("G", None), [SolverAst.IntLeaf (1)])])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> fail "Expected failure"
  | Error _ -> ()

(*let sd_test_another_ambiguous_reference_1 () =
  (* TODO: Fix ugly paths. The test_cases directory is currently not included in the build directory, 
           so we have to reference it from the build directory. *)
  let input = "../../../test/test_cases/test_another_ambiguous_reference_1" in
  let _, output, _ = main_pipeline ~engine:(Some SygusDac) input in
  check string "test_another_ambiguous_reference_1" output "infeasible\n"

let sd_test_another_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference_2" in
  let _, output, _ = main_pipeline ~engine:(Some SygusDac) input in
  check string "test_another_ambiguous_reference_2" output "infeasible\n"*)

(*let sd_test_another_ambiguous_reference () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dot_notation_2 () =
  let input = "../../../test/test_cases/test_dot_notation_2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_cyclic_dependencies () =
  let input = "../../../test/test_cases/test_cyclic_dependencies" in
  try 
    let _ = main_pipeline ~engine:(Some SygusDac) input in
    fail "expected error"
  with _ -> () 

let sd_test_horizontal_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_horizontal_ambiguous_reference_1" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_vertical_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_1" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_vertical_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Semantic constraint example *)
let sd_test_sc () =
  let input = "../../../test/test_cases/test_sc" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_placeholder () =
  let input = "../../../test/test_cases/test_placeholder" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Dependent term calculation example *)
let sd_test_dt () =
  let input = "../../../test/test_cases/test_dt" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dt2 () =
  let input = "../../../test/test_cases/test_dt2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Divide and conquer example *)
let sd_test_dc () = 
  let input = "../../../test/test_cases/test_dc" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_bl () = 
  let input = "../../../test/test_cases/test_bl" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_ty_annot_sc2 () = 
  let input = "../../../test/test_cases/test_ty_annot_sc2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_mult_prod_rules () = 
  let input = "../../../test/test_cases/test_mult_prod_rules" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_bv_len () = 
  let input = "../../../test/test_cases/test_bv_len" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dt3 () = 
  let input = "../../../test/test_cases/test_dt3" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dt4 () = 
  let input = "../../../test/test_cases/test_dt4" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dt5 () = 
  let input = "../../../test/test_cases/test_dt5" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dt6 () = 
  let input = "../../../test/test_cases/test_dt6" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* let sd_test_recombine () =
  let input = "../../../test/test_cases/test_recombine" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg *)
  
let sd_test_dynamic_typing () = 
  let input = "../../../test/test_cases/test_dynamic_typing" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dot_notation () = 
  let input = "../../../test/test_cases/test_dot_notation" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_overlapping_constraints () = 
  let input = "../../../test/test_cases/overlapping_constraints" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_overlapping_constraints_2 () = 
  let input = "../../../test/test_cases/overlapping_constraints_2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_repeated_nt_dependency () = 
  let input = "../../../test/test_cases/repeated_nt_dependency" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_strings () = 
  let input = "../../../test/test_cases/test_strings" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg*)

let dm_test_another_ambiguous_reference_1 () =
  (* TODO: Fix ugly paths. The test_cases directory is currently not included in the build directory, 
           so we have to reference it from the build directory. *)
  let input = "../../../test/test_cases/test_another_ambiguous_reference_1" in
  let _, output, _ = main_pipeline ~engine:(Some DpllMono) input in
  check string "test_another_ambiguous_reference_1" output "infeasible\n"

let dm_test_another_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference_2" in
  let _, output, _ = main_pipeline ~engine:(Some DpllMono) input in
  check string "test_another_ambiguous_reference_2" output "infeasible\n"

let bug5 () =
  let input = "../../../test/test_cases/bug5.gbl" in
  let solver_ast, _, ast = main_pipeline input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let bug6 () =
  let input = "../../../test/test_cases/bug6.gbl" in
  try 
    let _ = main_pipeline input in
    fail "should fail"
  with _ -> ()

let dm_test_another_ambiguous_reference () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dot_notation_2 () =
  let input = "../../../test/test_cases/test_dot_notation_2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_cyclic_dependencies () =
  let input = "../../../test/test_cases/test_cyclic_dependencies" in
  try 
    let _ = main_pipeline ~engine:(Some DpllMono) input in
    fail "expected error"
  with _ -> () 

let dm_test_horizontal_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_horizontal_ambiguous_reference_1" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_vertical_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_1" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_vertical_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Semantic constraint example *)
let dm_test_sc () =
  let input = "../../../test/test_cases/test_sc" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_placeholder () =
  let input = "../../../test/test_cases/test_placeholder" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let probabilities () =
  let input = "../../../test/test_cases/probabilities.gbl" in
  let solver_ast, _, ast = main_pipeline input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let probabilities_2 () =
  let input = "../../../test/test_cases/probabilities_2.gbl" in
  let solver_ast, _, ast = main_pipeline input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Dependent term calculation example *)
let dm_test_dt () =
  let input = "../../../test/test_cases/test_dt" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dt2 () =
  let input = "../../../test/test_cases/test_dt2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Divide and conquer example *)
let dm_test_dc () = 
  let input = "../../../test/test_cases/test_dc" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_bl () = 
  let input = "../../../test/test_cases/test_bl" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_ty_annot_sc2 () = 
  let input = "../../../test/test_cases/test_ty_annot_sc2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_mult_prod_rules () = 
  let input = "../../../test/test_cases/test_mult_prod_rules" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_bv_len () = 
  let input = "../../../test/test_cases/test_bv_len" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dt3 () = 
  let input = "../../../test/test_cases/test_dt3" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dt4 () = 
  let input = "../../../test/test_cases/test_dt4" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dt5 () = 
  let input = "../../../test/test_cases/test_dt5" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dt6 () = 
  let input = "../../../test/test_cases/test_dt6" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_recombine () =
  let input = "../../../test/test_cases/test_recombine" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg
  
let dm_test_dynamic_typing () = 
  let input = "../../../test/test_cases/test_dynamic_typing" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dot_notation () = 
  let input = "../../../test/test_cases/test_dot_notation" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_overlapping_constraints () = 
  let input = "../../../test/test_cases/overlapping_constraints" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_overlapping_constraints_2 () = 
  let input = "../../../test/test_cases/overlapping_constraints_2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_repeated_nt_dependency () = 
  let input = "../../../test/test_cases/repeated_nt_dependency" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_strings () = 
  let input = "../../../test/test_cases/test_strings" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(*let dd_test_another_ambiguous_reference_1 () =
  (* TODO: Fix ugly paths. The test_cases directory is currently not included in the build directory, 
           so we have to reference it from the build directory. *)
  let input = "../../../test/test_cases/test_another_ambiguous_reference_1" in
  let _, output, _ = main_pipeline ~engine:(Some DpllDac) input in
  check string "test_another_ambiguous_reference_1" output "infeasible\n"

let dd_test_another_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference_2" in
  let _, output, _ = main_pipeline ~engine:(Some DpllDac) input in
  check string "test_another_ambiguous_reference_2" output "infeasible\n"

let dd_test_another_ambiguous_reference () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dot_notation_2 () =
  let input = "../../../test/test_cases/test_dot_notation_2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_cyclic_dependencies () =
  let input = "../../../test/test_cases/test_cyclic_dependencies" in
  try 
    let _ = main_pipeline ~engine:(Some DpllDac) input in
    fail "expected error"
  with _ -> () 

let dd_test_horizontal_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_horizontal_ambiguous_reference_1" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_vertical_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_1" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_vertical_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Semantic constraint example *)
let dd_test_sc () =
  let input = "../../../test/test_cases/test_sc" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_placeholder () =
  let input = "../../../test/test_cases/test_placeholder" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Dependent term calculation example *)
let dd_test_dt () =
  let input = "../../../test/test_cases/test_dt" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dt2 () =
  let input = "../../../test/test_cases/test_dt2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Divide and conquer example *)
let dd_test_dc () = 
  let input = "../../../test/test_cases/test_dc" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_bl () = 
  let input = "../../../test/test_cases/test_bl" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_ty_annot_sc2 () = 
  let input = "../../../test/test_cases/test_ty_annot_sc2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_mult_prod_rules () = 
  let input = "../../../test/test_cases/test_mult_prod_rules" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_bv_len () = 
  let input = "../../../test/test_cases/test_bv_len" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dt3 () = 
  let input = "../../../test/test_cases/test_dt3" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dt4 () = 
  let input = "../../../test/test_cases/test_dt4" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dt5 () = 
  let input = "../../../test/test_cases/test_dt5" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dt6 () = 
  let input = "../../../test/test_cases/test_dt6" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_recombine () =
  let input = "../../../test/test_cases/test_recombine" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg
  
let dd_test_dynamic_typing () = 
  let input = "../../../test/test_cases/test_dynamic_typing" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dot_notation () = 
  let input = "../../../test/test_cases/test_dot_notation" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_repeated_nt_dependency () = 
  let input = "../../../test/test_cases/repeated_nt_dependency" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_strings () = 
  let input = "../../../test/test_cases/test_strings" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_another_ambiguous_reference_1 () =
  (* TODO: Fix ugly paths. The test_cases directory is currently not included in the build directory, 
           so we have to reference it from the build directory. *)
  let input = "../../../test/test_cases/test_another_ambiguous_reference_1" in
  let _, output, _ = main_pipeline ~engine:(Some MixedDac) input in
  check string "test_another_ambiguous_reference_1" output "infeasible\n"

let md_test_another_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference_2" in
  let _, output, _ = main_pipeline ~engine:(Some MixedDac) input in
  check string "test_another_ambiguous_reference_2" output "infeasible\n"

let md_test_another_ambiguous_reference () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_dot_notation_2 () =
  let input = "../../../test/test_cases/test_dot_notation_2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_cyclic_dependencies () =
  let input = "../../../test/test_cases/test_cyclic_dependencies" in
  try 
    let _ = main_pipeline ~engine:(Some MixedDac) input in
    fail "expected error"
  with _ -> () 

let md_test_horizontal_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_horizontal_ambiguous_reference_1" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_vertical_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_1" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_vertical_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Semantic constraint example *)
let md_test_sc () =
  let input = "../../../test/test_cases/test_sc" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(*let md_test_placeholder () =
  let input = "../../../test/test_cases/test_placeholder" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg*)

(* Dependent term calculation example *)
let md_test_dt () =
  let input = "../../../test/test_cases/test_dt" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(*let md_test_dt2 () =
  let input = "../../../test/test_cases/test_dt2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg*)

(* Divide and conquer example *)
let md_test_dc () = 
  let input = "../../../test/test_cases/test_dc" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_bl () = 
  let input = "../../../test/test_cases/test_bl" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_ty_annot_sc2 () = 
  let input = "../../../test/test_cases/test_ty_annot_sc2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_mult_prod_rules () = 
  let input = "../../../test/test_cases/test_mult_prod_rules" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_bv_len () = 
  let input = "../../../test/test_cases/test_bv_len" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_dt3 () = 
  let input = "../../../test/test_cases/test_dt3" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_dt4 () = 
  let input = "../../../test/test_cases/test_dt4" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_dt5 () = 
  let input = "../../../test/test_cases/test_dt5" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(*let md_test_dt6 () = 
  let input = "../../../test/test_cases/test_dt6" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg*)
  
let md_test_dynamic_typing () = 
  let input = "../../../test/test_cases/test_dynamic_typing" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_dot_notation () = 
  let input = "../../../test/test_cases/test_dot_notation" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_overlapping_constraints () = 
  let input = "../../../test/test_cases/overlapping_constraints" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_overlapping_constraints_2 () = 
  let input = "../../../test/test_cases/overlapping_constraints_2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_repeated_nt_dependency () = 
  let input = "../../../test/test_cases/repeated_nt_dependency" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_strings () = 
  let input = "../../../test/test_cases/test_strings" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test2 () = 
  let input = "../../../test/test_cases/test2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test3 () = 
  let input = "../../../test/test_cases/test3" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test4 () = 
  let input = "../../../test/test_cases/test4" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test5 () = 
  let input = "../../../test/test_cases/test5" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test6 () = 
  let input = "../../../test/test_cases/test6" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test7 () = 
  let input = "../../../test/test_cases/test7" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test8 () = 
  let input = "../../../test/test_cases/test8" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test9 () = 
  let input = "../../../test/test_cases/test9" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test11 () = 
  let input = "../../../test/test_cases/test11" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test14 () = 
  let input = "../../../test/test_cases/test14" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test17 () = 
  let input = "../../../test/test_cases/test17" in
  try 
    let _ = main_pipeline ~engine:(Some SygusDac) input in
    fail "expected error"
  with _ -> () *)

let dm_test17 () = 
  let input = "../../../test/test_cases/test17" in
  try 
    let _ = main_pipeline ~engine:(Some DpllMono) input in
    fail "expected error"
  with _ -> () 

(*let dd_test17 () = 
  let input = "../../../test/test_cases/test17" in
  try 
    let _ = main_pipeline ~engine:(Some DpllDac) input in
    fail "expected error"
  with _ -> () 

let md_test17 () = 
  let input = "../../../test/test_cases/test17" in
  try 
    let _ = main_pipeline ~engine:(Some MixedDac) input in
    fail "expected error"
  with _ -> () 

let sd_test18 () = 
  let input = "../../../test/test_cases/test18" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg*)

let dm_test18 () = 
  let input = "../../../test/test_cases/test18" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(*let dd_test18 () = 
  let input = "../../../test/test_cases/test18" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test18 () = 
  let input = "../../../test/test_cases/test18" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg*)

let dm_test2 () = 
  let input = "../../../test/test_cases/test2" in
  try 
    let _ = main_pipeline ~engine:(Some DpllMono) input in
    fail "should fail"
  with _ -> ()

let reset_bug4 () = 
  let input = "../../../test/test_cases/ngap-ngsetup-bug4.gbl" in
  try 
    let _ = main_pipeline input in
    fail "should fail"
  with _ -> ()

let dm_test3 () = 
  let input = "../../../test/test_cases/test3" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test4 () = 
  let input = "../../../test/test_cases/test4" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test5 () = 
  let input = "../../../test/test_cases/test5" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test6 () = 
  let input = "../../../test/test_cases/test6" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test7 () = 
  let input = "../../../test/test_cases/test7" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test8 () = 
  let input = "../../../test/test_cases/test8" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg
         
let dm_test9 () = 
  let input = "../../../test/test_cases/test9" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test13 () = 
  let input = "../../../test/test_cases/test13" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test11 () = 
  let input = "../../../test/test_cases/test11" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test12 () = 
  let input = "../../../test/test_cases/test12" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test14 () = 
  let input = "../../../test/test_cases/test14" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(*let dd_test3 () = 
  let input = "../../../test/test_cases/test3" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test4 () = 
  let input = "../../../test/test_cases/test4" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test5 () = 
  let input = "../../../test/test_cases/test5" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test6 () = 
  let input = "../../../test/test_cases/test6" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test7 () = 
  let input = "../../../test/test_cases/test7" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test8 () = 
  let input = "../../../test/test_cases/test8" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test9 () = 
  let input = "../../../test/test_cases/test9" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test11 () = 
  let input = "../../../test/test_cases/test11" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test12 () = 
  let input = "../../../test/test_cases/test12" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test14 () = 
  let input = "../../../test/test_cases/test14" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test2 () = 
  let input = "../../../test/test_cases/test2" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(*let md_test3 () = 
  let input = "../../../test/test_cases/test3" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg*)

let md_test4 () = 
  let input = "../../../test/test_cases/test4" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test5 () = 
  let input = "../../../test/test_cases/test5" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test6 () = 
  let input = "../../../test/test_cases/test6" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test7 () = 
  let input = "../../../test/test_cases/test7" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test8 () = 
  let input = "../../../test/test_cases/test8" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test9 () = 
  let input = "../../../test/test_cases/test9" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test11 () = 
  let input = "../../../test/test_cases/test11" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test14 () = 
  let input = "../../../test/test_cases/test14" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test16 () = 
  let input = "../../../test/test_cases/test16" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg*)

let example_fail () = 
  let input = "../../../test/test_cases/example-fail.gbl" in
  try 
    let _ = main_pipeline input in
    fail "should fail"
  with _ -> ()

let dm_test16 () = 
  let input = "../../../test/test_cases/test16" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let ngap_ngsetup_bug () = 
  let input = "../../../test/test_cases/ngap-ngsetup-bug.gbl" in
  let solver_ast, _, ast = main_pipeline input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

  (* TODO: Support passing --starting-depth-limit functionally *)
(*let msg2 () = 
  let input = "../../../test/test_cases/msg2.gbl" in
  let solver_ast, _, ast = main_pipeline input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg*)

(*let md_test16 () = 
  let input = "../../../test/test_cases/test16" in
  let solver_ast, _, ast = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg*)

let test10 () =
  let input = "../../../test/test_cases/test10" in
  match main_pipeline ~engine:(Some SygusDac) input with
  | _ -> Alcotest.fail "Expected exception, but got success"
  | exception _ -> ()  (* test passes *)

let () = 
  run "My_module" [
    "test_check_solver_ast", [test_case "test_check_solver_ast" `Quick test_check_solver_ast];
    "test_check_solver_ast_2", [test_case "test_check_solver_ast_2" `Quick test_check_solver_ast_2];
    "test_check_solver_ast_3", [test_case "test_check_solver_ast_3" `Quick test_check_solver_ast_3];
    "test_check_solver_ast_4", [test_case "test_check_solver_ast_4" `Quick test_check_solver_ast_4];
    "test_check_solver_ast_5", [test_case "test_check_solver_ast_5" `Quick test_check_solver_ast_5];

    (*"sd_test_sc", [test_case "Semantic constraint" `Quick sd_test_sc];
    "sd_test_placeholder", [test_case "Placeholder" `Quick sd_test_placeholder];
    "sd_test_dt", [test_case "Dependent term" `Quick sd_test_dt];
    "sd_test_dt2", [test_case "Dependent term 2" `Quick sd_test_dt2];
    "sd_test_dc", [test_case "Divide and conquer" `Quick sd_test_dc];
    "sd_test_bl", [test_case "Bit list" `Quick sd_test_bl];
    "sd_test_mult_prod_rules", [test_case "Test example with nonterminal with multiple prod rules, with semantic constraints" `Quick sd_test_mult_prod_rules];
    "sd_test_ty_annot_sc2", [test_case "Top level type annotation with semantic constraint 2" `Quick sd_test_ty_annot_sc2];
    "sd_test_bv_len", [test_case "Top length function on bitvector" `Quick sd_test_bv_len];
    "sd_test_dt3", [test_case "Dependent term 3" `Quick sd_test_dt3];
    "sd_test_dt4", [test_case "Dependent term 4" `Quick sd_test_dt4];
    "sd_test_dt5", [test_case "Dependent term 5" `Quick sd_test_dt5];
    "sd_test_dt6", [test_case "Dependent term 6" `Quick sd_test_dt6];
    "sd_test_dynamic_typing", [test_case "Dynamic typing" `Quick sd_test_dynamic_typing];
    "sd_test_dot_notation", [test_case "Test dot notation" `Quick sd_test_dot_notation];
    "sd_test_vertical_ambiguous_reference_1", [test_case "test_vertical_ambiguous_reference_1" `Quick sd_test_vertical_ambiguous_reference_1];
    "sd_test_vertical_ambiguous_reference_2", [test_case "test_vertical_ambiguous_reference_2" `Quick sd_test_vertical_ambiguous_reference_2];
    "sd_test_horizontal_ambiguous_reference_1", [test_case "test_horizontal_ambiguous_reference_1" `Quick sd_test_horizontal_ambiguous_reference_1];
    "sd_test_cyclic_dependencies", [test_case "test_cyclic_dependencies" `Quick sd_test_cyclic_dependencies];
    "sd_test_dot_notation_2", [test_case "test_dot_notation_2" `Quick sd_test_dot_notation_2];
    "sd_test_another_ambiguous_reference", [test_case "test_another_ambiguous_reference" `Quick sd_test_another_ambiguous_reference];
    "sd_overlapping_constraints", [test_case "overlapping_constraints" `Quick sd_overlapping_constraints];
    "sd_overlapping_constraints_2", [test_case "overlapping_constraints_2" `Quick sd_overlapping_constraints_2];
    "sd_repeated_nt_dependency", [test_case "repeated_nt_dependency" `Quick sd_repeated_nt_dependency];
    "sd_test_strings", [test_case "test_strings" `Quick sd_test_strings];*)
 
    "dm_test_sc", [test_case "Semantic constraint" `Quick dm_test_sc];
    "dm_test_placeholder", [test_case "Placeholder" `Quick dm_test_placeholder];
    "dm_test_dt", [test_case "Dependent term" `Quick dm_test_dt];
    "dm_test_dt2", [test_case "Dependent term 2" `Quick dm_test_dt2];
    "dm_test_dc", [test_case "Divide and conquer" `Quick dm_test_dc];
    "dm_test_bl", [test_case "Bit list" `Quick dm_test_bl];
     "dm_test_mult_prod_rules", [test_case "Test example with nonterminal with multiple prod rules, with semantic constraints" `Quick dm_test_mult_prod_rules];
    "dm_test_ty_annot_sc2", [test_case "Top level type annotation with semantic constraint 2" `Quick dm_test_ty_annot_sc2];
    "dm_test_bv_len", [test_case "Top length function on bitvector" `Quick dm_test_bv_len];
    "dm_test_dt3", [test_case "Dependent term 3" `Quick dm_test_dt3];
    "dm_test_dt4", [test_case "Dependent term 4" `Quick dm_test_dt4];
    "dm_test_dt5", [test_case "Dependent term 5" `Quick dm_test_dt5];
    "dm_test_dt6", [test_case "Dependent term 6" `Quick dm_test_dt6];
    "dm_test_dynamic_typing", [test_case "Dynamic typing" `Quick dm_test_dynamic_typing];
    "dm_test_recombine", [test_case "Recombine" `Quick dm_test_recombine];
    "dm_test_dot_notation", [test_case "Test dot notation" `Quick dm_test_dot_notation];
     "dm_test_vertical_ambiguous_reference_1", [test_case "test_vertical_ambiguous_reference_1" `Quick dm_test_vertical_ambiguous_reference_1];
    "dm_test_vertical_ambiguous_reference_2", [test_case "test_vertical_ambiguous_reference_2" `Quick dm_test_vertical_ambiguous_reference_2];
    "dm_test_horizontal_ambiguous_reference_1", [test_case "test_horizontal_ambiguous_reference_1" `Quick dm_test_horizontal_ambiguous_reference_1];
    "dm_test_cyclic_dependencies", [test_case "test_cyclic_dependencies" `Quick dm_test_cyclic_dependencies];
    "dm_test_dot_notation_2", [test_case "test_dot_notation_2" `Quick dm_test_dot_notation_2];
    "dm_test_another_ambiguous_reference", [test_case "test_another_ambiguous_reference" `Quick dm_test_another_ambiguous_reference];
    "dm_overlapping_constraints", [test_case "overlapping_constraints" `Quick dm_overlapping_constraints];
    "dm_overlapping_constraints_2", [test_case "overlapping_constraints_2" `Quick dm_overlapping_constraints_2];
    "dm_repeated_nt_dependency", [test_case "repeated_nt_dependency" `Quick dm_repeated_nt_dependency];
    "dm_test_strings", [test_case "test_strings" `Quick dm_test_strings]; 
  
    (*"dd_test_sc", [test_case "Semantic constraint" `Quick dd_test_sc];
    "dd_test_placeholder", [test_case "Placeholder" `Quick dd_test_placeholder];
    "dd_test_dt", [test_case "Dependent term" `Quick dd_test_dt];
    "dd_test_dt2", [test_case "Dependent term 2" `Quick dd_test_dt2];
    "dd_test_dc", [test_case "Divide and conquer" `Quick dd_test_dc];
    "dd_test_bl", [test_case "Bit list" `Quick dd_test_bl];
    "dd_test_mult_prod_rules", [test_case "Test example with nonterminal with multiple prod rules, with semantic constraints" `Quick dd_test_mult_prod_rules];
    "dd_test_ty_annot_sc2", [test_case "Top level type annotation with semantic constraint 2" `Quick dd_test_ty_annot_sc2];
    "dd_test_bv_len", [test_case "Top length function on bitvector" `Quick dd_test_bv_len];
    "dd_test_dt3", [test_case "Dependent term 3" `Quick dd_test_dt3];
    "dd_test_dt4", [test_case "Dependent term 4" `Quick dd_test_dt4];
    "dd_test_dt5", [test_case "Dependent term 5" `Quick dd_test_dt5];
    "dd_test_dt6", [test_case "Dependent term 6" `Quick dd_test_dt6];
    "dd_test_dynamic_typing", [test_case "Dynamic typing" `Quick dd_test_dynamic_typing];
    "dd_test_recombine", [test_case "Recombine" `Quick dd_test_recombine];
    "dd_test_dot_notation", [test_case "Test dot notation" `Quick dd_test_dot_notation];
    "dd_test_vertical_ambiguous_reference_1", [test_case "test_vertical_ambiguous_reference_1" `Quick dd_test_vertical_ambiguous_reference_1];
    "dd_test_vertical_ambiguous_reference_2", [test_case "test_vertical_ambiguous_reference_2" `Quick dd_test_vertical_ambiguous_reference_2];
    "dd_test_horizontal_ambiguous_reference_1", [test_case "test_horizontal_ambiguous_reference_1" `Quick dd_test_horizontal_ambiguous_reference_1];
    "dd_test_cyclic_dependencies", [test_case "test_cyclic_dependencies" `Quick dd_test_cyclic_dependencies];
    "dd_test_dot_notation_2", [test_case "test_dot_notation_2" `Quick dd_test_dot_notation_2];
    "dd_test_another_ambiguous_reference", [test_case "test_another_ambiguous_reference" `Quick dd_test_another_ambiguous_reference];
    "dd_repeated_nt_dependency", [test_case "repeated_nt_dependency" `Quick dd_repeated_nt_dependency];
     "dd_test_strings", [test_case "test_strings" `Quick dd_test_strings]; 

    "md_test_sc", [test_case "Semantic constraint" `Quick md_test_sc];
    (* "md_test_placeholder", [test_case "Placeholder" `Quick md_test_placeholder]; *)
    "md_test_dt", [test_case "Dependent term" `Quick md_test_dt];
    (* "md_test_dt2", [test_case "Dependent term 2" `Quick md_test_dt2]; *)
    "md_test_dc", [test_case "Divide and conquer" `Quick md_test_dc];
    "md_test_bl", [test_case "Bit list" `Quick md_test_bl];
    "md_test_mult_prod_rules", [test_case "Test example with nonterminal with multiple prod rules, with semantic constraints" `Quick md_test_mult_prod_rules];
    "md_test_ty_annot_sc2", [test_case "Top level type annotation with semantic constraint 2" `Quick md_test_ty_annot_sc2];
    "md_test_bv_len", [test_case "Top length function on bitvector" `Quick md_test_bv_len];
    "md_test_dt3", [test_case "Dependent term 3" `Quick md_test_dt3]; 
    "md_test_dt4", [test_case "Dependent term 4" `Quick md_test_dt4];
    "md_test_dt5", [test_case "Dependent term 5" `Quick md_test_dt5];
    (* "md_test_dt6", [test_case "Dependent term 6" `Quick md_test_dt6]; *)
    "md_test_dynamic_typing", [test_case "Dynamic typing" `Quick md_test_dynamic_typing];
    "md_test_dot_notation",   [test_case "Test dot notation" `Quick md_test_dot_notation]; 
    "md_test_vertical_ambiguous_reference_1", [test_case "test_vertical_ambiguous_reference_1" `Quick md_test_vertical_ambiguous_reference_1];
    "md_test_vertical_ambiguous_reference_2", [test_case "test_vertical_ambiguous_reference_2" `Quick md_test_vertical_ambiguous_reference_2];
    "md_test_horizontal_ambiguous_reference_1", [test_case "test_horizontal_ambiguous_reference_1" `Quick md_test_horizontal_ambiguous_reference_1];
    "md_test_cyclic_dependencies", [test_case "test_cyclic_dependencies" `Quick md_test_cyclic_dependencies];
    "md_test_dot_notation_2", [test_case "test_dot_notation_2" `Quick md_test_dot_notation_2];
    "md_test_another_ambiguous_reference", [test_case "test_another_ambiguous_reference" `Quick md_test_another_ambiguous_reference];
    "md_overlapping_constraints", [test_case "overlapping_constraints" `Quick md_overlapping_constraints];
    "md_overlapping_constraints_2", [test_case "overlapping_constraints_2" `Quick md_overlapping_constraints_2];
    "md_repeated_nt_dependency", [test_case "repeated_nt_dependency" `Quick md_repeated_nt_dependency];
    "md_test_strings", [test_case "test_strings" `Quick md_test_strings]; 

    "sd_test2", [test_case "test2" `Quick sd_test2];
    "sd_test3", [test_case "test3" `Quick sd_test3];
    "sd_test4", [test_case "test4" `Quick sd_test4];
    "sd_test5", [test_case "test5" `Quick sd_test5];
    "sd_test6", [test_case "test6" `Quick sd_test6];
    "sd_test7", [test_case "test7" `Quick sd_test7];
    "sd_test8", [test_case "test8" `Quick sd_test8];
    "sd_test9", [test_case "test9" `Quick sd_test9];
    "sd_test14", [test_case "test14" `Quick sd_test14];
    "sd_test11", [test_case "test11" `Quick sd_test11];*)

    "dm_test2", [test_case "test2" `Quick dm_test2];
    "dm_test3", [test_case "test3" `Quick dm_test3];
    "dm_test4", [test_case "test4" `Quick dm_test4];
    "dm_test5", [test_case "test5" `Quick dm_test5];
    "dm_test6", [test_case "test6" `Quick dm_test6];
    "dm_test7", [test_case "test7" `Quick dm_test7];
    "dm_test8", [test_case "test8" `Quick dm_test8];
    "dm_test9", [test_case "test9" `Quick dm_test9];
    "dm_test14", [test_case "test14" `Quick dm_test14];
    "dm_test11", [test_case "test11" `Quick dm_test11];
    "dm_test12", [test_case "test12" `Quick dm_test12];
    "dm_test13", [test_case "test13" `Quick dm_test13];

    (*"dd_test3", [test_case "test3" `Quick dd_test3];
    "dd_test4", [test_case "test4" `Quick dd_test4];
    "dd_test5", [test_case "test5" `Quick dd_test5];
    "dd_test6", [test_case "test6" `Quick dd_test6];
    "dd_test7", [test_case "test7" `Quick dd_test7];
    "dd_test8", [test_case "test8" `Quick dd_test8];
    "dd_test9", [test_case "test9" `Quick dd_test9];
    "dd_test14", [test_case "test14" `Quick dd_test14];
    "dd_test11", [test_case "test11" `Quick dd_test11];
    "dd_test12", [test_case "test12" `Quick dd_test12]; 

   "md_test2", [test_case "test2" `Quick md_test2];
    (* "md_test3", [test_case "test3" `Quick md_test3]; *)
    "md_test4", [test_case "test4" `Quick md_test4];
    "md_test5", [test_case "test5" `Quick md_test5];
    "md_test6", [test_case "test6" `Quick md_test6];
    "md_test7", [test_case "test7" `Quick md_test7];
    "md_test8", [test_case "test8" `Quick md_test8];
    "md_test9", [test_case "test9" `Quick md_test9];
    "md_test14", [test_case "test14" `Quick md_test14];
    "md_test11", [test_case "test11" `Quick md_test11];*)

    (*"sd_test17", [test_case "test17" `Quick sd_test17];*)
    "dm_test17", [test_case "test17" `Quick dm_test17];
    (*"dd_test17", [test_case "test17" `Quick dd_test17];
    "md_test17", [test_case "test17" `Quick md_test17];

    "sd_test16", [test_case "test16" `Quick sd_test16];*)
    "dm_test16", [test_case "test16" `Quick dm_test16];
    (*"md_test16", [test_case "test16" `Quick md_test16];

    "sd_test18", [test_case "test18" `Quick sd_test18];*)
    "dm_test18", [test_case "test18" `Quick dm_test18];
    (*"dd_test18", [test_case "test18" `Quick dd_test18];
    "md_test18", [test_case "test18" `Quick md_test18];*)

    "test10", [test_case "test10" `Quick test10];

    "test_check_solver_ast_6", [test_case "test_check_solver_ast_6" `Quick test_check_solver_ast_6];
    "test_check_solver_ast_7", [test_case "test_check_solver_ast_7" `Quick test_check_solver_ast_7]; 

    (*"sd_test_another_ambiguous_reference_1", [test_case "sd_test_another_ambiguous_reference_1" `Quick sd_test_another_ambiguous_reference_1]; *)
    "dm_test_another_ambiguous_reference_1", [test_case "dm_test_another_ambiguous_reference_1" `Quick dm_test_another_ambiguous_reference_1]; 
    (*"dd_test_another_ambiguous_reference_1", [test_case "dd_test_another_ambiguous_reference_1" `Quick dd_test_another_ambiguous_reference_1]; 
    "md_test_another_ambiguous_reference_1", [test_case "md_test_another_ambiguous_reference_1" `Quick md_test_another_ambiguous_reference_1]; *)
    (*"sd_test_another_ambiguous_reference_2", [test_case "sd_test_another_ambiguous_reference_2" `Quick sd_test_another_ambiguous_reference_2];  *)
    "dm_test_another_ambiguous_reference_2", [test_case "dm_test_another_ambiguous_reference_2" `Quick dm_test_another_ambiguous_reference_2]; 
    "bug1", [test_case "bug1" `Quick bug1]; 
    "bug3", [test_case "bug3" `Quick bug3]; 
    "bug4", [test_case "bug4" `Quick bug4]; 
    "bug5", [test_case "bug5" `Quick bug5]; 
    "bug6", [test_case "bug6" `Quick bug6]; 
    "bug2", [test_case "bug2" `Quick bug2]; 
    "probabilities", [test_case "probabilities" `Quick probabilities]; 
    "probabilities_2", [test_case "probabilities_2" `Quick probabilities_2]; 
    "example-fail", [test_case "example-fail" `Quick example_fail]; 
    "ngap-ngsetup", [test_case "ngap-ngsetup" `Quick ngap_ngsetup_bug]; 
    "reset_bug4", [test_case "reset_bug4" `Quick reset_bug4]; 
    (*"msg2", [test_case "msg2" `Quick msg2]; *)
    (*"dd_test_another_ambiguous_reference_2", [test_case "dd_test_another_ambiguous_reference_2" `Quick dd_test_another_ambiguous_reference_2]; 
    "md_test_another_ambiguous_reference_2", [test_case "md_test_another_ambiguous_reference_2" `Quick md_test_another_ambiguous_reference_2];  *)


  ]
