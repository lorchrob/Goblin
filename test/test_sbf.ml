open Sbf.Pipeline
open Sbf
open Alcotest
module SA = SygusAst

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
  let sygus_ast = SygusAst.Node (("A", None), [SygusAst.Node (("B", None), [SygusAst.VarLeaf ""]); SygusAst.Node (("C", None), [SygusAst.VarLeaf ""])]) in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let test_check_sygus_ast_2 () =
  let filename = "../../../test/test_cases/test_check_sygus_ast" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let sygus_ast = SygusAst.Node (("A", None), [SygusAst.Node (("D", None), [SygusAst.VarLeaf ""]); SygusAst.Node (("C", None), [SygusAst.VarLeaf ""])]) in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> fail "Expected error"
  | Error _ -> ()

let test_check_sygus_ast_3 () =
  let filename = "../../../test/test_cases/test_check_sygus_ast_2" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let sygus_ast = SygusAst.Node (("A", None), [SygusAst.Node (("B", None), [SygusAst.IntLeaf 3]); SygusAst.Node (("C", None), [SygusAst.IntLeaf 2])]) in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()
  | Error msg -> fail msg

let test_check_sygus_ast_4 () =
  let filename = "../../../test/test_cases/test_check_sygus_ast_2" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let sygus_ast = SygusAst.Node (("A", None), [SygusAst.Node (("B", None), [SygusAst.IntLeaf 1]); SygusAst.Node (("C", None), [SygusAst.IntLeaf 2])]) in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> fail "Expected failure"
  | Error _ -> ()

let test_check_sygus_ast_5 () =
  let filename = "../../../test/test_cases/test_check_sygus_ast_2" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let sygus_ast = SygusAst.Node (("C", None), [SygusAst.Node (("G", None), [SygusAst.IntLeaf 1])]) in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> fail "Expected failure"
  | Error _ -> ()

let sd_test_another_ambiguous_reference_1 () =
  (* TODO: Fix ugly paths. The test_cases directory is currently not included in the build directory, 
           so we have to reference it from the build directory. *)
  let input = "../../../test/test_cases/test_another_ambiguous_reference_1" in
  let _, output = main_pipeline ~engine:(Some SygusDac) input in
  check string "test_another_ambiguous_reference_1" output "infeasible\n"

let sd_test_another_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference_2" in
  let _, output = main_pipeline ~engine:(Some SygusDac) input in
  check string "test_another_ambiguous_reference_2" output "infeasible\n"

let sd_test_another_ambiguous_reference () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dot_notation_2 () =
  let input = "../../../test/test_cases/test_dot_notation_2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_cyclic_dependencies () =
  let input = "../../../test/test_cases/test_cyclic_dependencies" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_horizontal_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_horizontal_ambiguous_reference_1" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_vertical_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_1" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_vertical_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Semantic constraint example *)
let sd_test_sc () =
  let input = "../../../test/test_cases/test_sc" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_placeholder () =
  let input = "../../../test/test_cases/test_placeholder" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Dependent term calculation example *)
let sd_test_dt () =
  let input = "../../../test/test_cases/test_dt" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dt2 () =
  let input = "../../../test/test_cases/test_dt2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Divide and conquer example *)
let sd_test_dc () = 
  let input = "../../../test/test_cases/test_dc" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_bl () = 
  let input = "../../../test/test_cases/test_bl" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_ty_annot_sc2 () = 
  let input = "../../../test/test_cases/test_ty_annot_sc2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_mult_prod_rules () = 
  let input = "../../../test/test_cases/test_mult_prod_rules" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_bv_len () = 
  let input = "../../../test/test_cases/test_bv_len" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dt3 () = 
  let input = "../../../test/test_cases/test_dt3" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dt4 () = 
  let input = "../../../test/test_cases/test_dt4" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dt5 () = 
  let input = "../../../test/test_cases/test_dt5" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dt6 () = 
  let input = "../../../test/test_cases/test_dt6" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* let sd_test_recombine () =
  let input = "../../../test/test_cases/test_recombine" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg *)
  
let sd_test_dynamic_typing () = 
  let input = "../../../test/test_cases/test_dynamic_typing" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_dot_notation () = 
  let input = "../../../test/test_cases/test_dot_notation" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_overlapping_constraints () = 
  let input = "../../../test/test_cases/overlapping_constraints" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_overlapping_constraints_2 () = 
  let input = "../../../test/test_cases/overlapping_constraints_2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_repeated_nt_dependency () = 
  let input = "../../../test/test_cases/repeated_nt_dependency" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test_strings () = 
  let input = "../../../test/test_cases/test_strings" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_another_ambiguous_reference_1 () =
  (* TODO: Fix ugly paths. The test_cases directory is currently not included in the build directory, 
           so we have to reference it from the build directory. *)
  let input = "../../../test/test_cases/test_another_ambiguous_reference_1" in
  let _, output = main_pipeline ~engine:(Some DpllMono) input in
  check string "test_another_ambiguous_reference_1" output "infeasible\n"

let dm_test_another_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference_2" in
  let _, output = main_pipeline ~engine:(Some DpllMono) input in
  check string "test_another_ambiguous_reference_2" output "infeasible\n"

let dm_test_another_ambiguous_reference () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dot_notation_2 () =
  let input = "../../../test/test_cases/test_dot_notation_2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_cyclic_dependencies () =
  let input = "../../../test/test_cases/test_cyclic_dependencies" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_horizontal_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_horizontal_ambiguous_reference_1" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_vertical_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_1" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_vertical_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Semantic constraint example *)
let dm_test_sc () =
  let input = "../../../test/test_cases/test_sc" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_placeholder () =
  let input = "../../../test/test_cases/test_placeholder" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Dependent term calculation example *)
let dm_test_dt () =
  let input = "../../../test/test_cases/test_dt" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dt2 () =
  let input = "../../../test/test_cases/test_dt2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Divide and conquer example *)
let dm_test_dc () = 
  let input = "../../../test/test_cases/test_dc" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_bl () = 
  let input = "../../../test/test_cases/test_bl" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_ty_annot_sc2 () = 
  let input = "../../../test/test_cases/test_ty_annot_sc2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_mult_prod_rules () = 
  let input = "../../../test/test_cases/test_mult_prod_rules" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_bv_len () = 
  let input = "../../../test/test_cases/test_bv_len" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dt3 () = 
  let input = "../../../test/test_cases/test_dt3" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dt4 () = 
  let input = "../../../test/test_cases/test_dt4" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dt5 () = 
  let input = "../../../test/test_cases/test_dt5" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dt6 () = 
  let input = "../../../test/test_cases/test_dt6" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_recombine () =
  let input = "../../../test/test_cases/test_recombine" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg
  
let dm_test_dynamic_typing () = 
  let input = "../../../test/test_cases/test_dynamic_typing" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_dot_notation () = 
  let input = "../../../test/test_cases/test_dot_notation" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_overlapping_constraints () = 
  let input = "../../../test/test_cases/overlapping_constraints" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_overlapping_constraints_2 () = 
  let input = "../../../test/test_cases/overlapping_constraints_2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_repeated_nt_dependency () = 
  let input = "../../../test/test_cases/repeated_nt_dependency" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test_strings () = 
  let input = "../../../test/test_cases/test_strings" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_another_ambiguous_reference_1 () =
  (* TODO: Fix ugly paths. The test_cases directory is currently not included in the build directory, 
           so we have to reference it from the build directory. *)
  let input = "../../../test/test_cases/test_another_ambiguous_reference_1" in
  let _, output = main_pipeline ~engine:(Some DpllDac) input in
  check string "test_another_ambiguous_reference_1" output "infeasible\n"

let dd_test_another_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference_2" in
  let _, output = main_pipeline ~engine:(Some DpllDac) input in
  check string "test_another_ambiguous_reference_2" output "infeasible\n"

let dd_test_another_ambiguous_reference () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dot_notation_2 () =
  let input = "../../../test/test_cases/test_dot_notation_2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_cyclic_dependencies () =
  let input = "../../../test/test_cases/test_cyclic_dependencies" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_horizontal_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_horizontal_ambiguous_reference_1" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_vertical_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_1" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_vertical_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Semantic constraint example *)
let dd_test_sc () =
  let input = "../../../test/test_cases/test_sc" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_placeholder () =
  let input = "../../../test/test_cases/test_placeholder" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Dependent term calculation example *)
let dd_test_dt () =
  let input = "../../../test/test_cases/test_dt" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dt2 () =
  let input = "../../../test/test_cases/test_dt2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Divide and conquer example *)
let dd_test_dc () = 
  let input = "../../../test/test_cases/test_dc" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_bl () = 
  let input = "../../../test/test_cases/test_bl" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_ty_annot_sc2 () = 
  let input = "../../../test/test_cases/test_ty_annot_sc2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_mult_prod_rules () = 
  let input = "../../../test/test_cases/test_mult_prod_rules" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_bv_len () = 
  let input = "../../../test/test_cases/test_bv_len" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dt3 () = 
  let input = "../../../test/test_cases/test_dt3" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dt4 () = 
  let input = "../../../test/test_cases/test_dt4" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dt5 () = 
  let input = "../../../test/test_cases/test_dt5" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dt6 () = 
  let input = "../../../test/test_cases/test_dt6" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_recombine () =
  let input = "../../../test/test_cases/test_recombine" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg
  
let dd_test_dynamic_typing () = 
  let input = "../../../test/test_cases/test_dynamic_typing" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_dot_notation () = 
  let input = "../../../test/test_cases/test_dot_notation" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_repeated_nt_dependency () = 
  let input = "../../../test/test_cases/repeated_nt_dependency" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test_strings () = 
  let input = "../../../test/test_cases/test_strings" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_another_ambiguous_reference_1 () =
  (* TODO: Fix ugly paths. The test_cases directory is currently not included in the build directory, 
           so we have to reference it from the build directory. *)
  let input = "../../../test/test_cases/test_another_ambiguous_reference_1" in
  let _, output = main_pipeline ~engine:(Some MixedDac) input in
  check string "test_another_ambiguous_reference_1" output "infeasible\n"

let md_test_another_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference_2" in
  let _, output = main_pipeline ~engine:(Some MixedDac) input in
  check string "test_another_ambiguous_reference_2" output "infeasible\n"

let md_test_another_ambiguous_reference () =
  let input = "../../../test/test_cases/test_another_ambiguous_reference" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_dot_notation_2 () =
  let input = "../../../test/test_cases/test_dot_notation_2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_cyclic_dependencies () =
  let input = "../../../test/test_cases/test_cyclic_dependencies" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_horizontal_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_horizontal_ambiguous_reference_1" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_vertical_ambiguous_reference_1 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_1" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_vertical_ambiguous_reference_2 () =
  let input = "../../../test/test_cases/test_vertical_ambiguous_reference_2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Semantic constraint example *)
let md_test_sc () =
  let input = "../../../test/test_cases/test_sc" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_placeholder () =
  let input = "../../../test/test_cases/test_placeholder" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Dependent term calculation example *)
let md_test_dt () =
  let input = "../../../test/test_cases/test_dt" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_dt2 () =
  let input = "../../../test/test_cases/test_dt2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Divide and conquer example *)
let md_test_dc () = 
  let input = "../../../test/test_cases/test_dc" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_bl () = 
  let input = "../../../test/test_cases/test_bl" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_ty_annot_sc2 () = 
  let input = "../../../test/test_cases/test_ty_annot_sc2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_mult_prod_rules () = 
  let input = "../../../test/test_cases/test_mult_prod_rules" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_bv_len () = 
  let input = "../../../test/test_cases/test_bv_len" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_dt3 () = 
  let input = "../../../test/test_cases/test_dt3" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_dt4 () = 
  let input = "../../../test/test_cases/test_dt4" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_dt5 () = 
  let input = "../../../test/test_cases/test_dt5" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_dt6 () = 
  let input = "../../../test/test_cases/test_dt6" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* let md_test_recombine () =
  let input = "../../../test/test_cases/test_recombine" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg *)
  
let md_test_dynamic_typing () = 
  let input = "../../../test/test_cases/test_dynamic_typing" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_dot_notation () = 
  let input = "../../../test/test_cases/test_dot_notation" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_overlapping_constraints () = 
  let input = "../../../test/test_cases/overlapping_constraints" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_overlapping_constraints_2 () = 
  let input = "../../../test/test_cases/overlapping_constraints_2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_repeated_nt_dependency () = 
  let input = "../../../test/test_cases/repeated_nt_dependency" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test_strings () = 
  let input = "../../../test/test_cases/test_strings" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test2 () = 
  let input = "../../../test/test_cases/test2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test3 () = 
  let input = "../../../test/test_cases/test3" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test4 () = 
  let input = "../../../test/test_cases/test4" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test5 () = 
  let input = "../../../test/test_cases/test5" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test6 () = 
  let input = "../../../test/test_cases/test6" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test7 () = 
  let input = "../../../test/test_cases/test7" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test8 () = 
  let input = "../../../test/test_cases/test8" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test9 () = 
  let input = "../../../test/test_cases/test9" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test13 () = 
  let input = "../../../test/test_cases/test13" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test11 () = 
  let input = "../../../test/test_cases/test11" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test12 () = 
  let input = "../../../test/test_cases/test12" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let sd_test14 () = 
  let input = "../../../test/test_cases/test14" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test2 () = 
  let input = "../../../test/test_cases/test2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test3 () = 
  let input = "../../../test/test_cases/test3" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test4 () = 
  let input = "../../../test/test_cases/test4" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test5 () = 
  let input = "../../../test/test_cases/test5" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test6 () = 
  let input = "../../../test/test_cases/test6" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test7 () = 
  let input = "../../../test/test_cases/test7" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test8 () = 
  let input = "../../../test/test_cases/test8" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test9 () = 
  let input = "../../../test/test_cases/test9" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test13 () = 
  let input = "../../../test/test_cases/test13" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test11 () = 
  let input = "../../../test/test_cases/test11" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test12 () = 
  let input = "../../../test/test_cases/test12" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dm_test14 () = 
  let input = "../../../test/test_cases/test14" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllMono) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test2 () = 
  let input = "../../../test/test_cases/test2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test3 () = 
  let input = "../../../test/test_cases/test3" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test4 () = 
  let input = "../../../test/test_cases/test4" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test5 () = 
  let input = "../../../test/test_cases/test5" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test6 () = 
  let input = "../../../test/test_cases/test6" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test7 () = 
  let input = "../../../test/test_cases/test7" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test8 () = 
  let input = "../../../test/test_cases/test8" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test9 () = 
  let input = "../../../test/test_cases/test9" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test13 () = 
  let input = "../../../test/test_cases/test13" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test11 () = 
  let input = "../../../test/test_cases/test11" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test12 () = 
  let input = "../../../test/test_cases/test12" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let dd_test14 () = 
  let input = "../../../test/test_cases/test14" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some DpllDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test2 () = 
  let input = "../../../test/test_cases/test2" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test3 () = 
  let input = "../../../test/test_cases/test3" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test4 () = 
  let input = "../../../test/test_cases/test4" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test5 () = 
  let input = "../../../test/test_cases/test5" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test6 () = 
  let input = "../../../test/test_cases/test6" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some SygusDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test7 () = 
  let input = "../../../test/test_cases/test7" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test8 () = 
  let input = "../../../test/test_cases/test8" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test9 () = 
  let input = "../../../test/test_cases/test9" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test13 () = 
  let input = "../../../test/test_cases/test13" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test11 () = 
  let input = "../../../test/test_cases/test11" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test12 () = 
  let input = "../../../test/test_cases/test12" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let md_test14 () = 
  let input = "../../../test/test_cases/test14" in
  let ast = Parsing.parse (Utils.read_file input) in
  let sygus_ast, _ = main_pipeline ~engine:(Some MixedDac) input in
  let output = CheckSygusAst.check_sygus_ast ast sygus_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let () = 
  run "My_module" [
    "test_check_sygus_ast", [test_case "test_check_sygus_ast" `Quick test_check_sygus_ast];
    "test_check_sygus_ast_2", [test_case "test_check_sygus_ast_2" `Quick test_check_sygus_ast_2];
    "test_check_sygus_ast_3", [test_case "test_check_sygus_ast_3" `Quick test_check_sygus_ast_3];
    "test_check_sygus_ast_4", [test_case "test_check_sygus_ast_4" `Quick test_check_sygus_ast_4];
    "test_check_sygus_ast_5", [test_case "test_check_sygus_ast_5" `Quick test_check_sygus_ast_5];

    "sd_test_sc", [test_case "Semantic constraint" `Quick sd_test_sc];
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
    (* not applicable "sd_test_recombine", [test_case "Recombine" `Quick sd_test_recombine]; *)
    "sd_test_dot_notation", [test_case "Test dot notation" `Quick sd_test_dot_notation];
    "sd_test_vertical_ambiguous_reference_1", [test_case "test_vertical_ambiguous_reference_1" `Quick sd_test_vertical_ambiguous_reference_1];
    "sd_test_vertical_ambiguous_reference_2", [test_case "test_vertical_ambiguous_reference_2" `Quick sd_test_vertical_ambiguous_reference_2];
    "sd_test_horizontal_ambiguous_reference_1", [test_case "test_horizontal_ambiguous_reference_1" `Quick sd_test_horizontal_ambiguous_reference_1];
    "sd_test_cyclic_dependencies", [test_case "test_cyclic_dependencies" `Quick sd_test_cyclic_dependencies];
    "sd_test_dot_notation_2", [test_case "test_dot_notation_2" `Quick sd_test_dot_notation_2];
    "sd_test_another_ambiguous_reference", [test_case "test_another_ambiguous_reference" `Quick sd_test_another_ambiguous_reference];
    "sd_test_another_ambiguous_reference_1", [test_case "test_another_ambiguous_reference_1" `Quick sd_test_another_ambiguous_reference_1];
    "sd_test_another_ambiguous_reference_2", [test_case "test_another_ambiguous_reference_2" `Quick sd_test_another_ambiguous_reference_2];
    "sd_overlapping_constraints", [test_case "overlapping_constraints" `Quick sd_overlapping_constraints];
    "sd_overlapping_constraints_2", [test_case "overlapping_constraints_2" `Quick sd_overlapping_constraints_2];
    "sd_repeated_nt_dependency", [test_case "repeated_nt_dependency" `Quick sd_repeated_nt_dependency];
    "sd_test_strings", [test_case "test_strings" `Quick sd_test_strings];
 
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
    (* "dm_test_another_ambiguous_reference_1", [test_case "test_another_ambiguous_reference_1" `Quick dm_test_another_ambiguous_reference_1]; *)
    (* "dm_test_another_ambiguous_reference_2", [test_case "test_another_ambiguous_reference_2" `Quick dm_test_another_ambiguous_reference_2]; *)
    "dm_overlapping_constraints", [test_case "overlapping_constraints" `Quick dm_overlapping_constraints];
    "dm_overlapping_constraints_2", [test_case "overlapping_constraints_2" `Quick dm_overlapping_constraints_2];
    "dm_repeated_nt_dependency", [test_case "repeated_nt_dependency" `Quick dm_repeated_nt_dependency];
    "dm_test_strings", [test_case "test_strings" `Quick dm_test_strings];
 
    "dd_test_sc", [test_case "Semantic constraint" `Quick dd_test_sc];
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
    (* "dd_test_another_ambiguous_reference_1", [test_case "test_another_ambiguous_reference_1" `Quick dd_test_another_ambiguous_reference_1]; *)
    (* "dd_test_another_ambiguous_reference_2", [test_case "test_another_ambiguous_reference_2" `Quick dd_test_another_ambiguous_reference_2]; *)
    "dd_repeated_nt_dependency", [test_case "repeated_nt_dependency" `Quick dd_repeated_nt_dependency];
     "dd_test_strings", [test_case "test_strings" `Quick dd_test_strings];

    "md_test_sc", [test_case "Semantic constraint" `Quick md_test_sc];
    "md_test_placeholder", [test_case "Placeholder" `Quick md_test_placeholder];
    "md_test_dt", [test_case "Dependent term" `Quick md_test_dt];
    "md_test_dt2", [test_case "Dependent term 2" `Quick md_test_dt2];
    "md_test_dc", [test_case "Divide and conquer" `Quick md_test_dc];
    "md_test_bl", [test_case "Bit list" `Quick md_test_bl];
    "md_test_mult_prod_rules", [test_case "Test example with nonterminal with multiple prod rules, with semantic constraints" `Quick md_test_mult_prod_rules];
    "md_test_ty_annot_sc2", [test_case "Top level type annotation with semantic constraint 2" `Quick md_test_ty_annot_sc2];
    "md_test_bv_len", [test_case "Top length function on bitvector" `Quick md_test_bv_len];
    "md_test_dt3", [test_case "Dependent term 3" `Quick md_test_dt3];
    "md_test_dt4", [test_case "Dependent term 4" `Quick md_test_dt4];
    "md_test_dt5", [test_case "Dependent term 5" `Quick md_test_dt5];
    "md_test_dt6", [test_case "Dependent term 6" `Quick md_test_dt6];
    "md_test_dynamic_typing", [test_case "Dynamic typing" `Quick md_test_dynamic_typing];
    (* not applicable "md_test_recombine", [test_case "Recombine" `Quick md_test_recombine]; *)
    "md_test_dot_notation", [test_case "Test dot notation" `Quick md_test_dot_notation];
    "md_test_vertical_ambiguous_reference_1", [test_case "test_vertical_ambiguous_reference_1" `Quick md_test_vertical_ambiguous_reference_1];
    "md_test_vertical_ambiguous_reference_2", [test_case "test_vertical_ambiguous_reference_2" `Quick md_test_vertical_ambiguous_reference_2];
    "md_test_horizontal_ambiguous_reference_1", [test_case "test_horizontal_ambiguous_reference_1" `Quick md_test_horizontal_ambiguous_reference_1];
    "md_test_cyclic_dependencies", [test_case "test_cyclic_dependencies" `Quick md_test_cyclic_dependencies];
    "md_test_dot_notation_2", [test_case "test_dot_notation_2" `Quick md_test_dot_notation_2];
    "md_test_another_ambiguous_reference", [test_case "test_another_ambiguous_reference" `Quick md_test_another_ambiguous_reference];
    (* "md_test_another_ambiguous_reference_1", [test_case "test_another_ambiguous_reference_1" `Quick md_test_another_ambiguous_reference_1]; *)
    (*!! "md_test_another_ambiguous_reference_2", [test_case "test_another_ambiguous_reference_2" `Quick md_test_another_ambiguous_reference_2]; *)
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
    "sd_test11", [test_case "test11" `Quick sd_test11];
    "sd_test12", [test_case "test12" `Quick sd_test12];
    "sd_test13", [test_case "test13" `Quick sd_test13];
  ]