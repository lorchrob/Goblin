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
  match main_pipeline input with
  | _ -> Alcotest.fail "Expected exception, but got success"
  | exception _ -> ()  

let bug4 () =
  let input = "../../../test/test_cases/bug4.gbl" in
  match main_pipeline input with
  | _ -> Alcotest.fail "Expected exception, but got success"
  | exception _ -> ()  

let contains_substring s sub =
  let n = String.length s and m = String.length sub in
  let rec loop i = i + m <= n && (String.sub s i m = sub || loop (i + 1)) in
  loop 0

let expect_error input expected_msg =
  match main_pipeline input with
  | _ -> Alcotest.fail "Expected exception, but got success"
  | exception Failure msg ->
    if not (contains_substring msg expected_msg) then
      Alcotest.failf "Expected error containing %S, but got %S" expected_msg msg

let inh_attr_fail_1 () =
  expect_error "../../../test/test_cases/inh-attr-fail-1.gbl"
    "Nonterminal L is passed the incorrect number of inherited attributes (found 0, expected 1)"

let inh_attr_fail_2 () =
  expect_error "../../../test/test_cases/inh-attr-fail-2.gbl"
    "Inherited attribute passed to nonterminal L has expected type Int but inferred type Bool"

let inh_attr_fail_3 () =
  expect_error "../../../test/test_cases/inh-attr-fail-3.gbl"
    "Unknown identifier w"

let inh_attr_fail_4 () =
  expect_error "../../../test/test_cases/inh-attr-fail-4.gbl"
    "Unknown identifier v"

let inh_attr_fail_5 () =
  expect_error "../../../test/test_cases/inh-attr-fail-5.gbl"
    "Unknown identifier v (attributes cannot be referenced in type annotations)"

let inh_attr_fail_6 () =
  expect_error "../../../test/test_cases/inh-attr-fail-6.gbl"
    "Nonterminal <L> declares the same inherited attribute more than once"

let inh_attr_fail_7 () =
  expect_error "../../../test/test_cases/inh-attr-fail-7.gbl"
    "Inherited attribute passed to nonterminal B has expected type Bool but inferred type Int"

let inh_attr_fail_8 () =
  expect_error "../../../test/test_cases/inh-attr-fail-8.gbl"
    "v is an inherited attribute of <L>, so it cannot be accessed with dot notation"

let inh_attr_fail_9 () =
  expect_error "../../../test/test_cases/inh-attr-fail-9.gbl"
    "Nonterminal <L> declares len as an inherited attribute and also defines it as a synthesized attribute"

let inh_attr_fail_10 () =
  expect_error "../../../test/test_cases/inh-attr-fail-10.gbl"
    "Unknown identifier size"

let bug3 () =
  let input = "../../../test/test_cases/bug3.gbl" in
  match main_pipeline input with
  | _ -> Alcotest.fail "Expected exception, but got success"
  | exception _ -> ()  

let bug8 () =
  let input = "../../../test/test_cases/bug8.gbl" in
  match main_pipeline input with
  | _ -> Alcotest.fail "Expected exception, but got success"
  | exception _ -> ()  

let bug2 () =
  let input = "../../../test/test_cases/bug2.gbl" in
  let solver_ast, _, ast = main_pipeline input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let index () =
  let input = "../../../test/test_cases/index.gbl" in
  let solver_ast, _, ast = main_pipeline input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let too_many_constraints () =
  let input = "../../../test/test_cases/too_many_constraints.gbl" in
  let solver_ast, _, ast = main_pipeline input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg


let test_check_solver_ast () =
  let filename = "../../../test/test_cases/test_check_solver_ast" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in
  let solver_ast = SolverAst.Node ((Nt.User "A", None, None), [SolverAst.Node ((Nt.User "B", None, None), [SolverAst.Node ((Nt.User "G", None, None), [SolverAst.Leaf (Int 0)])]); SolverAst.Node ((Nt.User "C", None, None), [SolverAst.Node ((Nt.User "G", None, None), [SolverAst.Leaf (Int 0)])])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let test_check_solver_ast_2 () =
  let filename = "../../../test/test_cases/test_check_solver_ast" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let solver_ast = SolverAst.Node ((Nt.User "A", None, None), [SolverAst.Node ((Nt.User "D", None, None), [SolverAst.Node ((Nt.User "G", None, None), [SolverAst.Leaf (Int 0)])]); SolverAst.Node ((Nt.User "C", None, None), [SolverAst.Node ((Nt.User "G", None, None), [SolverAst.Leaf (Int 0)])])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> fail "Expected error"
  | Error _ -> ()

let test_check_solver_ast_3 () =
  let filename = "../../../test/test_cases/test_check_solver_ast_2" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let solver_ast = SolverAst.Node ((Nt.User "A", None, None), [SolverAst.Node ((Nt.User "B", None, None), [SolverAst.Node ((Nt.User "G", None, None), [SolverAst.Leaf (Int 3)])]); SolverAst.Node ((Nt.User "C", None, None), [SolverAst.Node ((Nt.User "G", None, None), [SolverAst.Leaf (Int 2)])])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()
  | Error msg -> fail msg

let test_check_solver_ast_4 () =
  let filename = "../../../test/test_cases/test_check_solver_ast_2" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let solver_ast = SolverAst.Node ((Nt.User "A", None, None), [SolverAst.Node ((Nt.User "B", None, None), [SolverAst.Node ((Nt.User "G", None, None), [SolverAst.Leaf (Int 1)])]); SolverAst.Node ((Nt.User "C", None, None), [SolverAst.Node ((Nt.User "G", None, None), [SolverAst.Leaf (Int (2))])])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> fail "Expected failure"
  | Error _ -> ()

let test_check_solver_ast_5 () =
  let filename = "../../../test/test_cases/test_check_solver_ast_2" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let solver_ast = SolverAst.Node ((Nt.User "C", None, None), [SolverAst.Node ((Nt.User "G", None, None), [SolverAst.Leaf (Int 1)])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> fail "Expected failure"
  | Error _ -> ()

let test_check_solver_ast_6 () =
  let filename = "../../../test/test_cases/test_check_solver_ast_3" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let solver_ast = SolverAst.Node ((Nt.User "A", None, None), [SolverAst.Node ((Nt.User "B", None, None), [SolverAst.Node ((Nt.User "G", None, None), [SolverAst.Leaf (Int (-1))])]); SolverAst.Node ((Nt.User "C", None, None), [SolverAst.Node ((Nt.User "G", None, None), [SolverAst.Leaf (Int (-2))])])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()
  | Error msg -> fail msg

let test_check_solver_ast_7 () =
  let filename = "../../../test/test_cases/test_check_solver_ast_3" in
  let input = Utils.read_file filename in 
  let ast = Parsing.parse input in 
  let solver_ast = SolverAst.Node ((Nt.User "A", None, None), [SolverAst.Node ((Nt.User "B", None, None), [SolverAst.Node ((Nt.User "G", None, None), [SolverAst.Leaf (Int (2))])]); SolverAst.Node ((Nt.User "C", None, None), [SolverAst.Node ((Nt.User "G", None, None), [SolverAst.Leaf (Int (1))])])]) in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> fail "Expected failure"
  | Error _ -> ()

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
  match main_pipeline input with
  | exception _ -> () 
  | _ -> fail "should fail"

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
  match main_pipeline input with
  | _ -> Alcotest.fail "Expected exception, but got success"
  | exception _ -> ()  

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

let length_attr () =
  let input = "../../../test/test_cases/length-attr.gbl" in
  let solver_ast, _, ast = main_pipeline input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let length_attr_fail_1 () =
  let input = "../../../test/test_cases/length-attr-fail-1.gbl" in
  match main_pipeline input with
  | _ -> Alcotest.fail "Expected exception, but got success"
  | exception _ -> ()  

let length_attr_fail_2 () =
  let input = "../../../test/test_cases/length-attr-fail-2.gbl" in
  match main_pipeline input with
  | _ -> Alcotest.fail "Expected exception, but got success"
  | exception _ -> ()  

let length_attr_fail_3 () =
  let input = "../../../test/test_cases/length-attr-fail-3.gbl" in
  match main_pipeline input with
  | _ -> Alcotest.fail "Expected exception, but got success"
  | exception _ -> ()  

let inh_attr () =
  let input = "../../../test/test_cases/inh-attr.gbl" in
  let solver_ast, _, ast = main_pipeline input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let inh_attr_scoped () =
  let input = "../../../test/test_cases/inh-attr-scoped.gbl" in
  let solver_ast, _, ast = main_pipeline input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

let inh_attr_synth_same_name () =
  expect_error "../../../test/test_cases/inh-attr-synth-same-name.gbl"
    "Nonterminal <L> declares len as an inherited attribute and also defines it as a synthesized attribute"

(* A bare attribute name refers to the enclosing nonterminal's synthesized attribute *)
let inh_attr_own_synth () =
  let input = "../../../test/test_cases/inh-attr-own-synth.gbl" in
  let solver_ast, _, ast = main_pipeline input in
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

(* Nonterminal names ending in "_con" *)
let con_suffix () =
  List.iter (fun engine -> 
    List.iter (fun input -> 
      let solver_ast, _, ast = main_pipeline ~engine:(Some engine) input in
      match CheckSolverAst.check_solver_ast ast solver_ast with
      | Ok _ -> ()  
      | Error msg -> fail msg
    ) ["../../../test/test_cases/con-suffix.gbl"; "../../../test/test_cases/con-suffix-2.gbl"]
  ) [Flags.DpllMono; Flags.DpllDac]

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
  let output = CheckSolverAst.check_solver_ast ast solver_ast in
  match output with
  | Ok _ -> ()  
  | Error msg -> fail msg

  *)

let dm_test17 () = 
  let input = "../../../test/test_cases/test17" in
  match main_pipeline input with
  | _ -> Alcotest.fail "Expected exception, but got success"
  | exception _ -> ()  

(*let dd_test17 () = 
  let input = "../../../test/test_cases/test17" in
  try 
    let _ = main_pipeline ~engine:(Some DpllDac) input in
    fail "expected error"
  with _ -> () 

  *)

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

  *)

let dm_test2 () = 
  let input = "../../../test/test_cases/test2" in
  match main_pipeline ~engine:(Some DpllMono) input with
  | exception _ -> () 
  | _ -> fail "should fail"

let reset_bug4 () = 
  let input = "../../../test/test_cases/ngap-ngsetup-bug4.gbl" in
  match main_pipeline input with
  | exception _ -> () 
  | _ -> fail "should fail"

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

  *)

let example_fail () = 
  let input = "../../../test/test_cases/example_fail.gbl" in
  match main_pipeline input with
  | exception _ -> () 
  | _ -> fail "should fail"

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

(* Terms built by hand, so the checker can be tested on inputs the engine would
   never produce *)
let bv bits =
  SolverAst.Leaf (Value.BitVector (String.length bits,
    List.of_seq (String.to_seq bits |> Seq.map (fun c -> c = '1'))))

let leaf_node name value = SolverAst.Node ((Nt.User name, None, None), [value])

(* test_dt6 is <S> ::= <A> <B> <C> with <A> <- <B> and <B> <- <C>, so all three
   must agree *)
let dt6_term children = SolverAst.Node ((Nt.User "S", Some 0, Some 0), children)

let dt6_children a b c = [leaf_node "A" (bv a); leaf_node "B" (bv b); leaf_node "C" (bv c)]

(* Checks a term against a grammar without running the search *)
let verdict_of_term name solver_ast =
  let _, _, ast = Pipeline.front_end ("../../../test/test_cases/" ^ name) in
  CheckSolverAst.verdict_of ast solver_ast

let check_term name solver_ast =
  let input = "../../../test/test_cases/" ^ name in
  let _, _, ast = Pipeline.front_end input in
  CheckSolverAst.check_solver_ast ast solver_ast

let expect_valid name solver_ast () =
  match check_term name solver_ast with
  | Ok () -> ()
  | Error msg -> fail msg

let expect_invalid name solver_ast () =
  match check_term name solver_ast with
  | Ok () -> fail "Expected the term to be rejected, but it was accepted"
  | Error _ -> ()

(* For rejections where which defect is reported matters, not just that one is *)
let expect_rejected_with name solver_ast expected () =
  match check_term name solver_ast with
  | Ok () -> fail "Expected the term to be rejected, but it was accepted"
  | Error msg ->
    if not (contains_substring msg expected) then
      failf "Expected a rejection containing %S, but got %S" expected msg

let check_dt6_valid = expect_valid "test_dt6" (dt6_term (dt6_children "01" "01" "01"))
let check_dt6_wrong_value = expect_invalid "test_dt6" (dt6_term (dt6_children "01" "10" "01"))
let check_dt6_missing_child =
  expect_invalid "test_dt6" (dt6_term [leaf_node "A" (bv "01"); leaf_node "B" (bv "01")])
let check_dt6_wrong_start = expect_invalid "test_dt6" (leaf_node "A" (bv "01"))
let check_dt6_bare_value = expect_invalid "test_dt6" (bv "01")
let check_dt6_infeasible = expect_invalid "test_dt6" SolverAst.Infeasible

(* Occurrence indices are part of the printed form, and were once written to
   stdout rather than the supplied formatter *)
let print_occurrence_indices () =
  let printed = Format.asprintf "%a" SolverAst.pp_print_solver_ast
    (dt6_term (dt6_children "01" "10" "11")) in
  check string "printed term" "(S@{0}[0] (A 0b01) (B 0b10) (C 0b11))\n" printed

(* Phase 3: gaps where the checker used to return Valid without looking
   (doc/todo-evaluator.md, section C) *)

let indexed_node name occurrence value =
  SolverAst.Node ((Nt.User name, Some 0, Some occurrence), [value])

let int_leaf i = SolverAst.Leaf (Value.Int i)

(* An unindexed reference to a repeated nonterminal is implicitly universally
   quantified, so a violation at the second occurrence must be caught *)
let repeated_nt_second_violates = expect_invalid "repeated_nt.gbl"
  (SolverAst.Node ((Nt.User "S", Some 0, Some 0),
    [indexed_node "N" 0 (int_leaf 9); indexed_node "N" 1 (int_leaf 1)]))

let repeated_nt_both_satisfy = expect_valid "repeated_nt.gbl"
  (SolverAst.Node ((Nt.User "S", Some 0, Some 0),
    [indexed_node "N" 0 (int_leaf 9); indexed_node "N" 1 (int_leaf 7)]))

(* <Flags> is declared BitVec(8) *)
let width_correct = expect_valid "width.gbl"
  (SolverAst.Node ((Nt.User "S", Some 0, Some 0), [leaf_node "Flags" (bv "10101010")]))

let width_too_narrow = expect_rejected_with "width.gbl"
  (SolverAst.Node ((Nt.User "S", Some 0, Some 0), [leaf_node "Flags" (bv "01")]))
  "has type BitVec(2), but the grammar declares BitVec(8)"

let width_wrong_type = expect_invalid "width.gbl"
  (SolverAst.Node ((Nt.User "S", Some 0, Some 0), [leaf_node "Flags" (int_leaf 5)]))

(* A bit vector's width field and its bit list can disagree. Only the bits are
   serialized, so the field alone must not decide whether the declared width is met. *)
let desynced_bv width bits =
  SolverAst.Leaf (Value.BitVector (width, List.init bits (fun _ -> true)))

let width_field_overstates = expect_rejected_with "width.gbl"
  (SolverAst.Node ((Nt.User "S", Some 0, Some 0), [leaf_node "Flags" (desynced_bv 8 2)]))
  "declares width 8 but holds 2 bits"

let width_field_understates = expect_rejected_with "width.gbl"
  (SolverAst.Node ((Nt.User "S", Some 0, Some 0), [leaf_node "Flags" (desynced_bv 8 70)]))
  "declares width 8 but holds 70 bits"

(* A constraint naming a category the chosen option does not produce is trivially
   satisfied. <E> is <B>'s second option, so its node carries that index. *)
let vacuous_constraint_holds = expect_valid "vacuous.gbl"
  (SolverAst.Node ((Nt.User "A", Some 0, Some 0),
    [SolverAst.Node ((Nt.User "B", Some 0, Some 0),
      [SolverAst.Node ((Nt.User "E", Some 1, Some 0), [int_leaf (-92)])]);
     leaf_node "C" (int_leaf (-97))]))

(* A finished term is closed. The message is pinned because the declared-type check
   also rejects these, so asserting only on rejection would not reach check_closed. *)
let unclosed_term residue =
  SolverAst.Node ((Nt.User "S", Some 0, Some 0),
    [SolverAst.Node ((Nt.User "Flags", None, None), [residue])])

let closedness_stub =
  expect_rejected_with "width.gbl"
    (unclosed_term (SolverAst.StubLeaf (Nt.fresh_stub (Nt.User "Flags"))))
    "Term contains the uncomputed stub"

let closedness_model =
  expect_rejected_with "width.gbl" (unclosed_term (SolverAst.Model []))
    "Term contains an SMT model"

let closedness_infeasible =
  expect_rejected_with "width.gbl" (unclosed_term SolverAst.Infeasible)
    "Term contains an infeasibility marker"

(* A placeholder is not a closedness defect, since Placeholder is a type whose
   values are placeholders; a misplaced one is caught by the declared type *)
let placeholder_where_bitvec_declared = expect_invalid "width.gbl"
  (SolverAst.Node ((Nt.User "S", Some 0, Some 0),
    [leaf_node "Flags" (SolverAst.Leaf (Value.Placeholder "x"))]))

(* The engine always emits a node per nonterminal. A term that elides that level
   would skip the child's declared type, so it is rejected. *)
let elided_node_level = expect_invalid "width.gbl"
  (SolverAst.Node ((Nt.User "S", Some 0, Some 0), [bv "10101010"]))

(* EliminateTaConstraints inlines a refinement into the rules that use it, so these
   pin the inlined form, in both directions *)
let refinement_satisfied = expect_valid "refinement.gbl"
  (SolverAst.Node ((Nt.User "S", Some 0, Some 0), [leaf_node "G" (int_leaf (-5))]))

let refinement_violated = expect_invalid "refinement.gbl"
  (SolverAst.Node ((Nt.User "S", Some 0, Some 0), [leaf_node "G" (int_leaf 5)]))

(* A refinement left on its annotation resolves at the annotated node, and would pass
   vacuously as Top if that failed. Parsed raw, since the front end inlines it away. *)
let check_parsed_source source solver_ast =
  CheckSolverAst.check_solver_ast (Parsing.parse source) solver_ast

let check_parsed_term name solver_ast =
  check_parsed_source (Utils.read_file ("../../../test/test_cases/" ^ name)) solver_ast

let annotation_refinement_satisfied () =
  match check_parsed_term "refinement.gbl"
    (SolverAst.Node ((Nt.User "S", None, None), [leaf_node "G" (int_leaf (-5))])) with
  | Ok () -> ()
  | Error msg -> fail msg

let annotation_refinement_violated () =
  match check_parsed_term "refinement.gbl"
    (SolverAst.Node ((Nt.User "S", None, None), [leaf_node "G" (int_leaf 5)])) with
  | Ok () -> fail "Expected the term to be rejected, but it was accepted"
  | Error _ -> ()

(* Only a path's first step may name the annotated node, and only when no occurrence
   index disambiguates it; any other step denotes Top, so these hold for any <G> *)
let annotation_step_is_top source () =
  match check_parsed_source source
    (SolverAst.Node ((Nt.User "S", None, None), [leaf_node "G" (int_leaf 5)])) with
  | Ok () -> ()
  | Error msg -> fail msg

let annotation_later_steps_are_top =
  annotation_step_is_top "<S> ::= <G>; <G> :: Int { <G>.<G>.<G> < 0; };"

let annotation_indexed_step_is_top =
  annotation_step_is_top "<S> ::= <G>; <G> :: Int { <G>[3] < 0; };"

(* An annotation is a single occurrence of a single option, so index 0 of either kind
   names it and the refinement is enforced; only an out-of-range index denotes Top *)
let annotation_index_case source value expected () =
  let term = SolverAst.Node ((Nt.User "S", None, None), [leaf_node "G" (int_leaf value)]) in
  match check_parsed_source source term, expected with
  | Ok (), true | Error _, false -> ()
  | Ok (), false -> fail "Expected the term to be rejected, but it was accepted"
  | Error msg, true -> fail msg

let annotation_zero_index_satisfied =
  annotation_index_case "<S> ::= <G>; <G> :: Int { <G>[0] < 0; };" (-5) true

let annotation_zero_index_violated =
  annotation_index_case "<S> ::= <G>; <G> :: Int { <G>[0] < 0; };" 5 false

let annotation_zero_option_violated =
  annotation_index_case "<S> ::= <G>; <G> :: Int { <G>@{0} < 0; };" 5 false

let annotation_out_of_range_option_is_top =
  annotation_index_case "<S> ::= <G>; <G> :: Int { <G>@{7} < 0; };" 5 true

let annotation_out_of_range_option_with_occurrence_is_top =
  annotation_index_case "<S> ::= <G>; <G> :: Int { <G>@{1}[0] < 0; };" 5 true

(* A conversion wider than a native int cannot be reproduced, and the evaluator says
   so rather than wrapping: the verdict must be Unknown, not a guess either way *)
let wide_bv_conversion_is_undecided () =
  let bits = String.concat "" (List.init 64 (fun _ -> "1")) in
  let term = SolverAst.Node ((Nt.User "S", Some 0, Some 0),
    [leaf_node "BV" (bv bits); leaf_node "N" (int_leaf 0)]) in
  match verdict_of_term "wide-bv.gbl" term with
  | CheckSolverAst.Unknown _ -> ()
  | CheckSolverAst.Valid -> fail "A 64-bit conversion was decided, so it wrapped silently"
  | CheckSolverAst.Violated msg -> failf "Expected Unknown, but the term was rejected: %s" msg

(* A call site passing more arguments than the callee declares must come back as a
   verdict; List.nth on the parameter list used to escape as Failure "nth" *)
let call_site_arity_is_a_verdict () =
  match check_parsed_source "<S> ::= <L>(1, 2); <L>(v :: Int) ::= <E> { <E> = v; }; <E> :: Int;"
    (SolverAst.Node ((Nt.User "S", None, None),
      [SolverAst.Node ((Nt.User "L", None, None), [leaf_node "E" (int_leaf 1)])])) with
  | Ok () -> fail "Expected the term to be rejected, but it was accepted"
  | Error _ -> ()
  | exception Failure msg -> failf "check_solver_ast raised Failure %S instead of returning" msg

(* length() counts 8 bits per character of a String, which the SAE spec relies on *)
let length_string_term chars bits =
  SolverAst.Node ((Nt.User "S", Some 0, Some 0),
    [leaf_node "W" (SolverAst.Leaf (Value.String chars)); leaf_node "L" (int_leaf bits)])

let length_string_counts_bytes = expect_valid "length-string.gbl" (length_string_term "ab" 16)
let length_string_not_characters =
  expect_invalid "length-string.gbl" (length_string_term "ab" 2)

(* A value's type follows the bits it holds, not its width field *)
let value_ty_follows_bit_list () =
  check bool "desynced bit vector reports the width of its bit list" true
    (Value.ty (Value.BitVector (8, [true; false])) = Ast.BitVector 2)

(* Changes the first attribute node holding an integer, to show that attribute
   values are really evaluated rather than skipped *)
let rec corrupt_first_attribute solver_ast = match solver_ast with
| SolverAst.Node ((label, i, j), [SolverAst.Leaf (Value.Int n)]) when Nt.is_attribute label ->
  Some (SolverAst.Node ((label, i, j), [SolverAst.Leaf (Value.Int (n + 1))]))
| SolverAst.Node (label, children) ->
  let rec first prefix = function
  | [] -> None
  | child :: rest ->
    match corrupt_first_attribute child with
    | Some child -> Some (SolverAst.Node (label, List.rev prefix @ (child :: rest)))
    | None -> first (child :: prefix) rest
  in
  first [] children
| SolverAst.Leaf _ | SolverAst.StubLeaf _ | SolverAst.Model _ | SolverAst.Infeasible -> None

let attribute_values_are_checked name () =
  let input = "../../../test/test_cases/" ^ name in
  let solver_ast, _, ast = main_pipeline input in
  match CheckSolverAst.check_solver_ast ast solver_ast with
  | Error msg -> fail ("The generated term should be valid: " ^ msg)
  | Ok () ->
    match corrupt_first_attribute solver_ast with
    | None -> fail "No attribute node found to corrupt"
    | Some corrupted ->
      match CheckSolverAst.check_solver_ast ast corrupted with
      | Ok () -> fail "A wrong attribute value was accepted"
      | Error _ -> ()

(* Negative corpus: a suite of "the generated term is valid" assertions is satisfied
   by a checker that accepts everything, so mutants are what give the oracle value *)

(* Applies a rewrite at the first node where it succeeds, bottom of the term last *)
let rec first_rewrite rewrite solver_ast =
  match rewrite solver_ast with
  | Some rewritten -> Some rewritten
  | None ->
    match solver_ast with
    | SolverAst.Node (label, children) ->
      let rec go prefix = function
      | [] -> None
      | child :: rest ->
        match first_rewrite rewrite child with
        | Some child -> Some (SolverAst.Node (label, List.rev prefix @ (child :: rest)))
        | None -> go (child :: prefix) rest
      in
      go [] children
    | SolverAst.Leaf _ | SolverAst.StubLeaf _ | SolverAst.Model _ | SolverAst.Infeasible -> None

(* Each of these violates a declared type or the term's structure, so no choice of
   production rule can make the result valid *)

let narrow_bitvector = first_rewrite (function
  | SolverAst.Leaf (Value.BitVector (width, _ :: bits)) when width > 1 ->
    Some (SolverAst.Leaf (Value.BitVector (width - 1, bits)))
  | SolverAst.Node _ | SolverAst.Leaf _ | SolverAst.StubLeaf _
  | SolverAst.Model _ | SolverAst.Infeasible -> None)

let retype_leaf = first_rewrite (function
  | SolverAst.Leaf (Value.Int _) -> Some (SolverAst.Leaf (Value.Bool true))
  | SolverAst.Node _ | SolverAst.Leaf _ | SolverAst.StubLeaf _
  | SolverAst.Model _ | SolverAst.Infeasible -> None)

let insert_stub = first_rewrite (function
  | SolverAst.Leaf _ -> Some (SolverAst.StubLeaf (Nt.fresh_stub (Nt.User "mutant")))
  | SolverAst.Node _ | SolverAst.StubLeaf _ | SolverAst.Model _ | SolverAst.Infeasible -> None)

(* Drops the node level for a symbolic terminal, which the grammar requires *)
let unwrap_node = first_rewrite (function
  | SolverAst.Node ((label, _, _), [SolverAst.Leaf _ as leaf]) when not (Nt.is_attribute label) ->
    Some leaf
  | SolverAst.Node _ | SolverAst.Leaf _ | SolverAst.StubLeaf _
  | SolverAst.Model _ | SolverAst.Infeasible -> None)

(* Grammar-dependent: another production rule option may legitimately match the
   result, or the mutated leaf may be unconstrained *)
let drop_child = first_rewrite (function
  | SolverAst.Node (label, (_ :: _ :: _ as children)) ->
    Some (SolverAst.Node (label, List.tl children))
  | SolverAst.Node _ | SolverAst.Leaf _ | SolverAst.StubLeaf _
  | SolverAst.Model _ | SolverAst.Infeasible -> None)

let bump_int = first_rewrite (function
  | SolverAst.Leaf (Value.Int i) -> Some (SolverAst.Leaf (Value.Int (i + 1)))
  | SolverAst.Node _ | SolverAst.Leaf _ | SolverAst.StubLeaf _
  | SolverAst.Model _ | SolverAst.Infeasible -> None)

(* Terms are generated randomly, so the corpus is pinned to one seed to keep the
   assertions deterministic. Varying it samples further mutants. *)
let with_fixed_seed f =
  let previous = !Flags.seed in
  Flags.seed := Some 42;
  Fun.protect ~finally:(fun () -> Flags.seed := previous) f

let grammar_files () =
  Sys.readdir "../../../test/test_cases"
  |> Array.to_list
  |> List.sort String.compare

(* Runs one mutation operator over every grammar that produces a term *)
let mutation_catch_rate name mutate = with_fixed_seed @@ fun () ->
  let applied = ref 0 and caught = ref 0 in
  List.iter (fun file ->
    match main_pipeline ("../../../test/test_cases/" ^ file) with
    | exception _ -> ()
    | solver_ast, _, ast ->
      match solver_ast, CheckSolverAst.check_solver_ast ast solver_ast with
      | SolverAst.Node _, Ok () -> (
        match mutate solver_ast with
        | None -> ()
        | Some mutant ->
          incr applied;
          match CheckSolverAst.check_solver_ast ast mutant with
          | Error _ -> incr caught
          | Ok () -> Format.printf "[mutation]   %s accepted by %s@." name file
      )
      | _, _ -> ()
  ) (grammar_files ());
  Format.printf "[mutation] %s: %d of %d caught@." name !caught !applied

(* Not an assertion: these operators can legitimately yield another valid term, so
   the number is a measurement of the oracle, not a pass or fail *)
let measure_grammar_dependent_operators () =
  mutation_catch_rate "drop_child" drop_child;
  mutation_catch_rate "bump_int" bump_int

let mutation_operator_is_caught name mutate () = with_fixed_seed @@ fun () ->
  let applied = ref 0 in
  let accepted = ref [] in
  List.iter (fun file ->
    match main_pipeline ("../../../test/test_cases/" ^ file) with
    | exception _ -> ()
    | solver_ast, _, ast ->
      match solver_ast with
      | SolverAst.Node _ -> (
        match CheckSolverAst.check_solver_ast ast solver_ast with
        (* Only grammars whose own term checks out can say anything about a mutant *)
        | Error _ -> ()
        | Ok () ->
          match mutate solver_ast with
          | None -> ()
          | Some mutant ->
            incr applied;
            match CheckSolverAst.check_solver_ast ast mutant with
            | Error _ -> ()
            | Ok () -> accepted := file :: !accepted
      )
      | SolverAst.Leaf _ | SolverAst.StubLeaf _ | SolverAst.Model _ | SolverAst.Infeasible -> ()
  ) (grammar_files ());
  Format.printf "[mutation] %s: applied to %d grammars, %d accepted@."
    name !applied (List.length !accepted);
  if !accepted <> [] then
    fail (Format.asprintf "%s: mutant accepted for %s (applied to %d grammars)"
      name (String.concat ", " !accepted) !applied)
  else if !applied = 0 then
    fail (name ^ ": no grammar could be mutated, so nothing was tested")

(* Generates a term and checks it against its grammar *)
let verdict_for name =
  let input = "../../../test/test_cases/" ^ name in
  let solver_ast, _, ast = main_pipeline input in
  CheckSolverAst.verdict_of ast solver_ast

(* Every production rule option must define the same synthesized attributes, but
   the order they are written in is not part of that *)
let attr_order_accepted () =
  match verdict_for "attr-order.gbl" with
  | CheckSolverAst.Valid -> ()
  | CheckSolverAst.Violated msg | CheckSolverAst.Unknown msg -> fail msg

let attr_missing_rejected () =
  expect_error "../../../test/test_cases/attr-missing.gbl"
    "defines synthesized attribute(s) b in some production rule options but not others"

(* Since every option defines every attribute, a missing attribute node is a defect
   rather than an absent category, so it must not pass as trivially satisfied *)
let drop_attribute_node = first_rewrite (function
  | SolverAst.Node (label, children)
    when List.exists (function
      | SolverAst.Node ((child, _, _), _) -> Nt.is_attribute child
      | SolverAst.Leaf _ | SolverAst.StubLeaf _ | SolverAst.Model _
      | SolverAst.Infeasible -> false) children ->
    Some (SolverAst.Node (label, List.filter (function
      | SolverAst.Node ((child, _, _), _) -> not (Nt.is_attribute child)
      | SolverAst.Leaf _ | SolverAst.StubLeaf _ | SolverAst.Model _
      | SolverAst.Infeasible -> true) children))
  | SolverAst.Node _ | SolverAst.Leaf _ | SolverAst.StubLeaf _
  | SolverAst.Model _ | SolverAst.Infeasible -> None)

let missing_attribute_node_rejected name () =
  let input = "../../../test/test_cases/" ^ name in
  let solver_ast, _, ast = main_pipeline input in
  match drop_attribute_node solver_ast with
  | None -> fail "No attribute node found to drop"
  | Some dropped ->
    match CheckSolverAst.check_solver_ast ast dropped with
    | Ok () -> fail "A term missing an attribute node was accepted"
    | Error _ -> ()

(* SMT int_to_bv wraps, so the evaluator must too, or a valid term is rejected *)
let bv_overflow_wraps () =
  match verdict_for "bv-overflow.gbl" with
  | CheckSolverAst.Valid -> ()
  | CheckSolverAst.Violated msg | CheckSolverAst.Unknown msg -> fail msg

let sets_are_evaluated () =
  match verdict_for "sets.gbl" with
  | CheckSolverAst.Valid -> ()
  | CheckSolverAst.Violated msg | CheckSolverAst.Unknown msg -> fail msg

let regex_satisfied () =
  match verdict_for "regex.gbl" with
  | CheckSolverAst.Valid -> ()
  | CheckSolverAst.Violated msg | CheckSolverAst.Unknown msg -> fail msg

let str_leaf str = SolverAst.Leaf (Value.String str)

let word name str =
  SolverAst.Node ((Nt.User "S", Some 0, Some 0), [leaf_node name (str_leaf str)])

(* str.in_re is whole-string membership, not search: "xaby" contains "ab" but is
   not in its language *)
let regex_matches = expect_valid "regex.gbl" (word "Word" "ab")
let regex_rejects_other = expect_invalid "regex.gbl" (word "Word" "xy")
let regex_is_anchored = expect_invalid "regex.gbl" (word "Word" "xaby")

(* re.++(re.range("a","c"), re.*(str.to_re("z"))) *)
let regex_concat_star_range = expect_valid "regex2.gbl" (word "W" "bzz")
let regex_star_zero = expect_valid "regex2.gbl" (word "W" "a")
let regex_outside_range = expect_invalid "regex2.gbl" (word "W" "dzz")
let regex_empty_string = expect_invalid "regex2.gbl" (word "W" "")

(* re.range denotes the empty language unless both arguments are single characters,
   so only the other branch of the union can be matched *)
let regex_union_branch = expect_valid "regex3.gbl" (word "W" "ok")
let regex_bad_range_is_empty = expect_invalid "regex3.gbl" (word "W" "b")

(* Two options of the same shape differ only in their constraints, so the option
   index each child carries decides; structural matching would always pick the first *)
let option_node option value =
  SolverAst.Node ((Nt.User "N", Some option, Some 0), [value])

let rhs_options_term option value =
  SolverAst.Node ((Nt.User "S", Some 0, Some 0), [option_node option (int_leaf value)])

let rhs_option_0_satisfied = expect_valid "rhs-options.gbl" (rhs_options_term 0 6)
let rhs_option_1_satisfied = expect_valid "rhs-options.gbl" (rhs_options_term 1 (-1))
let rhs_option_0_violated = expect_invalid "rhs-options.gbl" (rhs_options_term 0 (-1))
let rhs_option_1_violated = expect_invalid "rhs-options.gbl" (rhs_options_term 1 6)

(* An unstamped term does not name an option, so it is an instance of the grammar
   if any option of the right shape accepts it — but still only if one does *)
let unstamped_term value =
  SolverAst.Node ((Nt.User "S", None, None), [leaf_node "N" (int_leaf value)])

let rhs_unstamped_option_0 = expect_valid "rhs-options.gbl" (unstamped_term 6)
let rhs_unstamped_option_1 = expect_valid "rhs-options.gbl" (unstamped_term (-1))
let rhs_unstamped_neither = expect_invalid "rhs-options.gbl" (unstamped_term 3)

(* The engine picks an option depending on the seed, so its own output must check
   out whichever one it picked *)
let generated_term_round_trips name () =
  let previous = !Flags.seed in
  Fun.protect ~finally:(fun () -> Flags.seed := previous) @@ fun () ->
  List.iter (fun seed ->
    Flags.seed := Some seed;
    let solver_ast, _, ast = main_pipeline ("../../../test/test_cases/" ^ name) in
    match CheckSolverAst.check_solver_ast ast solver_ast with
    | Ok () -> ()
    | Error msg -> failf "%s seed %d: %s" name seed msg
  ) (List.init 12 (fun i -> i + 1))

let rhs_options_generated = generated_term_round_trips "rhs-options.gbl"

(* AbstractDeps drops the indices from a derived field's grammar element, so a rule
   whose children are all derived fields yields a term with nothing stamped *)
let derived_field_options_generated = generated_term_round_trips "d1.gbl"

(* Trying every option must stay fail-closed: an option the evaluator cannot decide
   (here division by zero, which SMT-LIB leaves uninterpreted) is not an acceptance *)
let unknown_option_term a b =
  SolverAst.Node ((Nt.User "S", None, None),
    [leaf_node "A" (int_leaf a); leaf_node "B" (int_leaf b)])

let undecided_option_is_not_a_pass =
  expect_rejected_with "unknown-option.gbl" (unknown_option_term 5 0) "Could not be checked"

let undecided_option_does_not_mask_a_valid_one =
  expect_valid "unknown-option.gbl" (unknown_option_term 200 0)

(* <B>.<B>.<N> at a <B> on the non-recursive option names an absent category, so it
   denotes Top; resolving by the current node's name would alias it to this <B>'s <N> *)
let selfref_flat_term n =
  SolverAst.Node ((Nt.User "S", Some 0, Some 0),
    [SolverAst.Node ((Nt.User "B", Some 0, Some 0),
      [SolverAst.Node ((Nt.User "N", Some 1, Some 0), [int_leaf n])])])

let selfref_flat_positive = expect_valid "selfref.gbl" (selfref_flat_term 5)
let selfref_flat_negative = expect_valid "selfref.gbl" (selfref_flat_term (-5))
let selfref_generated = generated_term_round_trips "selfref.gbl"

let rec bump_ints solver_ast = match solver_ast with
| SolverAst.Leaf (Value.Int i) -> SolverAst.Leaf (Value.Int (i + 1))
| SolverAst.Node (label, children) -> SolverAst.Node (label, List.map bump_ints children)
| SolverAst.Leaf _ | SolverAst.StubLeaf _ | SolverAst.Model _ | SolverAst.Infeasible ->
  solver_ast

(* Bumps every integer in the root's occurrence-th <name> child. Whole-subtree, so
   that the child's own constraints still hold and only the call site can break. *)
let bump_nth_child name occurrence solver_ast = match solver_ast with
| SolverAst.Node (label, children) ->
  let seen = ref (-1) in
  SolverAst.Node (label, List.map (fun child -> match child with
  | SolverAst.Node ((Nt.User child_name, _, _), _) when String.equal child_name name ->
    incr seen;
    if !seen = occurrence then bump_ints child else child
  | SolverAst.Node _ | SolverAst.Leaf _ | SolverAst.StubLeaf _
  | SolverAst.Model _ | SolverAst.Infeasible -> child
  ) children)
| SolverAst.Leaf _ | SolverAst.StubLeaf _ | SolverAst.Model _ | SolverAst.Infeasible ->
  solver_ast

(* An unindexed reference in a call-site argument is universally quantified, so the
   argument constrains every occurrence rather than only the first *)
let inherited_arg_is_universal name child () = with_fixed_seed @@ fun () ->
  let solver_ast, _, ast = main_pipeline ("../../../test/test_cases/" ^ name) in
  (match CheckSolverAst.check_solver_ast ast solver_ast with
  | Ok () -> ()
  | Error msg -> fail ("The generated term should be valid: " ^ msg));
  List.iter (fun occurrence ->
    match CheckSolverAst.check_solver_ast ast (bump_nth_child child occurrence solver_ast) with
    | Error _ -> ()
    | Ok () ->
      failf "Corrupting occurrence %d was accepted, so the argument was checked at only one"
        occurrence
  ) [0; 1]

(* An attribute reference carries occurrence indices, so <P>[1].len must denote the
   second <P>'s attribute rather than the first's *)
let indexed_attribute_reference () =
  let solver_ast, _, ast = main_pipeline "../../../test/test_cases/attr-index.gbl" in
  (match CheckSolverAst.check_solver_ast ast solver_ast with
  | Ok () -> ()
  | Error msg -> fail msg);
  let rec find_l solver_ast = match solver_ast with
  | SolverAst.Node ((Nt.User "L", _, _), [SolverAst.Leaf (Value.Int i)]) -> Some i
  | SolverAst.Node (_, children) -> List.find_map find_l children
  | SolverAst.Leaf _ | SolverAst.StubLeaf _ | SolverAst.Model _ | SolverAst.Infeasible -> None
  in
  match find_l solver_ast with
  | Some 5 -> ()
  | Some i -> failf "<L> is %d, so <P>[1].len resolved to the wrong occurrence" i
  | None -> fail "No <L> node in the generated term"

let () =
  run "My_module" [


    "check_dt6_valid", [test_case "check_dt6_valid" `Quick check_dt6_valid];
    "check_dt6_wrong_value", [test_case "check_dt6_wrong_value" `Quick check_dt6_wrong_value];
    "check_dt6_missing_child", [test_case "check_dt6_missing_child" `Quick check_dt6_missing_child];
    "check_dt6_wrong_start", [test_case "check_dt6_wrong_start" `Quick check_dt6_wrong_start];
    "repeated_nt_second_violates", [test_case "repeated_nt_second_violates" `Quick repeated_nt_second_violates];
    "repeated_nt_both_satisfy", [test_case "repeated_nt_both_satisfy" `Quick repeated_nt_both_satisfy];
    "width_correct", [test_case "width_correct" `Quick width_correct];
    "width_too_narrow", [test_case "width_too_narrow" `Quick width_too_narrow];
    "width_wrong_type", [test_case "width_wrong_type" `Quick width_wrong_type];
    "width_field_overstates", [test_case "width_field_overstates" `Quick width_field_overstates];
    "width_field_understates", [test_case "width_field_understates" `Quick width_field_understates];
    "vacuous_constraint_holds", [test_case "vacuous_constraint_holds" `Quick vacuous_constraint_holds];
    "closedness_stub", [test_case "closedness_stub" `Quick closedness_stub];
    "closedness_model", [test_case "closedness_model" `Quick closedness_model];
    "closedness_infeasible", [test_case "closedness_infeasible" `Quick closedness_infeasible];
    "annotation_refinement_satisfied", [test_case "annotation_refinement_satisfied" `Quick annotation_refinement_satisfied];
    "annotation_refinement_violated", [test_case "annotation_refinement_violated" `Quick annotation_refinement_violated];
    "annotation_later_steps_are_top", [test_case "annotation_later_steps_are_top" `Quick annotation_later_steps_are_top];
    "annotation_indexed_step_is_top", [test_case "annotation_indexed_step_is_top" `Quick annotation_indexed_step_is_top];
    "annotation_zero_index_satisfied", [test_case "annotation_zero_index_satisfied" `Quick annotation_zero_index_satisfied];
    "annotation_zero_index_violated", [test_case "annotation_zero_index_violated" `Quick annotation_zero_index_violated];
    "annotation_zero_option_violated", [test_case "annotation_zero_option_violated" `Quick annotation_zero_option_violated];
    "annotation_out_of_range_option_is_top", [test_case "annotation_out_of_range_option_is_top" `Quick annotation_out_of_range_option_is_top];
    "annotation_out_of_range_option_with_occurrence_is_top", [test_case "annotation_out_of_range_option_with_occurrence_is_top" `Quick annotation_out_of_range_option_with_occurrence_is_top];
    "wide_bv_conversion_is_undecided", [test_case "wide_bv_conversion_is_undecided" `Quick wide_bv_conversion_is_undecided];
    "call_site_arity_is_a_verdict", [test_case "call_site_arity_is_a_verdict" `Quick call_site_arity_is_a_verdict];
    "length_string_counts_bytes", [test_case "length_string_counts_bytes" `Quick length_string_counts_bytes];
    "length_string_not_characters", [test_case "length_string_not_characters" `Quick length_string_not_characters];
    "value_ty_follows_bit_list", [test_case "value_ty_follows_bit_list" `Quick value_ty_follows_bit_list];
    "placeholder_where_bitvec_declared", [test_case "placeholder_where_bitvec_declared" `Quick placeholder_where_bitvec_declared];
    "inh_attr_values_checked", [test_case "inh_attr_values_checked" `Quick (attribute_values_are_checked "inh-attr.gbl")];
    "length_attr_values_checked", [test_case "length_attr_values_checked" `Quick (attribute_values_are_checked "length-attr.gbl")];
    "bug5_values_checked", [test_case "bug5_values_checked" `Quick (attribute_values_are_checked "bug5.gbl")];
    "mutate_narrow_bitvector", [test_case "mutate_narrow_bitvector" `Quick (mutation_operator_is_caught "narrow_bitvector" narrow_bitvector)];
    "mutate_retype_leaf", [test_case "mutate_retype_leaf" `Quick (mutation_operator_is_caught "retype_leaf" retype_leaf)];
    "mutate_insert_stub", [test_case "mutate_insert_stub" `Quick (mutation_operator_is_caught "insert_stub" insert_stub)];
    "mutate_unwrap_node", [test_case "mutate_unwrap_node" `Quick (mutation_operator_is_caught "unwrap_node" unwrap_node)];
    "measure_grammar_dependent_operators", [test_case "measure_grammar_dependent_operators" `Quick measure_grammar_dependent_operators];
    "elided_node_level", [test_case "elided_node_level" `Quick elided_node_level];
    "refinement_satisfied", [test_case "refinement_satisfied" `Quick refinement_satisfied];
    "refinement_violated", [test_case "refinement_violated" `Quick refinement_violated];
    "attr_order_accepted", [test_case "attr_order_accepted" `Quick attr_order_accepted];
    "attr_missing_rejected", [test_case "attr_missing_rejected" `Quick attr_missing_rejected];
    "inh_attr_node_required", [test_case "inh_attr_node_required" `Quick (missing_attribute_node_rejected "inh-attr.gbl")];
    "length_attr_node_required", [test_case "length_attr_node_required" `Quick (missing_attribute_node_rejected "length-attr.gbl")];
    "inh_attr_unused_node_required", [test_case "inh_attr_unused_node_required" `Quick (missing_attribute_node_rejected "inh-attr-unused.gbl")];
    "bv_overflow_wraps", [test_case "bv_overflow_wraps" `Quick bv_overflow_wraps];
    "sets_are_evaluated", [test_case "sets_are_evaluated" `Quick sets_are_evaluated];
    "regex_satisfied", [test_case "regex_satisfied" `Quick regex_satisfied];
    "regex_matches", [test_case "regex_matches" `Quick regex_matches];
    "regex_rejects_other", [test_case "regex_rejects_other" `Quick regex_rejects_other];
    "regex_is_anchored", [test_case "regex_is_anchored" `Quick regex_is_anchored];
    "regex_concat_star_range", [test_case "regex_concat_star_range" `Quick regex_concat_star_range];
    "regex_star_zero", [test_case "regex_star_zero" `Quick regex_star_zero];
    "regex_outside_range", [test_case "regex_outside_range" `Quick regex_outside_range];
    "regex_empty_string", [test_case "regex_empty_string" `Quick regex_empty_string];
    "regex_union_branch", [test_case "regex_union_branch" `Quick regex_union_branch];
    "regex_bad_range_is_empty", [test_case "regex_bad_range_is_empty" `Quick regex_bad_range_is_empty];
    "rhs_option_0_satisfied", [test_case "rhs_option_0_satisfied" `Quick rhs_option_0_satisfied];
    "rhs_option_1_satisfied", [test_case "rhs_option_1_satisfied" `Quick rhs_option_1_satisfied];
    "rhs_option_0_violated", [test_case "rhs_option_0_violated" `Quick rhs_option_0_violated];
    "rhs_option_1_violated", [test_case "rhs_option_1_violated" `Quick rhs_option_1_violated];
    "rhs_unstamped_option_0", [test_case "rhs_unstamped_option_0" `Quick rhs_unstamped_option_0];
    "rhs_unstamped_option_1", [test_case "rhs_unstamped_option_1" `Quick rhs_unstamped_option_1];
    "rhs_unstamped_neither", [test_case "rhs_unstamped_neither" `Quick rhs_unstamped_neither];
    "rhs_options_generated", [test_case "rhs_options_generated" `Quick rhs_options_generated];
    "derived_field_options_generated", [test_case "derived_field_options_generated" `Quick derived_field_options_generated];
    "undecided_option_is_not_a_pass", [test_case "undecided_option_is_not_a_pass" `Quick undecided_option_is_not_a_pass];
    "undecided_option_does_not_mask_a_valid_one", [test_case "undecided_option_does_not_mask_a_valid_one" `Quick undecided_option_does_not_mask_a_valid_one];
    "selfref_flat_positive", [test_case "selfref_flat_positive" `Quick selfref_flat_positive];
    "selfref_flat_negative", [test_case "selfref_flat_negative" `Quick selfref_flat_negative];
    "selfref_generated", [test_case "selfref_generated" `Quick selfref_generated];
    "inherited_arg_is_universal", [test_case "inherited_arg_is_universal" `Quick (inherited_arg_is_universal "inh-attr-index.gbl" "N")];
    "inherited_arg_path_is_universal", [test_case "inherited_arg_path_is_universal" `Quick (inherited_arg_is_universal "inh-attr-index-path.gbl" "P")];
    "inherited_arg_attr_is_universal", [test_case "inherited_arg_attr_is_universal" `Quick (inherited_arg_is_universal "inh-attr-index-attr.gbl" "P")];
    "indexed_attribute_reference", [test_case "indexed_attribute_reference" `Quick indexed_attribute_reference];
    "print_occurrence_indices", [test_case "print_occurrence_indices" `Quick print_occurrence_indices];
    "check_dt6_infeasible", [test_case "check_dt6_infeasible" `Quick check_dt6_infeasible];
    "check_dt6_bare_value", [test_case "check_dt6_bare_value" `Quick check_dt6_bare_value];

    "test_check_solver_ast", [test_case "test_check_solver_ast" `Quick test_check_solver_ast];
    "test_check_solver_ast_2", [test_case "test_check_solver_ast_2" `Quick test_check_solver_ast_2];
    "test_check_solver_ast_3", [test_case "test_check_solver_ast_3" `Quick test_check_solver_ast_3];
    "test_check_solver_ast_4", [test_case "test_check_solver_ast_4" `Quick test_check_solver_ast_4];
    "test_check_solver_ast_5", [test_case "test_check_solver_ast_5" `Quick test_check_solver_ast_5];

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

    *)

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
    "dd_test12", [test_case "test12" `Quick dd_test12]; *)

    "dm_test17", [test_case "test17" `Quick dm_test17];
    (*"dd_test17", [test_case "test17" `Quick dd_test17];*)
    "dm_test16", [test_case "test16" `Quick dm_test16];
    "dm_test18", [test_case "test18" `Quick dm_test18];
    (*"dd_test18", [test_case "test18" `Quick dd_test18];*)

    "test_check_solver_ast_6", [test_case "test_check_solver_ast_6" `Quick test_check_solver_ast_6];
    "test_check_solver_ast_7", [test_case "test_check_solver_ast_7" `Quick test_check_solver_ast_7]; 

    "dm_test_another_ambiguous_reference_1", [test_case "dm_test_another_ambiguous_reference_1" `Quick dm_test_another_ambiguous_reference_1]; 
    (*"dd_test_another_ambiguous_reference_1", [test_case "dd_test_another_ambiguous_reference_1" `Quick dd_test_another_ambiguous_reference_1]; *)
    "dm_test_another_ambiguous_reference_2", [test_case "dm_test_another_ambiguous_reference_2" `Quick dm_test_another_ambiguous_reference_2]; 
    "bug1", [test_case "bug1" `Quick bug1]; 
    "bug3", [test_case "bug3" `Quick bug3]; 
    "bug4", [test_case "bug4" `Quick bug4]; 
    "bug5", [test_case "bug5" `Quick bug5]; 
    "bug6", [test_case "bug6" `Quick bug6]; 
    "bug2", [test_case "bug2" `Quick bug2]; 
    "bug8", [test_case "bug8" `Quick bug8]; 
    "probabilities", [test_case "probabilities" `Quick probabilities]; 
    "probabilities_2", [test_case "probabilities_2" `Quick probabilities_2]; 
    "example-fail", [test_case "example-fail" `Quick example_fail]; 
    "ngap-ngsetup", [test_case "ngap-ngsetup" `Quick ngap_ngsetup_bug]; 
    "reset_bug4", [test_case "reset_bug4" `Quick reset_bug4]; 
    "length_attr", [test_case "length_attr" `Quick length_attr]; 
    "length_attr_fail_1", [test_case "length_attr_fail_1" `Quick length_attr_fail_1]; 
    "length_attr_fail_2", [test_case "length_attr_fail_2" `Quick length_attr_fail_2]; 
    "length_attr_fail_3", [test_case "length_attr_fail_3" `Quick length_attr_fail_3]; 
    "inh_attr", [test_case "inh_attr" `Quick inh_attr]; 
    "inh_attr_fail_1", [test_case "inh_attr_fail_1" `Quick inh_attr_fail_1]; 
    "inh_attr_fail_2", [test_case "inh_attr_fail_2" `Quick inh_attr_fail_2]; 
    "inh_attr_fail_3", [test_case "inh_attr_fail_3" `Quick inh_attr_fail_3]; 
    "inh_attr_fail_4", [test_case "inh_attr_fail_4" `Quick inh_attr_fail_4]; 
    "inh_attr_fail_5", [test_case "inh_attr_fail_5" `Quick inh_attr_fail_5]; 
    "inh_attr_fail_6", [test_case "inh_attr_fail_6" `Quick inh_attr_fail_6]; 
    "inh_attr_fail_7", [test_case "inh_attr_fail_7" `Quick inh_attr_fail_7]; 
    "inh_attr_fail_8", [test_case "inh_attr_fail_8" `Quick inh_attr_fail_8]; 
    "inh_attr_fail_9", [test_case "inh_attr_fail_9" `Quick inh_attr_fail_9]; 
    "inh_attr_fail_10", [test_case "inh_attr_fail_10" `Quick inh_attr_fail_10]; 
    "con_suffix", [test_case "con_suffix" `Quick con_suffix]; 
    "inh_attr_scoped", [test_case "inh_attr_scoped" `Quick inh_attr_scoped]; 
    "inh_attr_synth_same_name", [test_case "inh_attr_synth_same_name" `Quick inh_attr_synth_same_name]; 
    "inh_attr_own_synth", [test_case "inh_attr_own_synth" `Quick inh_attr_own_synth]; 
    "index", [test_case "index" `Quick index]; 
    "too_many_constraints", [test_case "too_many_constraints" `Quick too_many_constraints]; 
    (*"msg2", [test_case "msg2" `Quick msg2]; *)
    (*"dd_test_another_ambiguous_reference_2", [test_case "dd_test_another_ambiguous_reference_2" `Quick dd_test_another_ambiguous_reference_2]; *)


  ]
