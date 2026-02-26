module A = Ast

type solver_instance = {
  in_channel : in_channel;   
  out_channel : out_channel; 
  err_channel : in_channel;  
}

let issue_solver_command: string -> solver_instance -> unit 
= fun cmd_string solver -> 
  if !Flags.debug then Format.pp_print_string Format.std_formatter ("Issuing " ^ cmd_string ^ "\n");
  output_string solver.out_channel cmd_string;
  flush solver.out_channel

let read_check_sat_response solver = 
  input_line solver.in_channel

let read_get_model_response solver =
  let rec loop acc =
    try
      let line = input_line solver.in_channel in
      if String.starts_with ~prefix:"(error" line then raise (Failure "cvc5 error");
      let acc = acc ^ "\n" ^ line in
      if String.trim line = ")" then acc
      else loop acc
    with End_of_file -> acc
  in
  let result = loop "" in
  result

let initialize_solver () : solver_instance =
  let cvc5 = Utils.find_command_in_path "cvc5" in
  let cmd = 
    Printf.sprintf "%s --produce-models --dag-thresh=0 --lang=smtlib2 --incremental" 
      cvc5 
  in
  let set_logic_command = Format.asprintf "(set-logic QF_BVSNIAFS)\n" in
  let declare_unit_type_command = "(declare-sort Unit 0)" in
  (*let z3 = Utils.find_command_in_path "z3" in
  let cmd = 
    Printf.sprintf "%s -smt2 -in" 
     z3 
  in
  let set_logic_command = Format.asprintf "(set-logic QF_SLIA)\n" in*)

  let (in_chan, out_chan, err_chan) = Unix.open_process_full cmd (Unix.environment ()) in
  let solver = { in_channel = in_chan; out_channel = out_chan; err_channel = err_chan } in
  issue_solver_command set_logic_command solver;
  issue_solver_command declare_unit_type_command solver;
  issue_solver_command "(set-option :produce-models true)\n" solver;
  solver

let cleanup_solver (solver : solver_instance) : unit =
  try
    ignore (Unix.close_process_full
              (solver.in_channel, solver.out_channel, solver.err_channel))
  with Unix.Unix_error _ -> ()

let assert_smt_constraint: solver_instance -> Ast.expr -> unit 
= fun solver expr ->
  let assert_cmd = 
    Format.asprintf "(assert %a)\n" (SmtPrinter.pp_print_expr Utils.StringMap.empty) expr 
  in
  issue_solver_command assert_cmd solver; 
  ()


