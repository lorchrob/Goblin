let safe_f f name () =
    try f ()
    with exn ->
      Printf.eprintf "Function %s threw: %s\n%!" name (Printexc.to_string exn);
      raise exn

let race_n: ((unit -> 'a) * string) list -> 'a 
= fun funs -> 
  let funs = List.map (fun (f, id) -> safe_f f id, id) funs in 

  let result = Atomic.make None in

  let run id f =
    Domain.spawn (fun () ->
      try
        let r = f () in
        ignore (Atomic.compare_and_set result None (Some (id, r)))
      with exn ->
        Printf.eprintf "Domain %s crashed with: %s\n%s\n%!"
          id
          (Printexc.to_string exn)
          (Printexc.get_backtrace ());
        raise exn
    )
  in

  let ds = List.map (fun (f, id) -> run id f) funs in

  (* Wait until one finishes *)
  let (winner_id, winner) =
    let rec wait () =
      match Atomic.get result with
      | Some (id, r) -> (id, r)
      | None -> Domain.cpu_relax (); wait ()
    in wait ()
  in

  if !Flags.show_winner then Printf.printf "Function %s won the race!\n%!" winner_id;

  List.iter Domain.join ds;

  winner