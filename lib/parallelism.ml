let race3: (unit -> 'a) -> (unit -> 'a) -> (unit -> 'a) -> 'a
= fun f1 f2 f3 ->
  let result = Atomic.make None in

  let run id f =
    Domain.spawn (fun () ->
      try
        let r = f () in
        (* Combine result and ID into a tuple so it can be updated atomically *)
        ignore (Atomic.compare_and_set result None (Some (id, r)))
      with exn ->
        Printf.eprintf "Domain %d crashed with: %s\n%s\n%!"
          id
          (Printexc.to_string exn)
          (Printexc.get_backtrace ());
        raise exn
    )
  in

  let d1 = run 1 f1 in
  let d2 = run 2 f2 in
  let d3 = run 3 f3 in

  (* Wait until one finishes *)
  let (winner_id, winner) =
    let rec wait () =
      match Atomic.get result with
      | Some (id, r) -> (id, r)
      | None -> Domain.cpu_relax (); wait ()
    in wait ()
  in

  Printf.printf "Function %d won the race!\n%!" winner_id;

  Domain.join d1;
  Domain.join d2;
  Domain.join d3;

  winner
