let race3: (unit -> 'a) -> (unit -> 'a) -> (unit -> 'a) -> 'a
= fun f1 f2 f3 ->
  let result = Atomic.make None in
  let winner_id = Atomic.make None in

  let run id f =
    Domain.spawn (fun () ->
      try
        let r = f () in
        if Atomic.compare_and_set result None (Some r) then
          Atomic.set winner_id (Some id)
      with exn ->
        Printf.eprintf "Domain %d crashed with: %s\n%s\n"
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
  let rec wait () =
    match Atomic.get result with
    | Some r -> r
    | None -> Domain.cpu_relax (); wait ()
  in

  let winner = wait () in

  (* Log the winner *)
  (match Atomic.get winner_id with
   | Some id -> Printf.printf "Function %d won the race!\n%!" id
   | None -> Printf.printf "No winner could be determined.\n%!");

  (* Join all domains *)
  Domain.join d1;
  Domain.join d2;
  Domain.join d3;

  winner
