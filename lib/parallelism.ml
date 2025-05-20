let race3 f1 f2 f3 =
  (* Store both the result and the winner's label *)
  let result = Atomic.make None in

  let run label f =
    Domain.spawn (fun () ->
      let r = f () in
      let _ = Atomic.compare_and_set result None (Some (label, r)) in
      ()
    )
  in

  let d1 = run "f1" f1 in
  let d2 = run "f2" f2 in
  let d3 = run "f3" f3 in

  (* Wait until one finishes *)
  let rec wait () =
    match Atomic.get result with
    | Some res -> res
    | None -> Domain.cpu_relax (); wait ()
  in

  let (label, winner) = wait () in

  if !Flags.debug then Format.fprintf Format.std_formatter "Winner: %s\n" label;

  (* Optionally join all (cleanup) *)
  Domain.join d1;
  Domain.join d2;
  Domain.join d3;

  winner
