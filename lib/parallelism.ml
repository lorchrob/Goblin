exception AllReturnedNone

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

let race_n_opt: ((unit -> 'a option) * string) list -> 'a =
  fun funs ->
    let result = Atomic.make None in
    let remaining = Atomic.make (List.length funs) in

    let run id f =
      Domain.spawn (fun () ->
        try
          match f () with
          | Some r ->
              ignore (Atomic.compare_and_set result None (Some (id, r)))
          | None ->
              ignore (Atomic.fetch_and_add remaining (-1))
        with exn ->
          Printf.eprintf "Domain %s crashed with: %s\n%s\n%!"
            id
            (Printexc.to_string exn)
            (Printexc.get_backtrace ());
          ignore (Atomic.fetch_and_add remaining (-1));
          raise exn
      )
    in

    let ds = List.map (fun (f, id) -> run id f) funs in

    let rec wait () =
      match Atomic.get result with
      | Some (id, r) -> (id, r)
      | None ->
        if Atomic.get remaining = 0 then raise AllReturnedNone
        else (Domain.cpu_relax (); wait ())
    in

    let (winner_id, winner) = wait () in

    if !Flags.show_winner then Printf.printf "Function %s won the race!\n%!" winner_id;

    List.iter Domain.join ds;

    winner

(* Helper to split a list into n chunks. TODO: Review this function, and the next one *)
let split_into_chunks n lst =
  let len = List.length lst in
  let base_size = len / n in
  let remainder = len mod n in

  let rec take acc i lst =
    if i = 0 then (List.rev acc, lst)
    else match lst with
      | [] -> (List.rev acc, [])
      | x :: xs -> take (x :: acc) (i - 1) xs
  in

  let rec build_chunks i lst =
    if i >= n || lst = [] then []
    else
      let chunk_size = base_size + if i < remainder then 1 else 0 in
      let (chunk, rest) = take [] chunk_size lst in
      chunk :: build_chunks (i + 1) rest
  in
  build_chunks 0 lst

(* Parallel map, but with a bounded level of parallelism *)
let parallel_map (f : 'a -> 'b) (xs : 'a list) : 'b list =
  let num_domains = min (Domain.recommended_domain_count ()) (List.length xs) in
  let chunks = split_into_chunks num_domains xs in
  let tasks = List.map (fun chunk ->
    Domain.spawn (fun () -> List.map f chunk)
  ) chunks in
  List.concat (List.map Domain.join tasks)

let safe_kill pid =
  try Unix.kill pid Sys.sigterm
  with Unix.Unix_error (Unix.ESRCH, _, _) -> ()

(* Run two commands in parallel and report which finishes first. TODO: Review this code *)
let race_commands cmd1 cmd2 =
  let pid1 = Unix.create_process "bash" [| "bash"; "-c"; cmd1 |] Unix.stdin Unix.stdout Unix.stderr in
  let pid2 = Unix.create_process "bash" [| "bash"; "-c"; cmd2 |] Unix.stdin Unix.stdout Unix.stderr in

  let result = Atomic.make None in

  let wait_and_set pid other result_val =
    let _, _ = Unix.waitpid [] pid in
    if Atomic.compare_and_set result None (Some result_val) then (
      ignore (safe_kill other)
    )
  in

  let t1 = Thread.create (fun () -> wait_and_set pid1 pid2 true) () in
  let t2 = Thread.create (fun () -> wait_and_set pid2 pid1 false) () in

  let rec wait () =
    match Atomic.get result with
    | Some x -> x
    | None -> Thread.delay 0.01; wait ()
  in

  let winner = wait () in
  Thread.join t1;
  Thread.join t2;
  winner
