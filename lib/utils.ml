module StringMap = Map.Make(String)
module StringSet = Set.Make(String)
module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

module SILSet = Set.Make(struct
  type t = (string * int option) list 
  
  let compare l1 l2 =
    List.compare (fun (s1, i1) (s2, i2) -> 
    let cmp_str = String.compare s1 s2 in
    if cmp_str <> 0 then cmp_str
    else compare i1 i2) l1 l2 
end)

(* Module state for creating fresh identifiers *)
let k = ref 0

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let write_to_file filename content =
  let oc = open_out filename in  
  Printf.fprintf oc "%s\n" content; 
  close_out oc  

let append_to_file filename content =
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o666 filename in
  output_string oc content;
  close_out oc

let rec split3 lst =
  match lst with
  | [] -> ([], [], [])
  | (x, y, z) :: t ->
      let (xs, ys, zs) = split3 t in
      (x :: xs, y :: ys, z :: zs)

let mk_fresh_stub_id id = 
  let id = "_stub" ^ (string_of_int !k) ^ "_" ^ id in 
  k := !k + 1;
  String.uppercase_ascii id

let replicate value length =
  let rec replicate_aux value length acc =
    if length <= 0 then
      List.rev acc
    else
      replicate_aux value (length - 1) (value :: acc)
  in
  replicate_aux value length []
    
let find_index predicate lst =
  let rec aux i = function
    | [] -> raise Not_found
    | x :: xs -> if predicate x then i else aux (i + 1) xs
  in
  aux 0 lst

let find_index_opt predicate lst =
  let rec aux i = function
    | [] -> None
    | x :: xs -> if predicate x then Some i else aux (i + 1) xs
  in
  aux 0 lst

let capture_output: (Format.formatter -> 'a -> unit) -> 'a -> string = 
fun f arg ->
  let buf = Buffer.create 80 in
  let ppf = Format.formatter_of_buffer buf in
  f ppf arg;                
  Format.pp_print_flush ppf ();  
  Buffer.contents buf 

let pp_print_string_map_keys: Format.formatter -> 'a StringMap.t -> unit 
= fun ppf map -> 
  StringMap.iter (fun k _ -> Format.pp_print_string ppf k) map

let print_bytes_as_hex (b : bytes) =
  let print_byte byte =
    Printf.printf "%02X " (Char.code byte)
  in
  Bytes.iter print_byte b;
  print_newline ()
  
let rec recurse_until_fixpoint: 'a -> ('a -> 'a -> bool) -> ('a -> 'a) -> 'a = 
fun x eq f -> 
  let x' = f x in 
  if (eq x x') then x' 
  else recurse_until_fixpoint x' eq f

let last xs = xs |> List.rev |> List.hd 

let init xs = xs |> List.rev |> List.tl |> List.rev

let debug_print pp formatter value =
  if !Flags.debug then
    (pp formatter value; 
     Format.pp_print_flush formatter ();)
  else
    Format.ifprintf formatter "%a" pp value

let warning msg =
  if !Flags.no_warnings then
    ()
  else
    Format.printf "Warning: %s\n" msg 

let crash message = 
  match !Flags.filename with 
  | Some filename -> 
    raise (Failure ((Format.asprintf 
      "Internal error (%s): %s" filename message)))
  | None -> 
    raise (Failure ((Format.asprintf 
      "Internal error: %s" message)))

let error message (pos : Lexing.position) =
  let line = pos.Lexing.pos_lnum in
  let col  = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  let msg = Printf.sprintf "Error (%sline %d, column %d): %s" 
    (match !Flags.filename with 
    | Some filename -> filename ^ ", "
    | None -> "")
    line 
    col 
    message in
  raise (Failure msg)

let error_no_pos message =
  match !Flags.filename with 
  | Some filename -> 
    raise (Failure (Format.asprintf "Error (%s): %s" filename message))
  | None -> 
    raise (Failure (Format.asprintf "Error: %s" message))

let find_command_in_path cmd =
  match Sys.getenv_opt "PATH" with
  | None -> crash "$PATH is not set"
  | Some path ->
      let paths = String.split_on_char ':' path in
      let rec find_in_paths = function
        | [] -> crash (cmd ^ " not found in $PATH")
        | dir :: rest ->
            let full_path = Filename.concat dir cmd in
            if Sys.file_exists full_path && Sys.is_directory full_path = false then
              full_path
            else
              find_in_paths rest
      in
      find_in_paths paths

(* let rec drop lst n =
  match (lst, n) with
  | (xs, 0) -> xs
  | ([], x) -> 
    if x = 0 then [] else
    crash "drop"
  | (_ :: xs, n) -> drop xs (n - 1);;

let prefixes lst =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> aux ((match acc with [] -> [x] | p :: _ -> (p @ [x])) :: acc) xs
  in
  aux [] lst *)

let rec fresh_random_element exclude lst =
  let len = List.length lst in
  if len = 0 then
    invalid_arg "random_element: empty list"
  else
    let idx = Random.int len in
    if IntSet.cardinal exclude = List.length lst then (print_endline "infeasible"; exit 0);
    if IntSet.mem idx exclude then fresh_random_element exclude lst 
    else idx, List.nth lst idx

let random_bools n =
  let rec aux i acc =
    if i = 0 then acc
    else aux (i - 1) ((Random.bool ()) :: acc)
  in
  aux n []

let random_string n =
  let gen_char () =
    Char.chr (Random.int (126 - 32 + 1) + 32)  (* Range: 32 to 126 *)
  in
  String.init n (fun _ -> gen_char ())

let sequence_option (xs : 'a option list) : 'a list option =
  List.fold_right
    (fun opt acc ->
       match opt, acc with
       | Some x, Some xs -> Some (x :: xs)
       | _, _ -> None)
    xs
    (Some [])

let extract_base_name str =
  let str = String.lowercase_ascii str in
  let open Str in
  (* Remove optional trailing "_con" or "_con123" *)
  let str =
    global_replace (regexp "_con[0-9]*$") "" str
  in
  (* Remove prefix "_stub" *)
  let str =
    global_replace (Str.regexp "^_stub[0-9]*_") "" str
  in
  str

let str_eq_ci s1 s2 =
  String.lowercase_ascii s1 = String.lowercase_ascii s2

let parse_str_nat_suffix (s : string) : string * int option =
  let len = String.length s in
  let rec split_at_digit_suffix i =
    if i < 0 then 0
    else if Char.code s.[i] >= Char.code '0' && Char.code s.[i] <= Char.code '9'
    then split_at_digit_suffix (i - 1)
    else i + 1
  in
  let split_index = split_at_digit_suffix (len - 1) in
  let prefix = String.sub s 0 split_index in
  if split_index = len then
    (prefix, None)
  else
    let suffix = String.sub s split_index (len - split_index) in
    match int_of_string_opt suffix with
    | Some n -> (prefix, Some n)
    | None -> (s, None)  (* fallback, shouldn't normally happen *)

let all_equal (xs : 'a list) (eq : 'a -> 'a -> bool) : bool =
  match xs with
  | [] | [_] -> true
  | x :: rest -> List.for_all (fun y -> eq x y) rest

(* Naive but whatever *)
let has_duplicate eq lst =
  let rec aux = function
    | [] -> false
    | x :: xs ->
        if List.exists (fun y -> eq x y) xs then true
        else aux xs
  in
  aux lst
