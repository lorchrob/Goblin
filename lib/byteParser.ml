(* open Bitstring *)
(* open Yojson.Basic *)

type state = NOTHING_ | CONFIRMED_ | ACCEPTED_ | IGNORE_

let char_to_binary c =
  let ascii = Char.code c in
  Printf.sprintf "%01x" ascii  (* Format the ASCII value as an 8-bit binary string *)

(* Function to convert a string to a binary representation *)
let string_to_hex s =
  let binary_strings = List.map char_to_binary (List.init (String.length s) (fun i -> String.get s i)) in
  String.concat "" binary_strings  (* Concatenate all binary strings *)

let bitstring_to_hex (bitstr : Bitstring.bitstring) : string =
  (* print_endline (Bitstring.string_of_bitstring bitstr) ; *)
  let rec to_hex acc bitstr =
    let len = Bitstring.bitstring_length bitstr in
    (* Printf.printf "Length of bitstring: %d\n" len; *)
    if len = 0 then
      acc  (* Stop when there are no more bits *)
    else if len < 8 then
      acc  (* Handle cases where there are less than 8 bits left *)
    else
      match%bitstring bitstr with
      | {| byte : 8; rest : -1 : bitstring |} ->
        (* Convert each byte to a two-digit hex string and recurse *)
        to_hex (acc ^ Printf.sprintf "%02x" byte) rest
  in
  to_hex "" bitstr

let write_json_to_file filename json_data =
  let oc = open_out_bin filename in
  output_string oc json_data;
  close_out oc

let clear_file filename =
  let _ = Unix.system ("touch " ^ filename) in
  Unix.sleepf 0.1 ;
  print_endline "clear_file" ;
  let oc = open_out filename in
  close_out oc  (* Opens and immediately closes the file to clear its content *)
  
let load_bitstring_from_file filename =
  let channel = open_in_bin filename in
  try
    let length = in_channel_length channel in
    let content = really_input_string channel length in
    close_in channel;
    Bitstring.bitstring_of_string content
  with e ->
    close_in_noerr channel;
    raise e

let read_from_file filename =
  let _ = Unix.system ("touch " ^ filename) in
  Unix.sleepf 0.1 ;
  print_endline "read_from_file" ;

  let ic = open_in filename in
  try
    let line = input_line ic in
    close_in ic;
    Some line
  with End_of_file ->
    close_in ic;
    None

let wait_for_oracle_response response_file =
  let _ = Unix.system ("touch " ^ response_file) in
  Unix.sleepf 0.1 ;
  print_endline "wait_for__oracle_python" ;
  let rec loop () =
    match read_from_file response_file with
    | Some response ->
        (* Clear the file after reading *)
        clear_file response_file;
        if response = "NOTHING" then NOTHING_
        else if response = "CONFIRMED" then CONFIRMED_
        else if response = "ACCEPTED" then ACCEPTED_
        else if response = "IGNORE" then IGNORE_
        else Utils.crash "unexpected condition in oracle.."
    | None ->
        Unix.sleepf 0.1;  (* Wait for a while before checking again *)
        loop ()
  in
  loop ()

  let parse_packet (packet : Bitstring.bitstring) : unit =
    match%bitstring packet with
      (* LONGEST PATTERNS FIRST - With ac_token and all three containers *)
      | {| algo : 16 : littleendian 
      ;auth_seq : 16 : littleendian
      ;status : 16 : littleendian  
      ;_group_id : 16 
      ;ac_token : 80 : bitstring ; scalar : 64 :bitstring ; element : 72 : bitstring
      ;_pi_container : 8
      ;_pi_length : 8 
      ;pi_id : 8
      ;pi_id_list : (_pi_length - 1) * 8 : bitstring
      ;_rg_container : 8 
      ;_rg_length : 8 
      ;rg_id : 8
      ;rg_id_list : (_rg_length - 1) * 8 : bitstring
      ;_ac_container : 8 
      ;_ac_length : 8 
      ;ac_id : 8
      ;ac_id_list : (_ac_length - 1) * 8 : bitstring
      |} 
      when rg_id = 92 && pi_id = 33 && ac_id = 93 ->
        let json_to_driver = Printf.sprintf {|
          {
          "algo" : "%d",
          "auth_seq" : "%d",
          "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "ac_token" : "%s",
            "pi_list" : "%s",
            "rg_list" : "%s",
            "ac_list" : "%s"
          }
        |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex ac_token) (bitstring_to_hex pi_id_list) (bitstring_to_hex rg_id_list) (bitstring_to_hex ac_id_list) in
        write_json_to_file "sync/driver_oracle.json" json_to_driver ;

      (* Without ac_token but with all three containers *)
      | {| algo : 16 : littleendian; 
        auth_seq : 16 : littleendian;
        status : 16 : littleendian ; 
        _group_id : 16 ;
        scalar : 64 :bitstring ; 
        element : 72 : bitstring ;
        _pi_container : 8 ; 
        _pi_length : 8 ;
        pi_id : 8 ; 
        pi_id_list : (_pi_length - 1) * 8 : bitstring;
        _rg_container : 8 ; 
        _rg_length : 8 ;
        rg_id : 8 ; 
        rg_id_list : (_rg_length - 1) * 8 : bitstring;
        _ac_container : 8 ; 
        _ac_length : 8 ;
        ac_id : 8 ; 
        ac_id_list : (_ac_length - 1) * 8 : bitstring 
        |} 
        when rg_id = 92 && pi_id = 33 && ac_id = 93 ->
          let json_to_driver = Printf.sprintf {|
          {
            "algo" : "%d",
            "auth_seq" : "%d",
            "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "pi_list" : "%s",
            "rg_list" : "%s",
            "ac_list" : "%s"
          }
        |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex pi_id_list) (bitstring_to_hex rg_id_list) (bitstring_to_hex ac_id_list) in
        write_json_to_file "sync/driver_oracle.json" json_to_driver ;

      (* MEDIUM LENGTH PATTERNS - Two containers with ac_token *)
      
      (* With ac_token, pi and rg containers *)
      | {| algo : 16 : littleendian 
      ;auth_seq : 16 : littleendian
      ;status : 16 : littleendian  
      ;_group_id : 16 
      ;ac_token : 80 : bitstring 
      ;scalar : 64 :bitstring  
      ;element : 72 : bitstring 
      ;_pi_container : 8  
      ;_pi_length : 8 
      ;pi_id : 8  
      ;pi_id_list : (_pi_length - 1) * 8 : bitstring 
      ;_rg_container : 8  
      ;_rg_length : 8 
      ;rg_id : 8  
      ;rg_id_list : (_rg_length - 1) * 8 : bitstring 
      |} 
      when rg_id = 92 && pi_id = 33 ->
        let json_to_driver = Printf.sprintf {|
          {
          "algo" : "%d",
          "auth_seq" : "%d",
          "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "ac_token" : "%s",
            "pi_list" : "%s",
            "rg_list" : "%s"
          }
        |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex ac_token) (bitstring_to_hex pi_id_list) (bitstring_to_hex rg_id_list) in
        write_json_to_file "sync/driver_oracle.json" json_to_driver ;

      (* With ac_token, pi and ac containers *)
      | {| algo : 16 : littleendian 
          ;auth_seq : 16 : littleendian
          ;status : 16 : littleendian  
          ;_group_id : 16 
          ;ac_token : 80 : bitstring 
          ;scalar : 64 :bitstring  
          ;element : 72 : bitstring 
          ;_pi_container : 8  
          ;_pi_length : 8 
          ;pi_id : 8  
          ;pi_id_list : (_pi_length - 1) * 8 : bitstring 
          ;_ac_container : 8  
          ;_ac_length : 8 
          ;ac_id : 8  
          ;ac_id_list : (_ac_length - 1) * 8 : bitstring 
          |} 
        when pi_id = 33 && ac_id = 93 ->
          let json_to_driver = Printf.sprintf {|
            {
            "algo" : "%d",
            "auth_seq" : "%d",
            "status" : "%s",
              "scalar" : "%s",
              "element" : "%s",
              "ac_token" : "%s",
              "pi_list" : "%s",
              "ac_list" : "%s"
            }
          |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex ac_token) (bitstring_to_hex pi_id_list) (bitstring_to_hex ac_id_list) in
          write_json_to_file "sync/driver_oracle.json" json_to_driver ;

      (* With ac_token, rg and ac containers *)
      | {| algo : 16 : littleendian 
        ;auth_seq : 16 : littleendian
        ;status : 16 : littleendian  
        ;_group_id : 16 
        ;ac_token : 80 : bitstring 
        ;scalar : 64 :bitstring  
        ;element : 72 : bitstring 
        ;_rg_container : 8  
        ;_rg_length : 8 
        ;rg_id : 8  
        ;rg_id_list : (_rg_length - 1) * 8 : bitstring 
        ;_ac_container : 8  
        ;_ac_length : 8 
        ;ac_id : 8  
        ;ac_id_list : (_ac_length - 1) * 8 : bitstring 
        |} 
        when rg_id = 92 && ac_id = 93 ->
          let json_to_driver = Printf.sprintf {|
            {
            "algo" : "%d",
            "auth_seq" : "%d",
            "status" : "%s",
              "scalar" : "%s",
              "element" : "%s",
              "ac_token" : "%s",
              "rg_list" : "%s",
              "ac_list" : "%s"
            }
          |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex ac_token) (bitstring_to_hex rg_id_list) (bitstring_to_hex ac_id_list) in
          write_json_to_file "sync/driver_oracle.json" json_to_driver ;

      (* Two containers without ac_token *)
      
      (* Without ac_token but with pi and rg containers *)
      | {| algo : 16 : littleendian; 
        auth_seq : 16 : littleendian;
        status : 16 : littleendian ; 
        _group_id : 16 ;
        scalar : 64 :bitstring ; 
        element : 72 : bitstring ;
        _pi_container : 8 ; 
        _pi_length : 8 ;
        pi_id : 8 ; 
        pi_id_list : (_pi_length - 1) * 8 : bitstring;
        _rg_container : 8 ; 
        _rg_length : 8 ;
        rg_id : 8 ; 
        rg_id_list : (_rg_length - 1) * 8 : bitstring
        |} 
        when rg_id = 92 && pi_id = 33 ->
          let json_to_driver = Printf.sprintf {|
          {
            "algo" : "%d",
            "auth_seq" : "%d",
            "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "pi_list" : "%s",
            "rg_list" : "%s"
          }
        |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex pi_id_list) (bitstring_to_hex rg_id_list) in
        write_json_to_file "sync/driver_oracle.json" json_to_driver ;

      (* Without ac_token but with pi and ac containers *)
      | {| algo : 16 : littleendian; 
        auth_seq : 16 : littleendian;
        status : 16 : littleendian ; 
        _group_id : 16 ;
        scalar : 64 :bitstring ; 
        element : 72 : bitstring ;
        _pi_container : 8 ; 
        _pi_length : 8 ;
        pi_id : 8 ; 
        pi_id_list : (_pi_length - 1) * 8 : bitstring;
        _ac_container : 8 ; 
        _ac_length : 8 ;
        ac_id : 8 ; 
        ac_id_list : (_ac_length - 1) * 8 : bitstring
        |} 
        when pi_id = 33 && ac_id = 93 ->
          let json_to_driver = Printf.sprintf {|
          {
            "algo" : "%d",
            "auth_seq" : "%d",
            "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "pi_list" : "%s",
            "ac_list" : "%s"
          }
        |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex pi_id_list) (bitstring_to_hex ac_id_list) in
        write_json_to_file "sync/driver_oracle.json" json_to_driver ;

      (* Without ac_token but with rg and ac containers *)
      | {| algo : 16 : littleendian; 
        auth_seq : 16 : littleendian;
        status : 16 : littleendian ; 
        _group_id : 16 ;
        scalar : 64 :bitstring ; 
        element : 72 : bitstring ;
        _rg_container : 8 ; 
        _rg_length : 8 ;
        rg_id : 8 ; 
        rg_id_list : (_rg_length - 1) * 8 : bitstring;
        _ac_container : 8 ; 
        _ac_length : 8 ;
        ac_id : 8 ; 
        ac_id_list : (_ac_length - 1) * 8 : bitstring
        |} 
        when rg_id = 92 && ac_id = 93 ->
          let json_to_driver = Printf.sprintf {|
          {
            "algo" : "%d",
            "auth_seq" : "%d",
            "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "rg_list" : "%s",
            "ac_list" : "%s"
          }
        |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex rg_id_list) (bitstring_to_hex ac_id_list) in
        write_json_to_file "sync/driver_oracle.json" json_to_driver ;

      (* SHORTER PATTERNS - One container with ac_token *)
      
      (* With ac_token and only pi container *)
      | {| algo : 16 : littleendian 
      ;auth_seq : 16 : littleendian
      ;status : 16 : littleendian  
      ;_group_id : 16 
      ;ac_token : 80 : bitstring 
      ;scalar : 64 :bitstring  
      ;element : 72 : bitstring 
      ;_pi_container : 8  
      ;_pi_length : 8 
      ;pi_id : 8  
      ;pi_id_list : (_pi_length - 1) * 8 : bitstring 
      |} 
      when pi_id = 33 ->
        let json_to_driver = Printf.sprintf {|
          {
          "algo" : "%d",
          "auth_seq" : "%d",
          "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "ac_token" : "%s",
            "pi_list" : "%s"
          }
        |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex ac_token) (bitstring_to_hex pi_id_list) in
        write_json_to_file "sync/driver_oracle.json" json_to_driver ;
        
      (* With ac_token and only rg container *)
      | {| algo : 16 : littleendian 
        ;auth_seq : 16 : littleendian
        ;status : 16 : littleendian  
        ;_group_id : 16 
        ;ac_token : 80 : bitstring 
        ;scalar : 64 :bitstring  
        ;element : 72 : bitstring 
        ;_rg_container : 8  
        ;_rg_length : 8 
        ;rg_id : 8  
        ;rg_id_list : (_rg_length - 1) * 8 : bitstring 
        |}
        when rg_id = 92 ->
          let json_to_driver = Printf.sprintf {|
            {
            "algo" : "%d",
            "auth_seq" : "%d",
            "status" : "%s",
              "scalar" : "%s",
              "element" : "%s",
              "ac_token" : "%s",
              "rg_list" : "%s"
            }
          |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex ac_token) (bitstring_to_hex rg_id_list) in
          write_json_to_file "sync/driver_oracle.json" json_to_driver ;
          
      (* With ac_token and only ac container *)
      | {| algo : 16 : littleendian 
        ;auth_seq : 16 : littleendian
        ;status : 16 : littleendian  
        ;_group_id : 16 
        ;ac_token : 80 : bitstring 
        ;scalar : 64 :bitstring  
        ;element : 72 : bitstring 
        ;_ac_container : 8  
        ;_ac_length : 8 
        ;ac_id : 8  
        ;ac_id_list : (_ac_length - 1) * 8 : bitstring 
        |} 
        when ac_id = 93 ->
          let json_to_driver = Printf.sprintf {|
            {
            "algo" : "%d",
            "auth_seq" : "%d",
            "status" : "%s",
              "scalar" : "%s",
              "element" : "%s",
              "ac_token" : "%s",
              "ac_list" : "%s"
            }
          |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex ac_token) (bitstring_to_hex ac_id_list) in
            write_json_to_file "sync/driver_oracle.json" json_to_driver ;

      (* One container without ac_token *)

      (* Without ac_token and with only pi container *)
      | {| algo : 16 : littleendian; 
        auth_seq : 16 : littleendian;
        status : 16 : littleendian ; 
        _group_id : 16 ;
        scalar : 64 :bitstring ; 
        element : 72 : bitstring ;
        _pi_container : 8 ; 
        _pi_length : 8 ;
        pi_id : 8 ; 
        pi_id_list : (_pi_length - 1) * 8 : bitstring
        |} 
        when pi_id = 33 ->
          let json_to_driver = Printf.sprintf {|
          {
            "algo" : "%d",
            "auth_seq" : "%d",
            "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "pi_list" : "%s"
          }
        |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex pi_id_list) in
        write_json_to_file "sync/driver_oracle.json" json_to_driver ;

      (* Without ac_token and with only rg container *)
      | {| algo : 16 : littleendian; 
        auth_seq : 16 : littleendian;
        status : 16 : littleendian ; 
        _group_id : 16 ;
        scalar : 64 :bitstring ; 
        element : 72 : bitstring ;
        _rg_container : 8 ; 
        _rg_length : 8 ;
        rg_id : 8 ; 
        rg_id_list : (_rg_length - 1) * 8 : bitstring
        |} 
        when rg_id = 92 ->
          let json_to_driver = Printf.sprintf {|
          {
            "algo" : "%d",
            "auth_seq" : "%d",
            "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "rg_list" : "%s"
          }
        |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex rg_id_list) in
        write_json_to_file "sync/driver_oracle.json" json_to_driver ;

      (* Without ac_token and with only ac container *)
      | {| algo : 16 : littleendian; 
        auth_seq : 16 : littleendian;
        status : 16 : littleendian ; 
        _group_id : 16 ;
        scalar : 64 :bitstring ; 
        element : 72 : bitstring ;
        _ac_container : 8 ; 
        _ac_length : 8 ;
        ac_id : 8 ; 
        ac_id_list : (_ac_length - 1) * 8 : bitstring
        |} 
        when ac_id = 93 ->
          let json_to_driver = Printf.sprintf {|
          {
            "algo" : "%d",
            "auth_seq" : "%d",
            "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "ac_list" : "%s"
          }
        |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex ac_id_list) in
        write_json_to_file "sync/driver_oracle.json" json_to_driver ;

      (* SHORTEST PATTERNS - Special cases and base patterns *)
      
      (* Special case for confirm message *)
      | {| algo : 16 : littleendian; auth_seq : 16 : littleendian; status : 16 : littleendian ; send_confirm : 176 : bitstring ; confirm_hash : 112 : bitstring |}
      when algo = 3 && auth_seq = 2 ->
        let json_to_driver = Printf.sprintf {|
        {
          "algo" : "%d",
          "auth_seq" : "%d",
          "status" : %d,
          "send_confirm" : "%s",
          "confirm_hash" : "%s"
        }
      |} algo auth_seq status (bitstring_to_hex send_confirm) (bitstring_to_hex confirm_hash) in
      write_json_to_file "sync/driver_oracle.json" json_to_driver ;
            
      (* With ac_token but no containers *)
      | {| algo : 16 : littleendian; auth_seq : 16 : littleendian; status : 16 : littleendian ; _group_id : 16 ; ac_token : 80 : bitstring ; scalar : 64 :bitstring ; element : 72 : bitstring |} ->
      (* when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) ->   *)
        let json_to_driver = Printf.sprintf {|
        {
          "algo" : "%d",
          "auth_seq" : "%d",
          "status" : "%s",
          "scalar" : "%s",
          "element" : "%s",
          "ac_token" : "%s"
        }
      |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) (bitstring_to_hex ac_token) in
      write_json_to_file "sync/driver_oracle.json" json_to_driver ;
       
      (* Without ac_token and no containers - base case *)
      | {| algo : 16 : littleendian; auth_seq : 16 : littleendian; status : 16 : littleendian ; _group_id : 16 ; scalar : 64 :bitstring ; element : 72 : bitstring |} ->
      (* when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) -> *)
        let json_to_driver = Printf.sprintf {|
        {
          "algo" : "%d",
          "auth_seq" : "%d",
          "status" : "%s",
          "scalar" : "%s",
          "element" : "%s"
        }
      |} algo auth_seq (string_of_int status) (bitstring_to_hex scalar) (bitstring_to_hex element) in
      write_json_to_file "sync/driver_oracle.json" json_to_driver ;
      
      (* Default handler - catch all remaining cases *)
      | {| _ |} as packet ->
        (* Try to extract as much data as possible for debugging *)
        let debug_info = Printf.sprintf {|
          {
            "failed" : "True",
            "packet_hex" : "%s"
          }
        |} (bitstring_to_hex packet) in
        write_json_to_file "sync/driver_oracle.json" debug_info
(* let get_bytes_and_run filename =
  let _bytes = load_bitstring_from_file filename in
  parse_packet _bytes *)