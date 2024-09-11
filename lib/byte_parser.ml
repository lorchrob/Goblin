open Bitstring
(* open Yojson.Basic *)

type state = NOTHING_ | CONFIRMED_ | ACCEPTED_ | IGNORE_

let char_to_binary c =
  let ascii = Char.code c in
  Printf.sprintf "%01x" ascii  (* Format the ASCII value as an 8-bit binary string *)

(* Function to convert a string to a binary representation *)
let string_to_hex s =
  let binary_strings = List.map char_to_binary (List.init (String.length s) (fun i -> String.get s i)) in
  String.concat "" binary_strings  (* Concatenate all binary strings *)

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
        else failwith "unexpected condition in oracle.."
    | None ->
        Unix.sleepf 0.1;  (* Wait for a while before checking again *)
        loop ()
  in
  loop ()

let parse_packet (packet : Bitstring.bitstring) : state =
  match%bitstring packet with
  | {| algo : 16 : littleendian; auth_seq : 16 : littleendian; status : 16 : littleendian ; send_confirm : 16 : littleendian ; confirm_hash : 256 : bitstring |}
    when algo = 3 && auth_seq = 2 && status = 0 ->
      let json_to_driver = Printf.sprintf {|
      {
        "send_confirm" : "%s",
        "confirm_hash" : "%s"
      }
    |} (string_of_int send_confirm) (string_to_hex (string_of_bitstring confirm_hash)) in
    write_json_to_file "driver_oracle.json" json_to_driver ;
    wait_for_oracle_response "../oracle-response.txt"
    | {| algo : 16 : littleendian 
    ;auth_seq : 16 : littleendian
    ;status : 16 : littleendian  
    ;_group_id : 16 
    ;ac_token : 256 : bitstring ; scalar : 256 : bitstring ; element : 512 : bitstring
    ;_pi_container : 8
    ;_pi_length : 8 
    ;pi_id : 8
    ;pi_id_list : _pi_length * 8 
    ;_rg_container : 8 
    ;_rg_length : 8 
    ;rg_id : 8
    ;rg_id_list : _rg_length * 8 * 2 
    ;_ac_container : 8 
    ;_ac_length : 8 
    ;ac_id : 8
    ;ac_id_list : _ac_length * 8 
    |} 
    when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 && pi_id = 33 && ac_id = 93 ->
      let json_to_driver = Printf.sprintf {|
        {
        "status" : "%s",
          "scalar" : "%s",
          "element" : "%s",
          "ac_token" : "%s",
          "pi_list" : "%s",
          "rg_list" : "%s",
          "ac_list" : "%s"
        }
      |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring ac_token)) (string_to_hex (string_of_bitstring pi_id_list)) (string_to_hex (string_of_bitstring rg_id_list)) (string_to_hex (string_of_bitstring ac_id_list)) in
      write_json_to_file "driver_oracle.json" json_to_driver ;
    wait_for_oracle_response "../oracle-response.txt"
    | {| algo : 16 : littleendian 
    ;auth_seq : 16 : littleendian
    ;status : 16 : littleendian  
    ;_group_id : 16 
    ;ac_token : 256 : bitstring 
    ;scalar : 256 : bitstring  
    ;element : 512 : bitstring 
    ;_pi_container : 8  
    ;_pi_length : 8 
    ;pi_id : 8  
    ;pi_id_list : _pi_length * 8 
    ;_rg_container : 8  
    ;_rg_length : 8 
    ;rg_id : 8  
    ;rg_id_list : _rg_length * 8 * 2 
    |} 
    when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 && pi_id = 33 ->
      let json_to_driver = Printf.sprintf {|
        {
        "status" : "%s",
          "scalar" : "%s",
          "element" : "%s",
          "ac_token" : "%s",
          "pi_list" : "%s",
          "rg_list" : "%s"
        }
      |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring ac_token)) (string_to_hex (string_of_bitstring pi_id_list)) (string_to_hex (string_of_bitstring rg_id_list)) in
      write_json_to_file "driver_oracle.json" json_to_driver ;
      wait_for_oracle_response "../oracle-response.txt"
    | {| algo : 16 : littleendian 
      ;auth_seq : 16 : littleendian
      ;status : 16 : littleendian  
      ;_group_id : 16 
      ;ac_token : 256 : bitstring 
      ;scalar : 256 : bitstring  
      ;element : 512 : bitstring 
      ;_rg_container : 8  
      ;_rg_length : 8 
      ;rg_id : 8  
      ;rg_id_list : _rg_length * 8 * 2 
      ;_ac_container : 8  
      ;_ac_length : 8 
      ;ac_id : 8  
      ;ac_id_list : _ac_length * 8 
      |} 
      when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 && ac_id = 93 ->
        let json_to_driver = Printf.sprintf {|
          {
          "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "ac_token" : "%s",
            "rg_list" : "%s",
            "ac_list" : "%s"
          }
        |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring ac_token)) (string_to_hex (string_of_bitstring rg_id_list)) (string_to_hex (string_of_bitstring ac_id_list)) in
        write_json_to_file "driver_oracle.json" json_to_driver ;
        wait_for_oracle_response "../oracle-response.txt"
    | {| algo : 16 : littleendian 
        ;auth_seq : 16 : littleendian
        ;status : 16 : littleendian  
        ;_group_id : 16 
        ;ac_token : 256 : bitstring 
        ;scalar : 256 : bitstring  
        ;element : 512 : bitstring 
        ;_pi_container : 8  
        ;_pi_length : 8 
        ;pi_id : 8  
        ;pi_id_list : _pi_length * 8 
        ;_ac_container : 8  
        ;_ac_length : 8 
        ;ac_id : 8  
        ;ac_id_list : _ac_length * 8 
        |} 
      when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && pi_id = 33 && ac_id = 93 ->
        let json_to_driver = Printf.sprintf {|
          {
          "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "ac_token" : "%s",
            "pi_list" : "%s",
            "ac_list" : "%s"
          }
        |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring ac_token)) (string_to_hex (string_of_bitstring pi_id_list)) (string_to_hex (string_of_bitstring ac_id_list)) in
        write_json_to_file "driver_oracle.json" json_to_driver ;
        wait_for_oracle_response "../oracle-response.txt"
    
    | {| algo : 16 : littleendian 
    ;auth_seq : 16 : littleendian
    ;status : 16 : littleendian  
    ;_group_id : 16 
    ;ac_token : 256 : bitstring 
    ;scalar : 256 : bitstring  
    ;element : 512 : bitstring 
    ;_pi_container : 8  
    ;_pi_length : 8 
    ;pi_id : 8  
    ;pi_id_list : _pi_length * 8 
    |} 
    when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && pi_id = 33 ->
      let json_to_driver = Printf.sprintf {|
        {
        "status" : "%s",
          "scalar" : "%s",
          "element" : "%s",
          "ac_token" : "%s",
          "pi_list" : "%s"
        }
      |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring ac_token)) (string_to_hex (string_of_bitstring pi_id_list)) in
      write_json_to_file "driver_oracle.json" json_to_driver ;
      wait_for_oracle_response "../oracle-response.txt"
    | {| algo : 16 : littleendian 
      ;auth_seq : 16 : littleendian
      ;status : 16 : littleendian  
      ;_group_id : 16 
      ;ac_token : 256 : bitstring 
      ;scalar : 256 : bitstring  
      ;element : 512 : bitstring 
      ;_rg_container : 8  
      ;_rg_length : 8 
      ;rg_id : 8  
      ;rg_id_list : _rg_length * 8 * 2 
      |}
      when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 ->
        let json_to_driver = Printf.sprintf {|
          {
          "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "ac_token" : "%s",
            "rg_list" : "%s"
          }
        |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring ac_token)) (string_to_hex (string_of_bitstring rg_id_list)) in
        write_json_to_file "driver_oracle.json" json_to_driver ;
        wait_for_oracle_response "../oracle-response.txt"
    | {| algo : 16 : littleendian 
      ;auth_seq : 16 : littleendian
      ;status : 16 : littleendian  
      ;_group_id : 16 
      ;ac_token : 256 : bitstring 
      ;scalar : 256 : bitstring  
      ;element : 512 : bitstring 
      ;_ac_container : 8  
      ;_ac_length : 8 
      ;ac_id : 8  
      ;ac_id_list : _ac_length * 8 
      |} 
      when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && ac_id = 93 ->
        let json_to_driver = Printf.sprintf {|
          {
          "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "ac_token" : "%s",
            "ac_list" : "%s"
          }
        |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring ac_token)) (string_to_hex (string_of_bitstring ac_id_list)) in
          write_json_to_file "driver_oracle.json" json_to_driver ;
          wait_for_oracle_response "../oracle-response.txt"
    
    | {| algo : 16 : littleendian; 
      auth_seq : 16 : littleendian;
      status : 16 : littleendian ; 
      _group_id : 16 ;
      scalar : 256 : bitstring ; 
      element : 512 : bitstring ;
      _pi_container : 8 ; 
      _pi_length : 8 ;
      pi_id : 8 ; 
      pi_id_list : _pi_length * 8;
      _rg_container : 8 ; 
      _rg_length : 8 ;
      rg_id : 8 ; 
      rg_id_list : _rg_length * 8 * 2;
      _ac_container : 8 ; 
      _ac_length : 8 ;
      ac_id : 8 ; 
      ac_id_list : _ac_length * 8 
      |} 
      when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 && pi_id = 33 && ac_id = 93 ->
        let json_to_driver = Printf.sprintf {|
        {
          "status" : "%s",
          "scalar" : "%s",
          "element" : "%s",
          "pi_list" : "%s",
          "rg_list" : "%s",
          "ac_list" : "%s"
        }
      |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring pi_id_list)) (string_to_hex (string_of_bitstring rg_id_list)) (string_to_hex (string_of_bitstring ac_id_list)) in
      write_json_to_file "driver_oracle.json" json_to_driver ;
      wait_for_oracle_response "../oracle-response.txt"
    
    | {| algo : 16 : littleendian; 
      auth_seq : 16 : littleendian;
      status : 16 : littleendian ; 
      _group_id : 16 ;
      scalar : 256 : bitstring ; 
      element : 512 : bitstring ;
      _pi_container : 8 ; 
      _pi_length : 8 ;
      pi_id : 8 ; 
      pi_id_list : _pi_length * 8; 
      _rg_container : 8 ; 
      _rg_length : 8 ;
      rg_id : 8 ; 
      rg_id_list : _rg_length * 8 * 2 
      |} 
      when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 && pi_id = 33 ->
        let json_to_driver = Printf.sprintf {|
          {
            "status" : "%s",
            "scalar" : "%s",
            "element" : "%s",
            "pi_list" : "%s",
            "rg_list" : "%s"
          }
        |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring pi_id_list)) (string_to_hex (string_of_bitstring rg_id_list)) in
        write_json_to_file "driver_oracle.json" json_to_driver ;
      wait_for_oracle_response "../oracle-response.txt"
    | {| algo : 16 : littleendian; 
          auth_seq : 16 : littleendian;
          status : 16 : littleendian ; 
          _group_id : 16 ;
          scalar : 256 : bitstring ; 
          element : 512 : bitstring ;
          _rg_container : 8 ; 
          _rg_length : 8 ;
          rg_id : 8 ; 
          rg_id_list : _rg_length * 8 * 2; 
          _ac_container : 8 ; 
          _ac_length : 8 ;
          ac_id : 8 ; 
          ac_id_list : _ac_length * 8 
          |} 
        when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 && ac_id = 93 ->
          let json_to_driver = Printf.sprintf {|
            {
            "status" : "%s",
              "scalar" : "%s",
              "element" : "%s",
              "rg_list" : "%s",
              "ac_list" : "%s"
            }
          |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring rg_id_list)) (string_to_hex (string_of_bitstring ac_id_list)) in
        write_json_to_file "driver_oracle.json" json_to_driver ;
        wait_for_oracle_response "../oracle-response.txt"
    | {| algo : 16 : littleendian; 
        auth_seq : 16 : littleendian;
        status : 16 : littleendian ; 
        _group_id : 16 ;
        scalar : 256 : bitstring ; 
        element : 512 : bitstring ;
        _pi_container : 8 ; 
        _pi_length : 8 ;
        pi_id : 8 ; 
        pi_id_list : _pi_length * 8; 
        _ac_container : 8 ; 
        _ac_length : 8 ;
        ac_id : 8 ; 
        ac_id_list : _ac_length * 8 
        |} 
      when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && pi_id = 33 && ac_id = 93 ->
        let json_to_driver = Printf.sprintf {|
        {
          "status" : "%s",
          "scalar" : "%s",
          "element" : "%s",
          "pi_list" : "%s",
          "ac_list" : "%s"
        }
      |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring pi_id_list)) (string_to_hex (string_of_bitstring ac_id_list)) in
      write_json_to_file "driver_oracle.json" json_to_driver ;
      wait_for_oracle_response "../oracle-response.txt"
  | {| algo : 16 : littleendian; 
      auth_seq : 16 : littleendian;
      status : 16 : littleendian ; 
      _group_id : 16 ;
      scalar : 256 : bitstring ; 
      element : 512 : bitstring ;
      _pi_container : 8 ; 
      _pi_length : 8 ;
      pi_id : 8 ; 
      pi_id_list : _pi_length * 8 
      |} 
    when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && pi_id = 33 ->
      let json_to_driver = Printf.sprintf {|
        {
          "status" : "%s",
          "scalar" : "%s",
          "element" : "%s",
          "pi_list" : "%s"
        }
      |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring pi_id_list)) in
      write_json_to_file "driver_oracle.json" json_to_driver ;
      wait_for_oracle_response "../oracle-response.txt"
    | {| algo : 16 : littleendian; 
      auth_seq : 16 : littleendian;
      status : 16 : littleendian ; 
      _group_id : 16 ;
      scalar : 256 : bitstring ; 
      element : 512 : bitstring ;
      _rg_container : 8 ; 
      _rg_length : 8 ;
      rg_id : 8 ; 
      rg_id_list : _rg_length * 8 * 2 
      |}
    when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 ->
      let json_to_driver = Printf.sprintf {|
      {
        "status" : "%s",
        "scalar" : "%s",
        "element" : "%s",
        "rg_list" : "%s"
      }
    |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring rg_id_list)) in
    write_json_to_file "driver_oracle.json" json_to_driver ;
    wait_for_oracle_response "../oracle-response.txt"
    | {| algo : 16 : littleendian; 
      auth_seq : 16 : littleendian;
      status : 16 : littleendian ; 
      _group_id : 16 ;
      scalar : 256 : bitstring ; 
      element : 512 : bitstring ;
      _ac_container : 8 ; 
      _ac_length : 8 ;
      ac_id : 8 ; 
      ac_id_list : _ac_length * 8 
      |} 
    when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && ac_id = 93 ->
      let json_to_driver = Printf.sprintf {|
      {
        "status" : "%s",
        "scalar" : "%s",
        "element" : "%s",
        "ac_list" : "%s"
      }
    |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring ac_id_list)) in
    write_json_to_file "driver_oracle.json" json_to_driver ;
    wait_for_oracle_response "../oracle-response.txt"
    
    | {| algo : 16 : littleendian; auth_seq : 16 : littleendian; status : 16 : littleendian ; _group_id : 16 ; ac_token : 256 : bitstring ; scalar : 256 : bitstring ; element : 512 : bitstring |}
    when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) ->  
      let json_to_driver = Printf.sprintf {|
      {
        "status" : "%s",
        "scalar" : "%s",
        "element" : "%s",
        "ac_token" : "%s"
      }
    |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) (string_to_hex (string_of_bitstring ac_token)) in
    write_json_to_file "driver_oracle.json" json_to_driver ;
    wait_for_oracle_response "../oracle-response.txt" 
  | {| algo : 16 : littleendian; auth_seq : 16 : littleendian; status : 16 : littleendian ; _group_id : 16 ; scalar : 256 : bitstring ; element : 512 : bitstring |}
    when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) ->
      let json_to_driver = Printf.sprintf {|
      {
        "status" : "%s",
        "scalar" : "%s",
        "element" : "%s"
      }
    |} (string_of_int status) (string_to_hex (string_of_bitstring scalar)) (string_to_hex (string_of_bitstring element)) in
    write_json_to_file "driver_oracle.json" json_to_driver ;
    wait_for_oracle_response "../oracle-response.txt"
  | {| _ |} -> IGNORE_
    

let get_bytes_and_run filename =
  let _bytes = load_bitstring_from_file filename in
  parse_packet _bytes