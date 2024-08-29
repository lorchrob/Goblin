open Bitstring
open Yojson.Basic

type state = NOTHING | CONFIRMED | ACCEPTED | IGNORE

let write_json_to_file filename json_data =
  let oc = open_out filename in
  output_string oc json_data;
  close_out oc


let parse_packet (packet : Bitstring.bitstring) : unit =
  let length_of_packet = bitstring_length packet in
  if length_of_packet = 96 then handle_length_confirm packet
  else if length_of_packet = 160 then handle_commit packet 
  else if length_of_packet = 192 then handle_ac_commit
  else if length_of_packet > 192 && length_of_packet < 

let handle_length_confirm packet =  
  match%bitstring packet with
  | {| algo : 16; auth_seq : 16; status : 16 ; send_confirm : 16 ; confirm_hash : 32 |}
    when algo = 3 && auth_seq = 2 && status = 0 ->
      let json_to_driver = Printf.sprintf {|
      {
        "send_confirm" : "%s",
        "confirm_hash" : "%s"
      }
    |} (string_of_bitstring send_confirm) (string_of_bitstring confirm_hash) in
    write_json_to_file "../../driver_oracle.json" json_to_driver

let handle_length_max_ac packet =
  match%bitstring packet with
  | {| algo : 16; 
  auth_seq : 16;
  status : 16 ; 
  group_id : 16 ;
  ac_token : 32 ;
  scalar : 32 ; 
  element : 64 ;
  pi_container : 8 ; 
  pi_length : 8 ;
  pi_id : 8 ; 
  pi_id_list : pi_length * 8 
  rg_container : 8 ; 
  rg_length : 8 ;
  rg_id : 8 ; 
  rg_id_list : rg_length * 8 * 2 
  ac_container : 8 ; 
  ac_length : 8 ;
  ac_id : 8 ; 
  ac_id_list : ac_length * 8 
  |} 
  when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 && pi_id = 33 && ac_id = 93 ->
    let json_to_driver = Printf.sprintf {|
      {
      "status" : "%s",
        "scalar" : "%s",
        "element" : "%s",
        "ac_token" : "%s"
      }
    |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) (string_of_bitstring ac_token) in
    write_json_to_file "../../driver_oracle.json" json_to_driver 

let handle_length_two_case packet =
  match%bitstring packet with
  | {| algo : 16; 
  auth_seq) : 16;
  status : 16 ; 
  group_id : 16 ;
  ac_token : 32 ;
  scalar : 32 ; 
  element : 64 ;
  pi_container : 8 ; 
  pi_length : 8 ;
  pi_id : 8 ; 
  pi_id_list : pi_length * 8 
  rg_container : 8 ; 
  rg_length : 8 ;
  rg_id : 8 ; 
  rg_id_list : rg_length * 8 * 2 
  |} 
  when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 && pi_id = 33 ->
    let json_to_driver = Printf.sprintf {|
      {
      "status" : "%s",
        "scalar" : "%s",
        "element" : "%s",
        "ac_token" : "%s"
      }
    |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) (string_of_bitstring ac_token) in
    write_json_to_file "../../driver_oracle.json" json_to_driver
  | {| algo : 16; 
    auth_seq : 16;
    status : 16 ; 
    group_id : 16 ;
    ac_token : 32 ;
    scalar : 32 ; 
    element : 64 ;
    rg_container : 8 ; 
    rg_length : 8 ;
    rg_id : 8 ; 
    rg_id_list : rg_length * 8 * 2 
    ac_container : 8 ; 
    ac_length : 8 ;
    ac_id : 8 ; 
    ac_id_list : ac_length * 8 
    |} 
    when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 && ac_id = 93 ->
      let json_to_driver = Printf.sprintf {|
        {
        "status" : "%s",
          "scalar" : "%s",
          "element" : "%s",
          "ac_token" : "%s"
        }
      |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) (string_of_bitstring ac_token) in
      write_json_to_file "../../driver_oracle.json" json_to_driver
  | {| algo : 16; 
      auth_seq : 16;
      status : 16 ; 
      group_id : 16 ;
      ac_token : 32 ;
      scalar : 32 ; 
      element : 64 ;
      pi_container : 8 ; 
      pi_length : 8 ;
      pi_id : 8 ; 
      pi_id_list : pi_length * 8 
      ac_container : 8 ; 
      ac_length : 8 ;
      ac_id : 8 ; 
      ac_id_list : ac_length * 8 
      |} 
    when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && pi_id = 33 && ac_id = 93 ->
      let json_to_driver = Printf.sprintf {|
        {
        "status" : "%s",
          "scalar" : "%s",
          "element" : "%s",
          "ac_token" : "%s"
        }
      |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) (string_of_bitstring ac_token) in
      write_json_to_file "../../driver_oracle.json" json_to_driver

let handle_length_one_ac packet =
  match%bitstring packet with
  | {| algo : 16; 
  auth_seq : 16;
  status : 16 ; 
  group_id : 16 ;
  ac_token : 32 ;
  scalar : 32 ; 
  element : 64 ;
  pi_container : 8 ; 
  pi_length : 8 ;
  pi_id : 8 ; 
  pi_id_list : pi_length * 8 
  |} 
  when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && pi_id = 33 ->
    let json_to_driver = Printf.sprintf {|
      {
      "status" : "%s",
        "scalar" : "%s",
        "element" : "%s",
        "ac_token" : "%s"
      }
    |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) (string_of_bitstring ac_token) in
    write_json_to_file "../../driver_oracle.json" json_to_driver
  | {| algo : 16; 
    auth_seq : 16;
    status : 16 ; 
    group_id : 16 ;
    ac_token : 32 ;
    scalar : 32 ; 
    element : 64 ;
    rg_container : 8 ; 
    rg_length : 8 ;
    rg_id : 8 ; 
    rg_id_list : rg_length * 8 * 2 
    |}
    when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 ->
      let json_to_driver = Printf.sprintf {|
        {
        "status" : "%s",
          "scalar" : "%s",
          "element" : "%s",
          "ac_token" : "%s"
        }
      |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) (string_of_bitstring ac_token) in
      write_json_to_file "../../driver_oracle.json" json_to_driver
  | {| algo : 16; 
    auth_seq : 16;
    status : 16 ; 
    group_id : 16 ;
    ac_token : 32 ;
    scalar : 32 ; 
    element : 64 ;
    ac_container : 8 ; 
    ac_length : 8 ;
    ac_id : 8 ; 
    ac_id_list : ac_length * 8 
    |} 
    when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 93 ->
      let json_to_driver = Printf.sprintf {|
        {
        "status" : "%s",
          "scalar" : "%s",
          "element" : "%s",
          "ac_token" : "%s"
        }
      |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) (string_of_bitstring ac_token) in
        write_json_to_file "../../driver_oracle.json" json_to_driver

let handle_length_max_no_ac packet =
  match%bitstring packet with
  | {| algo : 16; 
    auth_seq : 16;
    status : 16 ; 
    group_id : 16 ;
    scalar : 32 ; 
    element : 64 ;
    pi_container : 8 ; 
    pi_length : 8 ;
    pi_id : 8 ; 
    pi_id_list : pi_length * 8 
    rg_container : 8 ; 
    rg_length : 8 ;
    rg_id : 8 ; 
    rg_id_list : rg_length * 8 * 2 
    ac_container : 8 ; 
    ac_length : 8 ;
    ac_id : 8 ; 
    ac_id_list : ac_length * 8 
    |} 
  when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 && pi_id = 33 && ac_id = 93 ->
    let json_to_driver = Printf.sprintf {|
    {
      "status" : "%s",
      "scalar" : "%s",
      "element" : "%s"
    }
  |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) in
        write_json_to_file "../../driver_oracle.json" json_to_driver

let handle_length_two_case_no_ac packet =
  match%bitstring packet with
  | {| algo : 16; 
    auth_seq : 16;
    status : 16 ; 
    group_id : 16 ;
    scalar : 32 ; 
    element : 64 ;
    pi_container : 8 ; 
    pi_length : 8 ;
    pi_id : 8 ; 
    pi_id_list : pi_length * 8 
    rg_container : 8 ; 
    rg_length : 8 ;
    rg_id : 8 ; 
    rg_id_list : rg_length * 8 * 2 
    |} 
  when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 && pi_id = 33 ->
    let json_to_driver = Printf.sprintf {|
      {
        "status" : "%s",
        "scalar" : "%s",
        "element" : "%s"
      }
    |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) in
        write_json_to_file "../../driver_oracle.json" json_to_driver
  | {| algo : 16; 
        auth_seq : 16;
        status : 16 ; 
        group_id : 16 ;
        scalar : 32 ; 
        element : 64 ;
        rg_container : 8 ; 
        rg_length : 8 ;
        rg_id : 8 ; 
        rg_id_list : rg_length * 8 * 2 
        ac_container : 8 ; 
        ac_length : 8 ;
        ac_id : 8 ; 
        ac_id_list : ac_length * 8 
        |} 
      when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 && ac_id = 93 ->
        let json_to_driver = Printf.sprintf {|
          {
          "status" : "%s",
            "scalar" : "%s",
            "element" : "%s"
          }
        |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) in
      write_json_to_file "../../driver_oracle.json" json_to_driver
  | {| algo : 16; 
      auth_seq : 16;
      status : 16 ; 
      group_id : 16 ;
      scalar : 32 ; 
      element : 64 ;
      pi_container : 8 ; 
      pi_length : 8 ;
      pi_id : 8 ; 
      pi_id_list : pi_length * 8 
      ac_container : 8 ; 
      ac_length : 8 ;
      ac_id : 8 ; 
      ac_id_list : ac_length * 8 
      |} 
    when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && pi_id = 33 && ac_id = 93 ->
      let json_to_driver = Printf.sprintf {|
      {
        "status" : "%s",
        "scalar" : "%s",
        "element" : "%s"
      }
    |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) in
    write_json_to_file "../../driver_oracle.json" json_to_driver

let handle_length_one_no_ac packet =
  match%bitstring packet with
  | {| algo : 16; 
    auth_seq : 16;
    status : 16 ; 
    group_id : 16 ;
    scalar : 32 ; 
    element : 64 ;
    pi_container : 8 ; 
    pi_length : 8 ;
    pi_id : 8 ; 
    pi_id_list : pi_length * 8 
    |} 
  when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && pi_id = 33 ->
    let json_to_driver = Printf.sprintf {|
      {
        "status" : "%s",
        "scalar" : "%s",
        "element" : "%s"
      }
    |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) in
    write_json_to_file "../../driver_oracle.json" json_to_driver
  | {| algo : 16; 
    auth_seq : 16;
    status : 16 ; 
    group_id : 16 ;
    scalar : 32 ; 
    element : 64 ;
    rg_container : 8 ; 
    rg_length : 8 ;
    rg_id : 8 ; 
    rg_id_list : rg_length * 8 * 2 
    |}
  when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 92 ->
    let json_to_driver = Printf.sprintf {|
    {
      "status" : "%s",
      "scalar" : "%s",
      "element" : "%s"
    }
  |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) in
  write_json_to_file "../../driver_oracle.json" json_to_driver

  | {| algo : 16; 
    auth_seq : 16;
    status : 16 ; 
    group_id : 16 ;
    scalar : 32 ; 
    element : 64 ;
    ac_container : 8 ; 
    ac_length : 8 ;
    ac_id : 8 ; 
    ac_id_list : ac_length * 8 
    |} 
  when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) && rg_id = 93 ->
    let json_to_driver = Printf.sprintf {|
    {
      "status" : "%s",
      "scalar" : "%s",
      "element" : "%s"
    }
  |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) in
  write_json_to_file "../../driver_oracle.json" json_to_driver

let handle_ac_commit packet =
      match%bitstring packet with
      | {| algo : 16; auth_seq : 16; status : 16 ; group_id : 16 ; ac_token : 32 ; scalar : 32 ; element : 64 |}
      when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) ->  
        let json_to_driver = Printf.sprintf {|
        {
          "status" : "%s",
          "scalar" : "%s",
          "element" : "%s",
          "ac_token" : "%s"
        }
      |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) (string_of_bitstring ac_token) in
      write_json_to_file "../../driver_oracle.json" json_to_driver

let handle_commit packet =
      match%bitstring packet with
      | {| algo : 16; auth_seq : 16; status : 16 ; group_id : 16 ; scalar : 32 ; element : 64 |}
      when algo = 3 && auth_seq = 1 && (status = 0 || status = 126) ->
        let json_to_driver = Printf.sprintf {|
        {
          "status" : "%s",
          "scalar" : "%s",
          "element" : "%s"
        }
      |} (string_of_bitstring status) (string_of_bitstring scalar) (string_of_bitstring element) in
      write_json_to_file "../../driver_oracle.json" json_to_driver

    
