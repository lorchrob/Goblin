(* Function to flip a bit at a specific position in a byte *)
let flip_bit byte n =
  byte lxor (1 lsl n)

(* Function to flip random bits in a byte array *)
let flip_bits (data: bytes) (flip_count: int) : bytes =
  let len = Bytes.length data in
  let result = Bytes.copy data in
  for _ = 1 to flip_count do
    let byte_index = Random.int len in
    let bit_index = Random.int 8 in
    let byte = Bytes.get result byte_index |> Char.code in
    let flipped_byte = flip_bit byte bit_index in
    Bytes.set result byte_index (Char.chr flipped_byte)
  done;
  result

(* Function to flip whole bytes at random positions *)
let flip_bytes (data: bytes) (flip_count: int) : bytes =
  let len = Bytes.length data in
  let result = Bytes.copy data in
  for _ = 1 to flip_count do
    let byte_index = Random.int len in
    let new_byte = Random.int 256 in  (* Random byte between 0 and 255 *)
    Bytes.set result byte_index (Char.chr new_byte)
  done;
  result

(* Function to add/subtract a small value to/from a byte *)
let arithmetic_mutation (data: bytes) : bytes =
  let len = Bytes.length data in
  let result = Bytes.copy data in
  let byte_index = Random.int len in
  let byte = Bytes.get result byte_index |> Char.code in
  let delta = if Random.bool () then 1 else -1 in
  let new_byte = (byte + delta) mod 256 in
  Bytes.set result byte_index (Char.chr new_byte);
  result

(* Function to delete a random block of bytes *)
let delete_block (data: bytes) : bytes =
  let len = Bytes.length data in
  if len <= 2 then data  (* Can't delete block if too small *)
  else
    let block_size = Random.int (len / 4) + 1 in  (* Random block size up to 1/4th the size *)
    let start = Random.int (len - block_size) in
    let before = Bytes.sub data 0 start in
    let after = Bytes.sub data (start + block_size) (len - start - block_size) in
    Bytes.cat before after

(* Function to duplicate a random block of bytes *)
let duplicate_block (data: bytes) : bytes =
  let len = Bytes.length data in
  if len <= 2 then data  (* Can't duplicate block if too small *)
  else
    let block_size = Random.int (len / 4) + 1 in
    let start = Random.int (len - block_size) in
    let block = Bytes.sub data start block_size in
    let before = Bytes.sub data 0 start in
    let after = Bytes.sub data start (len - start) in
    Bytes.concat Bytes.empty [before; block; after]

(* Havoc function: Applies multiple random mutations to the input *)
let havoc (data: bytes) (num_mutations: int) : bytes =
  let result = ref (Bytes.copy data) in
  for _ = 1 to num_mutations do
    let mutation_type = Random.int 5 in
    result := (match mutation_type with
      | 0 -> flip_bits !result 1
      | 1 -> flip_bytes !result 1
      | 2 -> arithmetic_mutation !result
      | 3 -> delete_block !result
      | 4 -> duplicate_block !result
      | _ -> !result)
  done;
  !result

(* Main AFL-like mutation loop *)
let afl_mutate (data: bytes) (iterations: int) : unit =
  Random.self_init () ;
  for _ = 1 to iterations do
    let num_mutations = Random.int 10 + 1 in
    let mutated_data = havoc data num_mutations in
    (* Here you would test the mutated data on the target program *)
    (* For example: test_program mutated_data *)
    print_endline (Bytes.to_string mutated_data)  (* Just printing for now *)
  done

