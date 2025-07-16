module SA = SygusAst

let xml_grammar : (string * string list) list = [
  ("xml-tree0", ["rec-xml-tree"; "xml-openclose-tag0"]);

  ("rec-xml-tree0", ["xml-openclose-tag0"; "xml-open-tag0"; "inner-xml-tree0"; "xml-close-tag0"]);

  ("inner-xml-tree0", ["text0"; "rec-xml-tree0"; "inner-xml-tree0"; "inner-xml-tree0"]);

  ("xml-open-tag0", ["id0"; "xml-attribute0"]);

  ("xml-close-tag0", ["id0"]);

  ("xml-openclose-tag0", ["id0"; "xml-attribute0"]);

  ("xml-attribute0", ["id0"; "text0"; "xml-attribute-10"; "xml-attribute-20"]);

  ("xml-attribute-10", ["xml-attribute0"]);
  ("xml-attribute-20", ["xml-attribute0"]);

  ("id0", ["id-no-prefix-10"; "id-with-prefix0"]);

  ("id-with-prefix0", ["id-no-prefix-10"; "id-no-prefix-20"]);

  ("text0", []);
  ("id-no-prefix-10", []);
  ("id-no-prefix-20", []);
]

let csv_grammar : (string * string list) list = [
  ("csv-file0", ["csv-header0"; "csv-records0"]);
  ("csv-header0", ["csv-record0"]);
  ("csv-records0", ["csv-records0"; "csv-record0"; "nil0"]);
  ("csv-record0", ["raw-field0"; "csv-record0"]);
  ("raw-field0", []);
  ("nil0", []);
]

let count_paths (grammar : (string * string list) list) (k : int) : Utils.StringSet.t * int =
  let rec explore_paths symbol remaining current_path acc =
    let new_path = current_path @ [symbol] in
    if remaining = 1 then
      Utils.StringSet.add (String.concat "/" new_path) acc
    else
      match List.assq_opt symbol grammar with
      | Some children ->
          if children = [] then acc
          else
            List.fold_left
              (fun acc child -> explore_paths child (remaining - 1) new_path acc)
              acc
              children
      | None -> acc
  in

  let all_paths =
    List.fold_left
      (fun acc (symbol, _) -> explore_paths symbol k [] acc)
      Utils.StringSet.empty
      grammar
  in
  (all_paths, Utils.StringSet.cardinal all_paths)

let collect_k_paths k ast =
  let rec aux current_path acc = function
    | SA.Node ((label, _), children) ->
        if String.length label > 0 && label.[0] = '_' then acc
        else
          let label = Format.asprintf "%s" label in
          let new_path = current_path @ [label] in
          let acc = if List.length new_path = k then (
              if !Flags.debug then Format.printf "%s\n" (String.concat "/" new_path);
              Utils.StringSet.add (String.concat "/" new_path) acc
          ) else acc
          in
          let acc = List.fold_left (fun a child -> aux new_path a child) acc children in
          acc
    | _ -> acc
  in
  let rec walk_and_collect acc node =
    let acc = aux [] acc node in
    match node with
    | SA.Node (_, children) ->
        List.fold_left walk_and_collect acc children
    | _ -> acc
  in
  walk_and_collect Utils.StringSet.empty ast

let collect_k_paths_from_outputs k asts =
  List.fold_left (fun acc ast -> Utils.StringSet.union acc (collect_k_paths k ast)) Utils.StringSet.empty asts

(* CSV pretty printers *)
let pp_raw_field = function
  | SA.Node ((label, _), [SA.StrLeaf s]) when String.starts_with ~prefix:"raw-field" label -> s
  | _ -> assert false

let rec pp_csv_record = function
  | SA.Node ((label, _), children) when label = "csv-record" ->
      let fields = List.filter_map (function
        | SA.Node ((child_label, _), _) as n when String.starts_with ~prefix:"raw-field" child_label -> Some (pp_raw_field n)
        | SA.Node ((child_label, _), _) as nested when child_label = "csv-record" -> Some (pp_csv_record nested)
        | SA.Node ((child_label, _), _) when String.starts_with ~prefix:"_" child_label -> None 
        | _ -> assert false
      ) children in
      String.concat "," fields
  | _ -> assert false

let rec pp_csv_records = function
  | SA.Node ((label, _), children) when label = "csv-records" ->
      List.filter_map (function
        | SA.Node ((child_label, _), _) as rec_node when child_label = "csv-record" -> Some (pp_csv_record rec_node)
        | SA.Node ((child_label, _), _) as nested when child_label = "csv-records" -> Some (pp_csv_records nested)
        | SA.Node ((child_label, _), _) when child_label = "nil" -> None
        | SA.Node ((child_label, _), _) when String.starts_with ~prefix:"_" child_label -> None 
        | _ -> assert false
      ) children
      |> String.concat "\n"
  | _ -> assert false

let pp_csv_header = function
  | SA.Node ((label, _), children) when label = "csv-header" ->
      let fields = List.filter_map (function
        | SA.Node ((child_label, _), _) as nested when child_label = "csv-record" -> Some (pp_csv_record nested)
        | SA.Node ((child_label, _), _) when String.starts_with ~prefix:"_" child_label -> None 
        | _ -> assert false
      ) children in
      String.concat "," fields
  | _ -> assert false

let pp_csv_file = function
  | SA.Node ((label, _), children) when label = "csv-file" ->
      let header = List.find_map (function
        | SA.Node ((child_label, _), _) as h when child_label = "csv-header" -> Some (pp_csv_header h)
        | _ -> None
      ) children in
      let records = List.find_map (function
        | SA.Node ((child_label, _), _) as r when child_label = "csv-records" -> Some (pp_csv_records r)
        | _ -> None
      ) children in
      Format.asprintf "%s\n%s" (Option.get header) (Option.get records)
  | sa -> SA.pp_print_sygus_ast Format.std_formatter sa; assert false

(* XML pretty-printers *)
let pp_text = function
  | SA.StrLeaf s -> s
  | _ -> assert false

let pp_id = function
  | SA.Node ((label, _), children) when label = "id" -> (
      let children = List.filter (function
        | SA.Node ((child_label, _), _) -> not (String.length child_label > 0 && child_label.[0] = '_')
        | _ -> false
      ) children in
      match children with
      | [SA.Node ((child_label, _), [SA.StrLeaf s])] when child_label = "id-no-prefix-" ->
          s
      | [SA.Node ((child_label, _), id_parts)] when child_label = "id-with-prefix" -> (
          let part1 = List.find_map (function
            | SA.Node ((part_label, Some 10), [SA.StrLeaf s]) when part_label = "id-no-prefix-" -> Some s
            | _ -> None
          ) id_parts in
          let part2 = List.find_map (function
            | SA.Node ((part_label, Some 20), [SA.StrLeaf s]) when part_label = "id-no-prefix-" -> Some s
            | _ -> None
          ) id_parts in
          (Option.get part1) ^ ":" ^ (Option.get part2)
        )
      | [sa] -> SA.pp_print_sygus_ast Format.std_formatter sa; assert false
      | _ -> assert false
    )
  | _ -> assert false
 
let rec pp_xml_attribute = function
  | SA.Node ((label, _), children) when label = "xml-attribute" -> (
      let meaningful_children = List.filter (function
        | SA.Node ((child_label, _), _) -> not (String.length child_label > 0 && child_label.[0] = '_')
        | _ -> false
      ) children in
      match meaningful_children with
      | [ SA.Node ((child_label1, _), _) as id_node;
          SA.Node ((child_label2, _), [SA.StrLeaf s]) ]
        when child_label1 = "id" && child_label2 = "text" ->
          let name = pp_id id_node in
          Format.asprintf "%s=\"%s\"" name s
      | [ SA.Node ((child_label1, _), [attr1]);
          SA.Node ((child_label2, _), [attr2]) ]
        when child_label1 = "xml-attribute-" && child_label2 = "xml-attribute-" ->
          let str1 = pp_xml_attribute attr1 in
          let str2 = pp_xml_attribute attr2 in
          String.concat " " [str1; str2]
      | _ -> assert false
    )
  | _ -> assert false

let pp_xml_open_tag = function
  | SA.Node ((label, _), children) when label = "xml-open-tag" ->
      let tag = List.find_map (function
        | SA.Node ((child_label, _), _) as id_node when child_label = "id" ->
            Some (pp_id id_node)
        | _ -> None
      ) children in
      let attributes =
        List.filter_map (function
          | SA.Node ((child_label, _), _) as attr_node when child_label = "xml-attribute" ->
              Some (pp_xml_attribute attr_node)
          | _ -> None
        ) children
      in
      let attrs_str = if attributes = [] then "" else " " ^ String.concat " " attributes in
      Format.asprintf "<%s%s>" (Option.get tag) attrs_str
  | _ -> assert false

let pp_xml_close_tag = function
  | SA.Node ((label, _), children) when label = "xml-close-tag" ->
      let tag = List.find_map (function
        | SA.Node ((child_label, _), _) as id_node when child_label = "id" ->
            Some (pp_id id_node)
        | _ -> None
      ) children in
      Format.asprintf "</%s>" (Option.get tag)
  | _ -> assert false

let pp_xml_openclose_tag = function
  | SA.Node ((label, _), children) when label = "xml-openclose-tag" ->
      let tag = List.find_map (function
        | SA.Node ((child_label, _), _) as id_node when child_label = "id" ->
            Some (pp_id id_node)
        | _ -> None
      ) children in
      let attributes =
        List.filter_map (function
          | SA.Node ((child_label, _), _) as attr_node when child_label = "xml-attribute" ->
              Some (pp_xml_attribute attr_node)
          | _ -> None
        ) children
      in
      let attrs_str = if attributes = [] then "" else " " ^ String.concat " " attributes in
      Format.asprintf "<%s%s/>" (Option.get tag) attrs_str
  | _ -> assert false

let rec pp_inner_xml_tree = function
  | SA.Node ((label, _), children) when label = "inner-xml-tree" ->
      List.map (function
        | SA.Node ((child_label, _), [SA.StrLeaf s]) when child_label = "text" -> s
        | SA.Node ((child_label, _), _) as rec_tree when child_label = "rec-xml-tree" -> pp_rec_xml_tree rec_tree
        | SA.Node ((child_label, _), _) as inner when child_label = "inner-xml-tree" -> pp_inner_xml_tree inner
        | _ -> ""
      ) children
      |> String.concat ""
  | _ -> assert false

and pp_rec_xml_tree = function
  | SA.Node ((label, _), children) when label = "rec-xml-tree" ->
      begin match List.find_opt (function
        | SA.Node ((child_label, _), _) when child_label = "xml-openclose-tag" -> true
        | _ -> false
      ) children with
      | Some openclose_node ->
          pp_xml_openclose_tag openclose_node
      | None ->
          let open_tag = List.find_map (function
            | SA.Node ((child_label, _), _) as o when child_label = "xml-open-tag" -> Some (pp_xml_open_tag o)
            | _ -> None
          ) children in
          let inner_tree = List.find_map (function
            | SA.Node ((child_label, _), _) as i when child_label = "inner-xml-tree" -> Some (pp_inner_xml_tree i)
            | _ -> None
          ) children in
          let close_tag = List.find_map (function
            | SA.Node ((child_label, _), _) as c when child_label = "xml-close-tag" -> Some (pp_xml_close_tag c)
            | _ -> None
          ) children in
          Format.asprintf "%s%s%s" (Option.get open_tag) (Option.get inner_tree) (Option.get close_tag)
      end
  | _ -> assert false

let pp_xml_tree = function
  | SA.Node ((label, _), children) when label = "xml-tree" ->
      List.map (function
        | SA.Node ((child_label, _), _) as tag when child_label = "xml-openclose-tag" -> pp_xml_openclose_tag tag
        | SA.Node ((child_label, _), _) as rec_tree when child_label = "rec-xml-tree" -> pp_rec_xml_tree rec_tree
        | _ -> assert false
      ) children
      |> String.concat ""
  | _ -> assert false

let evaluate () =
  let filename =
    match !Flags.filename with
    | Some filename -> filename
    | None -> Utils.error "You must specify an input filename with --file <filename>"
  in
  let input_string = Utils.read_file filename in
  let sygus_ast = Parsing.parse_sygus input_string [] |> Result.get_ok in
  let sygus_asts =
    match sygus_ast with
    | SA.Node (_, children) -> children
    | _ -> assert false
  in

  let outputs = List.mapi (fun i ast ->
    let output =
      match !Flags.analysis with
      | "csv" -> pp_csv_file ast
      | "xml" -> pp_xml_tree ast
      | _ -> Utils.error "Unknown grammar for post-analysis mode"
    in
    if !Flags.debug then
      Format.fprintf Format.std_formatter "Output %d:\n%s\n" (i + 1) output;
    output
  ) sygus_asts in

  let total_length = List.fold_left (fun acc s -> acc + String.length s) 0 outputs in
  let count = List.length outputs in
  Format.printf "Number of instances produced: %d\n" count;
  let _ = if count > 0 then
    let avg_length = float_of_int total_length /. float_of_int count in
    Format.printf "Average output length: %.2f\n" avg_length
  else
    Format.printf "No outputs found.\n"
  in

  let k = 3 in
  let selected_grammar = match !Flags.analysis with
    | "csv" -> csv_grammar
    | "xml" -> xml_grammar
    | _ -> Utils.error "Unknown grammar for path counting"
  in
  let all_paths, path_count = count_paths selected_grammar k in
  if !Flags.debug then Format.printf "%s grammar paths of length %d: %d\n" !Flags.analysis k path_count;

  let observed_paths = collect_k_paths_from_outputs k sygus_asts in
  let observed_count = Utils.StringSet.cardinal observed_paths in

  (* Compute coverage percentage *)
  let coverage = if path_count = 0 then assert false 
                 else (float_of_int observed_count /. float_of_int path_count) *. 100.0
  in

  if !Flags.debug then Format.printf "Observed distinct %d-paths: %d\n" k observed_count;
  if !Flags.debug then Format.printf "Total distinct %d-paths: %d\n" k path_count;
  if !Flags.debug then Format.printf "All distinct %d-paths: %a\n" k 
    (Lib.pp_print_list Format.pp_print_string ", ") (Utils.StringSet.to_list all_paths); 
  if !Flags.debug then Format.printf "Observed istinct %d-paths: %a\n" k 
    (Lib.pp_print_list Format.pp_print_string ", ") (Utils.StringSet.to_list observed_paths); 
  Format.printf "Coverage: %.2f%%\n" coverage
