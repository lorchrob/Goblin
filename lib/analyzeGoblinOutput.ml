open Printf

module SA = SygusAst

(* CSV pretty-printers *)
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
      sprintf "%s\n%s" (Option.get header) (Option.get records)
  | sa -> SA.pp_print_sygus_ast Format.std_formatter sa; assert false

(* XML pretty-printers *)
let pp_text = function
  | SA.StrLeaf s -> s
  | _ -> assert false

let pp_id = function
  | SA.Node ((label, _), [SA.StrLeaf s]) when label = "id-no-prefix-1" -> s
  | _ -> assert false

let pp_xml_attribute = function
  | SA.Node ((label, _), children) when label = "xml-attribute" ->
      let name = List.find_map (function
        | SA.Node ((child_label, _), id_children) when child_label = "id" -> 
            Some (pp_id (List.find (function SA.Node ((grand_label, _), _) -> grand_label = "id-no-prefix-1" | _ -> false) id_children))
        | _ -> None
      ) children in
      let value = List.find_map (function
        | SA.Node ((child_label, _), [SA.StrLeaf s]) when child_label = "text" -> Some s
        | _ -> None
      ) children in
      sprintf "%s=\"%s\"" (Option.get name) (Option.get value)
  | _ -> assert false

let pp_xml_open_tag = function
  | SA.Node ((label, _), children) when label = "xml-open-tag" ->
      let tag = List.find_map (function
        | SA.Node ((child_label, _), id_children) when child_label = "id" -> 
            Some (pp_id (List.find (function SA.Node ((grand_label, _), _) -> grand_label = "id-no-prefix-1" | _ -> false) id_children))
        | _ -> None
      ) children in
      let attributes =
        List.filter_map (function
          | SA.Node ((child_label, _), _) as attr when child_label = "xml-attribute" -> Some (pp_xml_attribute attr)
          | _ -> None
        ) children
      in
      let attrs_str = if attributes = [] then "" else " " ^ String.concat " " attributes in
      sprintf "<%s%s>" (Option.get tag) attrs_str
  | _ -> assert false

let pp_xml_close_tag = function
  | SA.Node ((label, _), children) when label = "xml-close-tag" ->
      let tag = List.find_map (function
        | SA.Node ((child_label, _), id_children) when child_label = "id" -> 
            Some (pp_id (List.find (function SA.Node ((grand_label, _), _) -> grand_label = "id-no-prefix-1" | _ -> false) id_children))
        | _ -> None
      ) children in
      sprintf "</%s>" (Option.get tag)
  | _ -> assert false

let pp_xml_openclose_tag = function
  | SA.Node ((label, _), children) when label = "xml-openclose-tag" ->
      let tag = List.find_map (function
        | SA.Node ((child_label, _), id_children) when child_label = "id" -> 
            Some (pp_id (List.find (function SA.Node ((grand_label, _), _) -> grand_label = "id-no-prefix-1" | _ -> false) id_children))
        | _ -> None
      ) children in
      let attributes =
        List.filter_map (function
          | SA.Node ((child_label, _), _) as attr when child_label = "xml-attribute" -> Some (pp_xml_attribute attr)
          | _ -> None
        ) children
      in
      let attrs_str = if attributes = [] then "" else " " ^ String.concat " " attributes in
      sprintf "<%s%s/>" (Option.get tag) attrs_str
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
          Printf.sprintf "%s%s%s" (Option.get open_tag) (Option.get inner_tree) (Option.get close_tag)
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
  let filename = match !Flags.filename with 
  | Some filename -> filename 
  | None -> Utils.error "You must specify an input filename with --file <filename>"
  in
  let input_string = Utils.read_file filename in 
  let sygus_ast = Parsing.parse_sygus input_string [] |> Result.get_ok in   
  let sygus_asts = match sygus_ast with 
  | SA.Node (_, children) -> children 
  | _ -> assert false 
  in

  List.iteri (fun i ast ->
    Printf.printf "Output %d:\n" (i+1);
    (match !Flags.analysis with 
     | "csv" ->
         let csv = pp_csv_file ast in
         Printf.printf "%s\n" csv
     | "xml" ->
         let xml = pp_xml_tree ast in
         Printf.printf "%s\n" xml
     | _ -> Utils.error "Unknown grammar for post-analysis mode")
  ) sygus_asts

