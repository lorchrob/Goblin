(* Resolve each attribute reference to the attribute it names, and report undeclared, 
   duplicate, clashing, and dot notation (`<L>.v`) references *)

module A = Ast

(* The attributes an unqualified reference can name within `owner`'s production rules,
   an inherited attribute taking precedence over a synthesized one *)
type scope = {
  owner : string;
  inh_attrs : string list;
  synth_attrs : string list;
}

(* `<L>.v` is rejected when `v` is an inherited attribute of `<L>`:
   the parent already knows the value, since it passes it to `<L>` *)
let check_synth_attr inh_attrs_of nt attr p =
  if List.mem attr (inh_attrs_of nt) then
    let msg = Format.asprintf
      "%s is an inherited attribute of <%a>, so it cannot be accessed with dot notation (<%a>.%s); use the expression passed to <%a> instead"
      attr Nt.pp nt Nt.pp nt attr Nt.pp nt
    in
    Utils.error msg p

(* `scope` is the nonterminal whose production rules contain `expr`
   (None for type annotations, which have no enclosing nonterminal) *)
let rec scope_expr check_synth scope expr =
  let r = scope_expr check_synth scope in
  match expr with
  | A.InhAttr (_, attr, p) ->
    (match scope with
    | Some { owner; inh_attrs; _ } when List.mem attr inh_attrs -> A.InhAttr (Some owner, attr, p)
    | Some { synth_attrs; _ } when List.mem attr synth_attrs -> A.OwnSynthAttr (attr, p)
    | Some { owner; _ } ->
      let msg = Format.asprintf
        "Unknown identifier %s (nonterminal <%s> neither declares it as an inherited attribute nor defines it as a synthesized attribute)" attr owner in
      Utils.error msg p
    | None ->
      let msg = Format.asprintf "Unknown identifier %s (attributes cannot be referenced in type annotations)" attr in
      Utils.error msg p)
  | SynthAttr (nt, attr, p) -> check_synth nt attr p; expr
  | EmptySet _ | BVConst _ | BLConst _ | BConst _ | IntConst _
  | PhConst _ | StrConst _ | NTExpr _ | OwnSynthAttr _ -> expr
  | Singleton (e, p) -> Singleton (r e, p)
  | BVCast (len, e, p) -> BVCast (len, r e, p)
  | BinOp (e1, op, e2, p) -> BinOp (r e1, op, r e2, p)
  | UnOp (op, e, p) -> UnOp (op, r e, p)
  | CompOp (e1, op, e2, p) -> CompOp (r e1, op, r e2, p)
  | BuiltInFunc (f, es, p) -> BuiltInFunc (f, List.map r es, p)
  | ActLit (e, p) -> ActLit (r e, p)

let scope_sc scope sc = match sc with
| A.SmtConstraint (e, p) -> A.SmtConstraint (scope e, p)
| DerivedField (nt, e, p) -> DerivedField (nt, scope e, p)
| AttrDef (attr, e, p) -> AttrDef (attr, scope e, p)

let scope_inh_attrs ast =
  let inh_attrs_of nt = List.concat_map (fun element -> match element with
  | A.ProdRule (nt2, ias, _, _) when Nt.equal nt2 nt -> List.map fst ias
  | A.ProdRule _ | TypeAnnotation _ -> []
  ) ast in
  let synth_attrs_of nt = List.concat_map (fun element -> match element with
  | A.ProdRule (nt2, _, rhss, _) when Nt.equal nt2 nt ->
    List.concat_map (fun sc -> match sc with 
    | A.AttrDef (attr, _, _) -> [attr] 
    | SmtConstraint _ | DerivedField _ -> [])
      (List.concat_map (function A.Rhs (_, scs, _, _) -> scs | A.StubbedRhs _ -> []) rhss)
  | A.ProdRule _ | TypeAnnotation _ -> []
  ) ast in
  let owner_of nt = match nt with
  | Nt.User owner -> owner
  | Nt.SynthAttr _ | InhAttr _ | Stub _ ->
    Utils.crash "Unexpected generated nonterminal before scoping inherited attributes"
  in
  (* Declarations are checked before any reference is resolved, so that every
     reference is resolved against a valid set of declarations *)
  List.iter (fun element -> match element with
  | A.TypeAnnotation _ -> ()
  | A.ProdRule (nt, ias, _, p) ->
    let owner = owner_of nt in
    let inh_attrs = List.map fst ias in
    if Utils.has_duplicate String.equal inh_attrs then (
      let msg = Format.asprintf "Nonterminal <%s> declares the same inherited attribute more than once" owner in
      Utils.error msg p
    );
    let synth_attrs = synth_attrs_of nt in
    (* A name declared as both would make unqualified references to it ambiguous *)
    (match List.find_opt (fun attr -> List.mem attr synth_attrs) inh_attrs with
    | Some attr ->
      let msg = Format.asprintf
        "Nonterminal <%s> declares %s as an inherited attribute and also defines it as a synthesized attribute" owner attr in
      Utils.error msg p
    | None -> ())
  ) ast;
  let check_synth = check_synth_attr inh_attrs_of in
  List.map (fun element -> match element with
  | A.ProdRule (nt, ias, rhss, p) ->
    let owner = owner_of nt in
    let inh_attrs = List.map fst ias in
    let scope = scope_expr check_synth (Some { owner; inh_attrs; synth_attrs = synth_attrs_of nt }) in
    let rhss = List.map (fun rhs -> match rhs with
    | A.StubbedRhs _ -> rhs
    | A.Rhs (ges, scs, prob, p) ->
      let ges = List.map (fun ge -> match ge with
      | A.StubbedNonterminal _ -> ge
      | A.Nonterminal (nt2, idx1, idx2, args, p) ->
        A.Nonterminal (nt2, idx1, idx2, List.map scope args, p)
      ) ges in
      A.Rhs (ges, List.map (scope_sc scope) scs, prob, p)
    ) rhss in
    A.ProdRule (nt, ias, rhss, p)
  | TypeAnnotation (nt, ty, scs, p) ->
    A.TypeAnnotation (nt, ty, List.map (scope_sc (scope_expr check_synth None)) scs, p)
  ) ast
