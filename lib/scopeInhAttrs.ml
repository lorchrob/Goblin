(* Scope inherited attributes to the nonterminal that declares them.

   Each reference `v` to an inherited attribute within `<L>`'s production rules
   (in semantic constraints, and in the arguments passed at call sites) is resolved
   to `InhAttr (Some "L", "v", _)`. This way, different nonterminals may declare inherited
   attributes with the same name but different types, and inherited attributes never clash with
   synthesized attributes.

   This pass also reports
     * duplicate inherited attribute declarations for the same nonterminal
     * references to inherited attributes that the enclosing nonterminal does not declare
       (including references within type annotations, which have no inherited attributes)
     * dot notation references `<L>.v` to an inherited attribute `v` of `<L>`

   Runs immediately after parsing, since later passes (syntax and type checking) need
   to know which nonterminal each inherited attribute belongs to. *)

module A = Ast

(* `<L>.v` is rejected when `v` is an inherited attribute of `<L>` (and not also a synthesized one):
   the parent already knows the value, since it passes it to `<L>` *)
let check_synth_attr inh_attrs_of synth_attrs_of nt attr p =
  if List.mem attr (inh_attrs_of nt) && not (List.mem attr (synth_attrs_of nt)) then
    let msg = Format.asprintf
      "%s is an inherited attribute of <%a>, so it cannot be accessed with dot notation (<%a>.%s); use the expression passed to <%a> instead"
      attr Nt.pp nt Nt.pp nt attr Nt.pp nt
    in
    Utils.error msg p

(* `owner` is the nonterminal whose production rules contain `expr`
   (None for type annotations), and `params` are its inherited attributes *)
let rec scope_expr check_synth owner params expr =
  let r = scope_expr check_synth owner params in
  match expr with
  | A.InhAttr (_, attr, p) ->
    (match owner with
    | Some owner when List.mem attr params -> A.InhAttr (Some owner, attr, p)
    | Some owner ->
      let msg = Format.asprintf "Inherited attribute %s is not declared by nonterminal <%s>" attr owner in
      Utils.error msg p
    | None ->
      let msg = Format.asprintf "Unknown identifier %s (inherited attributes cannot be referenced in type annotations)" attr in
      Utils.error msg p)
  | SynthAttr (nt, attr, p) -> check_synth nt attr p; expr
  | EmptySet _ | BVConst _ | BLConst _ | BConst _ | IntConst _
  | PhConst _ | StrConst _ | NTExpr _ -> expr
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
  | _ -> []
  ) ast in
  let synth_attrs_of nt = List.concat_map (fun element -> match element with
  | A.ProdRule (nt2, _, rhss, _) when Nt.equal nt2 nt ->
    List.concat_map (fun sc -> match sc with A.AttrDef (attr, _, _) -> [attr] | _ -> [])
      (List.concat_map (function A.Rhs (_, scs, _, _) -> scs | A.StubbedRhs _ -> []) rhss)
  | _ -> []
  ) ast in
  let check_synth = check_synth_attr inh_attrs_of synth_attrs_of in
  List.map (fun element -> match element with
  | A.ProdRule (nt, ias, rhss, p) ->
    let owner = match nt with
    | Nt.User owner -> owner
    | _ -> Utils.crash "Unexpected generated nonterminal before scoping inherited attributes"
    in
    let params = List.map fst ias in
    if Utils.has_duplicate String.equal params then (
      let msg = Format.asprintf "Nonterminal <%s> declares the same inherited attribute more than once" owner in
      Utils.error msg p
    );
    let scope = scope_expr check_synth (Some owner) params in
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
    A.TypeAnnotation (nt, ty, List.map (scope_sc (scope_expr check_synth None [])) scs, p)
  ) ast
