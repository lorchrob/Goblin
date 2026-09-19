module A = Ast
module SA = SolverAst

(* A dot-notation path referencing a syntactic category absent from the term
   denotes Top, which propagates through every operator and satisfies any
   constraint. This is what makes a separate applicability check unnecessary. *)
type v =
| Top
| Val of Value.t

type error =
(* Construct the evaluator does not implement; the verdict is Unknown, not a failure *)
| Unsupported of string * Lexing.position
(* Value is not determined by the semantics, e.g. division by zero in SMT-LIB *)
| Unspecified of string * Lexing.position
| Type_error of string * Lexing.position
| Internal of string * Lexing.position

let pp_error: Format.formatter -> error -> unit
= fun ppf -> function
| Unsupported (msg, _) -> Format.fprintf ppf "not supported by the evaluator: %s" msg
| Unspecified (msg, _) -> Format.fprintf ppf "value is unspecified: %s" msg
| Type_error (msg, _) -> Format.fprintf ppf "type error: %s" msg
| Internal (msg, _) -> Format.fprintf ppf "internal evaluator error: %s" msg

let pos_of_error = function
| Unsupported (_, p) | Unspecified (_, p) | Type_error (_, p) | Internal (_, p) -> p

type env = {
  (* The term the constraint under evaluation is attached to; dot-notation paths
     are resolved from here *)
  node : SA.solver_ast;
  (* Derived fields still held as stubs, computed on demand when a path reaches one *)
  deps : A.semantic_constraint Nt.StubMap.t;
  (* Whether this is a type annotation's own constraint, whose paths name the
     annotated node; inside a rule it would alias a recursive nonterminal to itself *)
  at_annotation : bool;
}

let (let*) = Res.(>>=)

(* Ast.pp_print_bin_op crashes on the set operators, and this is an error path *)
let binop_name: A.bin_operator -> string = function
| BVAnd -> "bvand" | BVOr -> "bvor" | BVXor -> "bvxor"
| LAnd | GLAnd -> "and" | LOr -> "or" | LXor -> "xor" | LImplies -> "=>"
| Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "div" | Mod -> "mod"
| StrConcat -> "str.++"
| SetUnion -> "set.union" | SetIntersection -> "set.inter"
| SetMembership -> "set.member"

let compop_name: A.comp_operator -> string = function
| Lt -> "<" | Lte -> "<=" | Gt -> ">" | Gte -> ">=" | Eq -> "="
| BVLt -> "bvult" | BVLte -> "bvulte" | BVGt -> "bvugt" | BVGte -> "bvugte"
| StrPrefix -> "str.prefixof" | StrContains -> "str.contains"

let type_error op operands p =
  let operands = List.map (fun v -> match v with
  | Top -> "Top"
  | Val value -> Format.asprintf "%a" Value.pp value
  ) operands in
  Error (Type_error (Format.asprintf "%s applied to %s" op (String.concat ", " operands), p))

(* Widths above this cannot be converted to a native int without silently wrapping *)
let max_int_bits = 62

let int_of_bits signed bits p =
  let n = List.length bits in
  if n > max_int_bits then
    Error (Unsupported (Format.asprintf "bit vector of width %d exceeds the evaluator's integer range" n, p))
  else
    let unsigned = List.fold_left (fun acc b -> (acc lsl 1) lor (if b then 1 else 0)) 0 bits in
    if signed && n > 0 && List.hd bits then Ok (unsigned - (1 lsl n)) else Ok unsigned

(* Unsigned comparison, most significant bit first *)
let rec bvult bits1 bits2 = match bits1, bits2 with
| [], [] -> false
| b1 :: t1, b2 :: t2 -> if b1 = b2 then bvult t1 t2 else b1 < b2
| _ :: _, [] | [], _ :: _ -> false

let equal_width bits1 bits2 = List.length bits1 = List.length bits2

let string_contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i = i + n <= h && (String.sub haystack i n = needle || at (i + 1)) in
  n = 0 || at 0

(* Finds the child named by one step of a dot-notation path. Occurrence indices
   disambiguate repeated names in the same production rule. *)
let child_for_step: SA.solver_ast -> Nt.t * int option * int option -> SA.solver_ast option
= fun node (id, _, occurrence) -> match node with
| Node (_, children) ->
  let named = List.filter (fun child -> match child with
  | SA.Node ((label, _, _), _) -> Nt.equal_ci (Nt.unstub id) (Nt.unstub label)
  | Leaf _ | StubLeaf _ | Model _ | Infeasible -> false
  ) children in
  (match occurrence with
  | None -> List.nth_opt named 0
  | Some i -> List.nth_opt named i)
| Leaf _ | StubLeaf _ | Model _ | Infeasible -> None

(* Every leaf value beneath a term, left to right. Derived fields are computed on
   demand, since an uncomputed stub would otherwise contribute nothing to `length`. *)
let rec leaves: env -> SA.solver_ast -> (Value.t list, error) result
= fun env solver_ast -> match solver_ast with
| Leaf value -> Ok [value]
| Node (_, children) ->
  let* valuess = Res.seq (List.map (fun child ->
    match child with
    (* Matched here rather than on recursion, because a derived field's definition
       is evaluated at the stub's parent *)
    | SA.Node (_, [SA.StubLeaf stub]) | SA.StubLeaf stub when Nt.StubMap.mem stub env.deps ->
      let* value = compute_stub { env with node = solver_ast } stub in
      Ok [value]
    | SA.Node _ | SA.Leaf _ | SA.StubLeaf _ | SA.Model _ | SA.Infeasible ->
      leaves { env with node = solver_ast } child
  ) children) in
  Ok (List.concat valuess)
| StubLeaf stub ->
  Error (Internal (Format.asprintf "uncomputed derived field '%a'" Nt.pp (Stub stub),
    Lexing.dummy_pos))
| Model _ -> Error (Internal ("SMT model where a term was expected", Lexing.dummy_pos))
| Infeasible -> Error (Internal ("infeasible where a term was expected", Lexing.dummy_pos))

(* Computes the derived field a stub stands for. [env.node] must be the stub's
   parent: that is the rule the definition is written against. *)
and compute_stub: env -> Nt.stub -> (Value.t, error) result
= fun env stub ->
  match Nt.StubMap.find_opt stub env.deps with
  | None ->
    Error (Internal (Format.asprintf "hanging identifier '%a'" Nt.pp (Stub stub), Lexing.dummy_pos))
  | Some (A.SmtConstraint (_, p)) | Some (A.AttrDef (_, _, p)) ->
    Error (Internal (Format.asprintf "'%a' is not a derived field" Nt.pp (Stub stub), p))
  | Some (A.DerivedField (_, expr, p)) ->
    (* Dropping the stub keeps a self-referential definition from looping *)
    let* v = eval { env with deps = Nt.StubMap.remove stub env.deps } expr in
    match v with
    | Val value -> Ok value
    | Top -> Error (Internal (Format.asprintf "derived field '%a' denotes Top" Nt.pp (Stub stub), p))

and eval: env -> A.expr -> (v, error) result
= fun env expr ->
  let p = A.pos_of_expr expr in
  let* values = eval_leaves env expr in
  match values with
  | [Top] -> Ok Top
  | [Val value] -> Ok (Val value)
  | _ ->
    Error (Internal (Format.asprintf "%a denotes %d values where one was expected"
      A.pp_print_expr expr (List.length values), p))

(* Expressions denote a single value except under `length`, whose argument may be
   a whole subtree; this is the only place where a list is meaningful. *)
and eval_leaves: env -> A.expr -> (v list, error) result
= fun env expr ->
  let p = A.pos_of_expr expr in
  let scalar v = Ok [v] in
  match expr with
  | NTExpr (path, _) -> (
    let* resolved = resolve env path in
    match resolved with
    | None -> scalar Top
    | Some (Node (_, [SA.Leaf (Value.Placeholder _)])) ->
      Error (Unsupported ("symbolic placeholder value", p))
    | Some node ->
      let* values = leaves { env with node } node in
      match values with
      | [] -> scalar Top
      | values -> Ok (List.map (fun value -> Val value) values)
  )
  | IntConst (i, _) -> scalar (Val (Value.Int i))
  | BConst (b, _) -> scalar (Val (Value.Bool b))
  | StrConst (s, _) -> scalar (Val (Value.String s))
  | PhConst (s, _) -> scalar (Val (Value.Placeholder s))
  | BVConst (width, bits, _) -> scalar (Val (Value.BitVector (width, bits)))
  | BLConst (bits, _) -> scalar (Val (Value.BitList bits))
  | EmptySet (String, _) -> scalar (Val (Value.StringSet Utils.StringSet.empty))
  | EmptySet (ty, _) ->
    Error (Unsupported (Format.asprintf "empty set of type %a" A.pp_print_ty ty, p))
  | Singleton (expr, _) -> (
    let* v = eval env expr in
    match v with
    | Top -> scalar Top
    | Val (Value.String s) -> scalar (Val (Value.StringSet (Utils.StringSet.singleton s)))
    | Val _ -> type_error "singleton" [v] p
  )
  | UnOp (op, expr, _) -> (
    let* v = eval env expr in
    match v, op with
    | Top, (UPlus | UMinus | LNot | BVNot) -> scalar Top
    | Val (Value.Int i), UPlus -> scalar (Val (Value.Int i))
    | Val (Value.Int i), UMinus -> scalar (Val (Value.Int (-i)))
    | Val (Value.Bool b), LNot -> scalar (Val (Value.Bool (not b)))
    | Val (Value.BitVector (width, bits)), BVNot ->
      scalar (Val (Value.BitVector (width, List.map not bits)))
    | Val _, UPlus -> type_error "unary +" [v] p
    | Val _, UMinus -> type_error "unary -" [v] p
    | Val _, LNot -> type_error "not" [v] p
    | Val _, BVNot -> type_error "bvnot" [v] p
  )
  | BinOp (expr1, op, expr2, _) ->
    let* v1 = eval env expr1 in
    let* v2 = eval env expr2 in
    let* v = eval_binop op v1 v2 p in
    scalar v
  | CompOp (expr1, op, expr2, _) ->
    let* v1 = eval env expr1 in
    let* v2 = eval env expr2 in
    let* v = eval_compop op v1 v2 p in
    scalar v
  | BVCast (width, expr, _) -> (
    let* v = eval env expr in
    match v with
    | Top -> scalar Top
    | Val (Value.Int i) -> (
      match A.il_int_to_bv width i p with
      | A.BVConst (width, bits, _) -> scalar (Val (Value.BitVector (width, bits)))
      | e -> Error (Internal (Format.asprintf "int_to_bv produced %a" A.pp_print_expr e, p))
    )
    | Val _ -> type_error "int_to_bv" [v] p
  )
  | BuiltInFunc (Length, [arg], _) ->
    let* values = eval_leaves env arg in
    let* bits = bit_width_of values p in
    scalar bits
  | BuiltInFunc (StrLength, [arg], _) -> (
    let* v = eval env arg in
    match v with
    | Top -> scalar Top
    | Val (Value.String s) -> scalar (Val (Value.Int (String.length s)))
    | Val _ -> type_error "str.len" [v] p
  )
  | BuiltInFunc (SeqLength, [arg], _) -> (
    let* v = eval env arg in
    match v with
    | Top -> scalar Top
    | Val (Value.BitList bits) -> scalar (Val (Value.Int (List.length bits)))
    | Val _ -> type_error "seq.len" [v] p
  )
  | BuiltInFunc (UbvToInt, [arg], _) -> (
    let* v = eval env arg in
    match v with
    | Top -> scalar Top
    | Val (Value.BitVector (_, bits)) ->
      let* i = int_of_bits false bits p in scalar (Val (Value.Int i))
    | Val _ -> type_error "ubv_to_int" [v] p
  )
  | BuiltInFunc (SbvToInt, [arg], _) -> (
    let* v = eval env arg in
    match v with
    | Top -> scalar Top
    | Val (Value.BitVector (_, bits)) ->
      let* i = int_of_bits true bits p in scalar (Val (Value.Int i))
    | Val _ -> type_error "sbv_to_int" [v] p
  )
  | BuiltInFunc (Repeat, [count; elt], _) -> (
    let* v1 = eval env count in
    let* v2 = eval env elt in
    match v1, v2 with
    | Top, _ | _, Top -> scalar Top
    | Val (Value.Int n), Val (Value.Bool b) -> scalar (Val (Value.BitList (Utils.replicate b n)))
    | _, _ -> type_error "repeat" [v1; v2] p
  )
  | BuiltInFunc ((Length | StrLength | SeqLength | UbvToInt | SbvToInt | Repeat) as f, args, _) ->
    Error (Internal (Format.asprintf "%a applied to %d arguments"
      A.pp_print_builtin_func f (List.length args), p))
  | BuiltInFunc (StrInRe, [subject; regex], _) -> (
    let* v = eval env subject in
    let* r = eval_regex env regex in
    match v, r with
    | Top, _ | _, None -> scalar Top
    | Val (Value.String str), Some r ->
      scalar (Val (Value.Bool (Re.execp (Re.compile (Re.whole_string r)) str)))
    | Val _, Some _ -> type_error "str.in_re" [v] p
  )
  | BuiltInFunc (StrInRe, args, _) ->
    Error (Internal (Format.asprintf "str.in_re applied to %d arguments" (List.length args), p))
  (* A regular expression is not a value, so it cannot stand where one is expected *)
  | BuiltInFunc ((ReUnion | ReRange | ReStar | ReConcat | StrToRe) as f, _, _) ->
    Error (Type_error (Format.asprintf "%a is a regular expression, not a value"
      A.pp_print_builtin_func f, p))
  (* An attribute's value lives in a generated child node of the nonterminal it
     belongs to, so every attribute reference is a path to one of those *)
  | SynthAttr (nt, attr, p) -> attribute_value env [nt] (Nt.SynthAttr attr) attr p
  | OwnSynthAttr (attr, p) -> attribute_value env [] (Nt.SynthAttr attr) attr p
  | InhAttr (Some owner, attr, p) ->
    attribute_value env [] (Nt.InhAttr (owner, attr)) attr p
  | InhAttr (None, attr, p) ->
    Error (Internal (Format.asprintf "unscoped inherited attribute %s" attr, p))
  (* Activation literals are introduced by the engine (ActLits, Dpll) after the
     checker's grammar has been built, so one cannot appear here *)
  | ActLit _ -> Error (Internal ("activation literal in a checked grammar", p))

(* The desugaring gives every attribute a node, so a missing one under a present
   owner is a defect; an absent owner is Top as usual *)
and attribute_value env prefix child attr p =
  let* owner = resolve env prefix in
  match owner with
  | None -> Ok [Top]
  | Some owner ->
    match child_for_step owner (child, None, None) with
    | None ->
      Error (Internal (Format.asprintf "attribute %s has no node under %a" attr
        SA.pp_print_solver_ast owner, p))
    | Some node ->
      let* values = leaves { env with node } node in
      match values with
      | [] -> Ok [Top]
      | values -> Ok (List.map (fun value -> Val value) values)

(* Regular expressions are their own syntactic category, never a term value, and
   None denotes Top. Departs from SMT-LIB in matching bytes, not code points. *)
and eval_regex: env -> A.expr -> (Re.t option, error) result
= fun env expr ->
  let p = A.pos_of_expr expr in
  let combine build args =
    let* rs = Res.seq (List.map (eval_regex env) args) in
    if List.exists Option.is_none rs then Ok None
    else Ok (Some (build (List.map Option.get rs)))
  in
  match expr with
  | BuiltInFunc (StrToRe, [arg], _) -> (
    let* v = eval env arg in
    match v with
    | Top -> Ok None
    | Val (Value.String str) -> Ok (Some (Re.str str))
    | Val _ -> type_error "str.to_re" [v] p
  )
  | BuiltInFunc (ReConcat, args, _) -> combine Re.seq args
  | BuiltInFunc (ReUnion, args, _) -> combine Re.alt args
  | BuiltInFunc (ReStar, [arg], _) -> (
    let* r = eval_regex env arg in
    match r with
    | None -> Ok None
    | Some r -> Ok (Some (Re.rep r))
  )
  | BuiltInFunc (ReRange, [lo; hi], _) -> (
    let* v1 = eval env lo in
    let* v2 = eval env hi in
    match v1, v2 with
    | Top, _ | _, Top -> Ok None
    | Val (Value.String lo), Val (Value.String hi) ->
      (* In SMT-LIB re.range denotes the empty language unless both arguments are
         single characters, and likewise when the range runs backwards *)
      if String.length lo = 1 && String.length hi = 1 && lo <= hi
      then Ok (Some (Re.rg lo.[0] hi.[0]))
      else Ok (Some Re.empty)
    | Val _, Val _ -> type_error "re.range" [v1; v2] p
  )
  | BuiltInFunc ((StrToRe | ReStar | ReRange) as f, args, _) ->
    Error (Internal (Format.asprintf "%a applied to %d arguments"
      A.pp_print_builtin_func f (List.length args), p))
  | BuiltInFunc ((Length | StrLength | SeqLength | UbvToInt | SbvToInt | Repeat | StrInRe), _, _)
  | NTExpr _ | InhAttr _ | SynthAttr _ | OwnSynthAttr _ | EmptySet _ | Singleton _
  | BinOp _ | UnOp _ | CompOp _ | BVCast _ | BVConst _ | BLConst _ | BConst _
  | IntConst _ | PhConst _ | StrConst _ | ActLit _ ->
    Error (Type_error (Format.asprintf "%a is not a regular expression" A.pp_print_expr expr, p))

and eval_binop: A.bin_operator -> v -> v -> Lexing.position -> (v, error) result
= fun op v1 v2 p ->
  let bool b = Ok (Val (Value.Bool b)) in
  let int i = Ok (Val (Value.Int i)) in
  match v1, v2 with
  | Top, _ | _, Top -> Ok Top
  | Val value1, Val value2 -> (
    match op, value1, value2 with
    | (LAnd | GLAnd), Value.Bool b1, Value.Bool b2 -> bool (b1 && b2)
    | LOr, Value.Bool b1, Value.Bool b2 -> bool (b1 || b2)
    | LXor, Value.Bool b1, Value.Bool b2 -> bool (b1 <> b2)
    | LImplies, Value.Bool b1, Value.Bool b2 -> bool ((not b1) || b2)
    | Plus, Value.Int i1, Value.Int i2 -> int (i1 + i2)
    | Minus, Value.Int i1, Value.Int i2 -> int (i1 - i2)
    | Times, Value.Int i1, Value.Int i2 -> int (i1 * i2)
    (* SMT-LIB leaves division and modulo by zero uninterpreted, so the solver may
       have chosen any value and the evaluator cannot reproduce it *)
    | Div, Value.Int _, Value.Int 0 -> Error (Unspecified ("division by zero", p))
    | Mod, Value.Int _, Value.Int 0 -> Error (Unspecified ("modulo by zero", p))
    | Div, Value.Int i1, Value.Int i2 -> int (i1 / i2)
    | Mod, Value.Int i1, Value.Int i2 -> int (i1 mod i2)
    | BVAnd, Value.BitVector (w, b1), Value.BitVector (_, b2) when equal_width b1 b2 ->
      Ok (Val (Value.BitVector (w, List.map2 (&&) b1 b2)))
    | BVOr, Value.BitVector (w, b1), Value.BitVector (_, b2) when equal_width b1 b2 ->
      Ok (Val (Value.BitVector (w, List.map2 (||) b1 b2)))
    | BVXor, Value.BitVector (w, b1), Value.BitVector (_, b2) when equal_width b1 b2 ->
      Ok (Val (Value.BitVector (w, List.map2 (<>) b1 b2)))
    | StrConcat, Value.String s1, Value.String s2 -> Ok (Val (Value.String (s1 ^ s2)))
    | SetUnion, Value.StringSet s1, Value.StringSet s2 ->
      Ok (Val (Value.StringSet (Utils.StringSet.union s1 s2)))
    | SetIntersection, Value.StringSet s1, Value.StringSet s2 ->
      Ok (Val (Value.StringSet (Utils.StringSet.inter s1 s2)))
    | SetMembership, Value.String s, Value.StringSet set ->
      bool (Utils.StringSet.mem s set)
    | (LAnd | GLAnd | LOr | LXor | LImplies | Plus | Minus | Times | Div | Mod
      | BVAnd | BVOr | BVXor | StrConcat | SetUnion | SetIntersection | SetMembership), _, _ ->
      type_error (binop_name op) [v1; v2] p
  )

and eval_compop: A.comp_operator -> v -> v -> Lexing.position -> (v, error) result
= fun op v1 v2 p ->
  let bool b = Ok (Val (Value.Bool b)) in
  match v1, v2 with
  | Top, _ | _, Top -> Ok Top
  | Val value1, Val value2 -> (
    match op, value1, value2 with
    | Lt, Value.Int i1, Value.Int i2 -> bool (i1 < i2)
    | Lte, Value.Int i1, Value.Int i2 -> bool (i1 <= i2)
    | Gt, Value.Int i1, Value.Int i2 -> bool (i1 > i2)
    | Gte, Value.Int i1, Value.Int i2 -> bool (i1 >= i2)
    | Eq, Value.Int i1, Value.Int i2 -> bool (i1 = i2)
    | Eq, Value.Bool b1, Value.Bool b2 -> bool (b1 = b2)
    | Eq, Value.String s1, Value.String s2 -> bool (String.equal s1 s2)
    | Eq, Value.Placeholder s1, Value.Placeholder s2 -> bool (String.equal s1 s2)
    | Eq, Value.BitVector (_, b1), Value.BitVector (_, b2) -> bool (b1 = b2)
    | Eq, Value.BitList b1, Value.BitList b2 -> bool (b1 = b2)
    | Eq, Value.StringSet s1, Value.StringSet s2 -> bool (Utils.StringSet.equal s1 s2)
    | Eq, Value.Unit, Value.Unit -> bool true
    | BVLt, Value.BitVector (_, b1), Value.BitVector (_, b2) when equal_width b1 b2 ->
      bool (bvult b1 b2)
    | BVLte, Value.BitVector (_, b1), Value.BitVector (_, b2) when equal_width b1 b2 ->
      bool (bvult b1 b2 || b1 = b2)
    | BVGt, Value.BitVector (_, b1), Value.BitVector (_, b2) when equal_width b1 b2 ->
      bool (bvult b2 b1)
    | BVGte, Value.BitVector (_, b1), Value.BitVector (_, b2) when equal_width b1 b2 ->
      bool (bvult b2 b1 || b1 = b2)
    | StrPrefix, Value.String s1, Value.String s2 ->
      let len1 = String.length s1 in
      bool (len1 <= String.length s2 && String.sub s2 0 len1 = s1)
    | StrContains, Value.String s1, Value.String s2 ->
      bool (string_contains s1 s2)
    | (Lt | Lte | Gt | Gte | Eq | BVLt | BVLte | BVGt | BVGte | StrPrefix | StrContains), _, _ ->
      type_error (compop_name op) [v1; v2] p
  )

(* Total width in bits of the values a `length` argument denotes *)
and bit_width_of: v list -> Lexing.position -> (v, error) result
= fun values p ->
  List.fold_left (fun acc v ->
    let* acc = acc in
    match acc, v with
    | Top, _ | _, Top -> Ok Top
    | Val (Value.Int total), Val value -> (
      match value with
      (* The bits actually held, so the width is right whatever the width field says *)
      | Value.BitVector (_, bits) -> Ok (Val (Value.Int (total + List.length bits)))
      | Value.BitList bits -> Ok (Val (Value.Int (total + List.length bits)))
      | Value.Bool _ -> Ok (Val (Value.Int (total + 1)))
      | Value.String s -> Ok (Val (Value.Int (total + 8 * String.length s)))
      (* A mathematical integer has no width, and a placeholder's is unknown *)
      | Value.Int _ -> Error (Type_error ("length of an Int value", p))
      | Value.Placeholder s ->
        Error (Unsupported (Format.asprintf "length of placeholder %s" s, p))
      | Value.StringSet _ -> Error (Type_error ("length of a Set value", p))
      | Value.Unit -> Ok (Val (Value.Int total))
    )
    | Val value, _ -> Error (Internal (Format.asprintf "length accumulated %a" Value.pp value, p))
  ) (Ok (Val (Value.Int 0))) values

(* Walks a dot-notation path from the current node, computing derived fields on
   demand when the path reaches one still held as a stub *)
and resolve: env -> (Nt.t * int option * int option) list -> (SA.solver_ast option, error) result
= fun env path -> match path with
| [] -> Ok (Some env.node)
| ((id, option_idx, occurrence) as step) :: rest ->
  (* The permission covers a path's first step only, so taking one spends it *)
  let at_annotation = env.at_annotation in
  let env = { env with at_annotation = false } in
  match child_for_step env.node step with
  (* An annotation's own constraints name the annotated node, and it is one occurrence
     of one option, so a first step naming it resolves only at index 0 of either kind *)
  | None when not (Nt.is_attribute id) ->
    let names_self = match env.node with
    | Node ((label, _, _), _) -> Nt.equal_ci (Nt.unstub id) (Nt.unstub label)
    | Leaf _ | StubLeaf _ | Model _ | Infeasible -> false
    in
    let in_range i = match i with None | Some 0 -> true | Some _ -> false in
    if at_annotation && in_range option_idx && in_range occurrence && names_self
    then resolve env rest else Ok None
  (* The desugaring gives every attribute a node, so a missing one under a present
     owner is a defect rather than an absent syntactic category *)
  | None -> (
    match env.node with
    | Node _ ->
      Error (Internal (Format.asprintf "attribute %a has no node under %a" Nt.pp id
        SA.pp_print_solver_ast env.node, Lexing.dummy_pos))
    | Leaf _ | StubLeaf _ | Model _ | Infeasible -> Ok None
  )
  | Some child ->
    match child with
    | Node (label, [SA.StubLeaf stub]) when Nt.StubMap.mem stub env.deps ->
      (* The derived field's expression is written against the rule that defines
         it, which is the rule applied at the stub's parent *)
      let* value = compute_stub env stub in
      resolve { env with node = Node (label, [SA.Leaf value]) } rest
    | Node _ | Leaf _ | StubLeaf _ | Model _ | Infeasible ->
      resolve { env with node = child } rest

(* A constraint holds when it denotes true, and also when it denotes Top, since a
   constraint over absent syntactic categories is trivially satisfied *)
let holds: env -> A.expr -> (bool, error) result
= fun env expr ->
  let* v = eval env expr in
  match v with
  | Top -> Ok true
  | Val (Value.Bool b) -> Ok b
  | Val _ -> type_error "constraint" [v] (A.pos_of_expr expr)
