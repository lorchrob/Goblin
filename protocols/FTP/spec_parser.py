"""
spec_parser.py — Parse LTL monitor spec files into coverage-tracker structures.

Parses the spec language used by formula_parser:
  - enum declarations:  enum name { val1, val2, ... };
  - variable decls:     int name;  bool name;
  - rules:              H(body);

Decomposes each rule body into a coverage structure:
  - ConjunctionRule:  H(!(a & b & c))  — violation when all atoms true
  - ImplicationRule:  H(P -> Q)        — violation when P true, Q false
  - InvariantRule:    H(atom)          — must hold on every event

Each "atom" is either a simple comparison (name=value, name>value) or
a compound disjunction ((a|b|c)) treated as a single unit.

Usage:
    spec = parse_spec_file("ftp.txt")
    for rule in spec.rules:
        print(rule.name, rule.kind)
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Callable, Optional, Union


# ============================================================================
# Tokens
# ============================================================================

class TT(Enum):
    IDENT  = auto()
    NUM    = auto()
    EQ     = auto()   # =
    GT     = auto()   # >
    LT     = auto()   # <
    GE     = auto()   # >=
    LE     = auto()   # <=
    NOT    = auto()   # !
    AND    = auto()   # &
    OR     = auto()   # |
    ARROW  = auto()   # ->
    LPAREN = auto()
    RPAREN = auto()
    LBRACE = auto()
    RBRACE = auto()
    SEMI   = auto()
    COMMA  = auto()
    EOF    = auto()


@dataclass
class Token:
    kind: TT
    value: str
    pos: int

    def __repr__(self):
        return f"Token({self.kind.name}, {self.value!r})"


# ============================================================================
# Lexer
# ============================================================================

def _tokenize(text: str) -> list[Token]:
    tokens = []
    i = 0
    n = len(text)
    while i < n:
        # Skip whitespace
        if text[i].isspace():
            i += 1
            continue
        # Skip line comments
        if i + 1 < n and text[i] == '/' and text[i + 1] == '/':
            while i < n and text[i] != '\n':
                i += 1
            continue
        # Two-character operators
        if i + 1 < n:
            two = text[i:i+2]
            if two == '->':
                tokens.append(Token(TT.ARROW, '->', i)); i += 2; continue
            if two == '>=':
                tokens.append(Token(TT.GE, '>=', i)); i += 2; continue
            if two == '<=':
                tokens.append(Token(TT.LE, '<=', i)); i += 2; continue
        # Single-character tokens
        ch = text[i]
        if ch == '=':  tokens.append(Token(TT.EQ, '=', i)); i += 1; continue
        if ch == '>':  tokens.append(Token(TT.GT, '>', i)); i += 1; continue
        if ch == '<':  tokens.append(Token(TT.LT, '<', i)); i += 1; continue
        if ch == '!':  tokens.append(Token(TT.NOT, '!', i)); i += 1; continue
        if ch == '&':  tokens.append(Token(TT.AND, '&', i)); i += 1; continue
        if ch == '|':  tokens.append(Token(TT.OR, '|', i)); i += 1; continue
        if ch == '(':  tokens.append(Token(TT.LPAREN, '(', i)); i += 1; continue
        if ch == ')':  tokens.append(Token(TT.RPAREN, ')', i)); i += 1; continue
        if ch == '{':  tokens.append(Token(TT.LBRACE, '{', i)); i += 1; continue
        if ch == '}':  tokens.append(Token(TT.RBRACE, '}', i)); i += 1; continue
        if ch == ';':  tokens.append(Token(TT.SEMI, ';', i)); i += 1; continue
        if ch == ',':  tokens.append(Token(TT.COMMA, ',', i)); i += 1; continue
        # Numbers (including negative)
        if ch.isdigit() or (ch == '-' and i + 1 < n and text[i + 1].isdigit()):
            start = i
            if ch == '-':
                i += 1
            while i < n and text[i].isdigit():
                i += 1
            tokens.append(Token(TT.NUM, text[start:i], start))
            continue
        # Identifiers (alphanumeric + underscore)
        if ch.isalpha() or ch == '_':
            start = i
            while i < n and (text[i].isalnum() or text[i] == '_'):
                i += 1
            tokens.append(Token(TT.IDENT, text[start:i], start))
            continue
        raise SyntaxError(f"Unexpected character {ch!r} at position {i}")

    tokens.append(Token(TT.EOF, '', n))
    return tokens


# ============================================================================
# AST nodes (propositional — temporal operators stripped)
# ============================================================================

@dataclass
class Atom:
    """A single comparison: name op value."""
    name: str
    op: str       # '=', '>', '<', '>=', '<='
    value: str    # string representation (could be enum val, int, bool)

    def __repr__(self):
        return f"{self.name}{self.op}{self.value}"


@dataclass
class Not:
    child: ASTNode

    def __repr__(self):
        return f"!({self.child})"


@dataclass
class And:
    children: list[ASTNode]

    def __repr__(self):
        return " & ".join(f"({c})" for c in self.children)


@dataclass
class Or:
    children: list[ASTNode]

    def __repr__(self):
        return " | ".join(f"({c})" for c in self.children)


@dataclass
class Implies:
    lhs: ASTNode
    rhs: ASTNode

    def __repr__(self):
        return f"({self.lhs}) -> ({self.rhs})"


ASTNode = Union[Atom, Not, And, Or, Implies]


# ============================================================================
# Parser
# ============================================================================

class _Parser:
    """Recursive-descent parser for the spec language."""

    def __init__(self, tokens: list[Token]):
        self._tokens = tokens
        self._pos = 0

    def _peek(self) -> Token:
        return self._tokens[self._pos]

    def _advance(self) -> Token:
        tok = self._tokens[self._pos]
        self._pos += 1
        return tok

    def _expect(self, kind: TT) -> Token:
        tok = self._advance()
        if tok.kind != kind:
            raise SyntaxError(
                f"Expected {kind.name}, got {tok.kind.name} ({tok.value!r}) "
                f"at position {tok.pos}")
        return tok

    def _at(self, kind: TT) -> bool:
        return self._peek().kind == kind

    def _match(self, kind: TT) -> Optional[Token]:
        if self._at(kind):
            return self._advance()
        return None

    # --- Top-level declarations ---

    def parse_spec(self) -> tuple[dict, list, list[ASTNode]]:
        """Returns (enums, variables, rule_bodies).
        enums:  {name: [val1, val2, ...]}
        variables: [(type_str, name)]
        rule_bodies: [ASTNode]  (one per H(...) rule, temporal op stripped)
        """
        enums: dict[str, list[str]] = {}
        variables: list[tuple[str, str]] = []
        rules: list[ASTNode] = []

        while not self._at(TT.EOF):
            if self._at(TT.IDENT):
                tok = self._peek()
                if tok.value == 'enum':
                    name, vals = self._parse_enum()
                    enums[name] = vals
                elif tok.value in ('int', 'bool'):
                    vtype, vname = self._parse_var_decl()
                    variables.append((vtype, vname))
                elif tok.value == 'H':
                    body = self._parse_rule()
                    rules.append(body)
                else:
                    raise SyntaxError(
                        f"Unexpected identifier {tok.value!r} at position {tok.pos}")
            else:
                raise SyntaxError(
                    f"Unexpected token {self._peek()} at top level")

        return enums, variables, rules

    def _parse_enum(self) -> tuple[str, list[str]]:
        self._expect(TT.IDENT)  # 'enum'
        name = self._expect(TT.IDENT).value
        self._expect(TT.LBRACE)
        vals = [self._expect(TT.IDENT).value]
        while self._match(TT.COMMA):
            vals.append(self._expect(TT.IDENT).value)
        self._expect(TT.RBRACE)
        self._expect(TT.SEMI)
        return name, vals

    def _parse_var_decl(self) -> tuple[str, str]:
        vtype = self._advance().value  # 'int' or 'bool'
        vname = self._expect(TT.IDENT).value
        self._expect(TT.SEMI)
        return vtype, vname

    def _parse_rule(self) -> ASTNode:
        self._expect(TT.IDENT)  # 'H'
        self._expect(TT.LPAREN)
        body = self._parse_expr()
        self._expect(TT.RPAREN)
        self._expect(TT.SEMI)
        return body

    # --- Expression parsing (precedence climbing) ---
    # Precedence (low to high): ->, |, &, !, atom

    def _parse_expr(self) -> ASTNode:
        return self._parse_implies()

    def _parse_implies(self) -> ASTNode:
        lhs = self._parse_or()
        if self._match(TT.ARROW):
            rhs = self._parse_implies()  # right-associative
            return Implies(lhs, rhs)
        return lhs

    def _parse_or(self) -> ASTNode:
        children = [self._parse_and()]
        while self._match(TT.OR):
            children.append(self._parse_and())
        return children[0] if len(children) == 1 else Or(children)

    def _parse_and(self) -> ASTNode:
        children = [self._parse_unary()]
        while self._match(TT.AND):
            children.append(self._parse_unary())
        return children[0] if len(children) == 1 else And(children)

    def _parse_unary(self) -> ASTNode:
        if self._match(TT.NOT):
            child = self._parse_unary()
            return Not(child)
        return self._parse_primary()

    def _parse_primary(self) -> ASTNode:
        if self._match(TT.LPAREN):
            expr = self._parse_expr()
            self._expect(TT.RPAREN)
            return expr
        # Must be an atom: IDENT op VALUE
        name = self._expect(TT.IDENT).value
        op_tok = self._advance()
        if op_tok.kind == TT.EQ:
            op = '='
        elif op_tok.kind == TT.GT:
            op = '>'
        elif op_tok.kind == TT.LT:
            op = '<'
        elif op_tok.kind == TT.GE:
            op = '>='
        elif op_tok.kind == TT.LE:
            op = '<='
        else:
            raise SyntaxError(
                f"Expected comparison operator after {name!r}, "
                f"got {op_tok.kind.name} at position {op_tok.pos}")
        val_tok = self._advance()
        if val_tok.kind not in (TT.IDENT, TT.NUM):
            raise SyntaxError(
                f"Expected value after {name}{op}, "
                f"got {val_tok.kind.name} at position {val_tok.pos}")
        return Atom(name, op, val_tok.value)


# ============================================================================
# Rule classification for coverage
# ============================================================================

class RuleKind(Enum):
    CONJUNCTION = auto()    # H(!(a & b & ...))  — violation when all true
    IMPLICATION = auto()    # H(P -> Q)          — violation when P true, Q false
    INVARIANT   = auto()    # H(expr)            — must hold every step


@dataclass
class CoverageAtom:
    """A single evaluable coverage atom — either a simple comparison
    or a compound disjunction of comparisons."""
    label: str                         # human-readable description
    evaluator: Callable[[dict], bool]  # kv_dict -> bool


@dataclass
class CoverageRule:
    """Structured rule decomposition for the coverage tracker."""
    name: str
    kind: RuleKind
    # For CONJUNCTION: atoms are the conjunction members; violation when ALL true.
    # For IMPLICATION: antecedent_atoms is P; consequent_atoms is Q.
    #   Violation when all P true AND all Q false.
    # For INVARIANT: atoms is the body; property must hold each step.
    atoms: list[CoverageAtom] = field(default_factory=list)
    antecedent_atoms: list[CoverageAtom] = field(default_factory=list)
    consequent_atoms: list[CoverageAtom] = field(default_factory=list)
    source: str = ""   # original formula text for debugging


# --- Atom evaluator generation ---

def _make_evaluator(node: ASTNode) -> Callable[[dict], bool]:
    """Generate a kv_dict -> bool function from an AST node."""
    if isinstance(node, Atom):
        name, op, value = node.name, node.op, node.value
        if op == '=':
            return lambda kv, n=name, v=value: kv.get(n) == v
        elif op == '>':
            return lambda kv, n=name, v=int(value): int(kv.get(n, '0')) > v
        elif op == '<':
            return lambda kv, n=name, v=int(value): int(kv.get(n, '0')) < v
        elif op == '>=':
            return lambda kv, n=name, v=int(value): int(kv.get(n, '0')) >= v
        elif op == '<=':
            return lambda kv, n=name, v=int(value): int(kv.get(n, '0')) <= v
    elif isinstance(node, Or):
        sub_evals = [_make_evaluator(c) for c in node.children]
        return lambda kv, fns=sub_evals: any(f(kv) for f in fns)
    elif isinstance(node, And):
        sub_evals = [_make_evaluator(c) for c in node.children]
        return lambda kv, fns=sub_evals: all(f(kv) for f in fns)
    elif isinstance(node, Not):
        inner = _make_evaluator(node.child)
        return lambda kv, f=inner: not f(kv)
    raise ValueError(f"Cannot make evaluator for {type(node).__name__}: {node}")


def _node_label(node: ASTNode) -> str:
    """Human-readable label for an AST node."""
    if isinstance(node, Atom):
        return f"{node.name}{node.op}{node.value}"
    elif isinstance(node, Or):
        return "(" + " | ".join(_node_label(c) for c in node.children) + ")"
    elif isinstance(node, And):
        return "(" + " & ".join(_node_label(c) for c in node.children) + ")"
    elif isinstance(node, Not):
        return f"!({_node_label(node.child)})"
    elif isinstance(node, Implies):
        return f"({_node_label(node.lhs)}) -> ({_node_label(node.rhs)})"
    return repr(node)


def _make_coverage_atom(node: ASTNode) -> CoverageAtom:
    """Wrap an AST node as a CoverageAtom."""
    return CoverageAtom(label=_node_label(node), evaluator=_make_evaluator(node))


# --- Flatten a conjunction into its top-level children ---

def _flatten_and(node: ASTNode) -> list[ASTNode]:
    """Flatten nested And nodes into a list of children.
    Each child becomes one coverage atom (even if it's an Or)."""
    if isinstance(node, And):
        result = []
        for child in node.children:
            result.extend(_flatten_and(child))
        return result
    return [node]


# --- Rule classification ---

def _classify_rule(body: ASTNode, index: int) -> CoverageRule:
    """Classify a rule body into its coverage structure."""
    source = _node_label(body)

    # Pattern: H(!(conjunction)) — negated conjunction = violation rule
    if isinstance(body, Not):
        inner = body.child
        atoms_nodes = _flatten_and(inner)
        atoms = [_make_coverage_atom(n) for n in atoms_nodes]
        return CoverageRule(
            name=f"rule_{index + 1}",
            kind=RuleKind.CONJUNCTION,
            atoms=atoms,
            source=source,
        )

    # Pattern: H(P -> Q) — implication rule
    if isinstance(body, Implies):
        p_nodes = _flatten_and(body.lhs)
        q_nodes = _flatten_and(body.rhs)
        return CoverageRule(
            name=f"rule_{index + 1}",
            kind=RuleKind.IMPLICATION,
            antecedent_atoms=[_make_coverage_atom(n) for n in p_nodes],
            consequent_atoms=[_make_coverage_atom(n) for n in q_nodes],
            source=source,
        )

    # Pattern: H(expr) — invariant (single atom, conjunction, or anything else)
    atoms_nodes = _flatten_and(body)
    atoms = [_make_coverage_atom(n) for n in atoms_nodes]
    return CoverageRule(
        name=f"rule_{index + 1}",
        kind=RuleKind.INVARIANT,
        atoms=atoms,
        source=source,
    )


# ============================================================================
# Parsed spec container
# ============================================================================

@dataclass
class ParsedSpec:
    """Complete parsed specification."""
    enums: dict[str, list[str]]           # enum_name -> [values]
    variables: list[tuple[str, str]]      # [(type, name)]
    rules: list[CoverageRule]             # classified rules


def parse_spec(text: str) -> ParsedSpec:
    """Parse a spec string into structured coverage rules."""
    tokens = _tokenize(text)
    parser = _Parser(tokens)
    enums, variables, rule_bodies = parser.parse_spec()
    rules = [_classify_rule(body, i) for i, body in enumerate(rule_bodies)]
    return ParsedSpec(enums=enums, variables=variables, rules=rules)


def parse_spec_file(path: str) -> ParsedSpec:
    """Parse a spec file."""
    with open(path, 'r') as f:
        return parse_spec(f.read())


# ============================================================================
# Self-test
# ============================================================================

if __name__ == "__main__":
    import sys

    if len(sys.argv) < 2:
        # Use inline test spec
        test_spec = """
enum ftp_command { cmdNotSet, cmdUSER, cmdPASS, cmdRETR, cmdSTOR, cmdPASV, cmdPORT, cmdRNFR, cmdRNTO, cmdDELE };
enum ftp_status_class { scNotSet, scSuccess, scIntermediate, scPermanentError };
enum auth_state { authNone, authUserSent, authPasswordSent, authComplete, authFailed };

int resp_code;
int port_number;
bool timeout;
bool resp_malformed;
bool connection_closed;
bool quit_sent;
bool user_sent;
bool user_logged_in;
bool rnfr_accepted;

// Rule 1: auth bypass
H(!(timeout=false & resp_malformed=false & connection_closed=false & quit_sent=false & ftp_command=cmdPASS & resp_code=230 & user_sent=false));

// Rule 2: unauth file op (with disjunction)
H(!(timeout=false & resp_malformed=false & connection_closed=false & quit_sent=false & user_logged_in=false & (ftp_command=cmdRETR | ftp_command=cmdSTOR | ftp_command=cmdDELE | ftp_command=cmdRNFR | ftp_command=cmdRNTO) & ftp_status_class=scSuccess));

// Rule 3: invariant
H(resp_malformed=false);

// Rule 4: implication
H((timeout=false & resp_malformed=false & connection_closed=false & quit_sent=false & resp_code=227) -> ftp_command=cmdPASV);

// Rule 5: implication with int comparison
H((timeout=false & connection_closed=false & quit_sent=false & ftp_command=cmdPORT & ftp_status_class=scSuccess) -> port_number>0);
"""
    else:
        with open(sys.argv[1]) as f:
            test_spec = f.read()

    spec = parse_spec(test_spec)

    print(f"Enums: {len(spec.enums)}")
    for name, vals in spec.enums.items():
        print(f"  {name}: {len(vals)} values")

    print(f"\nVariables: {len(spec.variables)}")
    for vtype, vname in spec.variables:
        print(f"  {vtype} {vname}")

    print(f"\nRules: {len(spec.rules)}")
    for rule in spec.rules:
        print(f"\n  [{rule.name}] kind={rule.kind.name}")
        print(f"    source: {rule.source[:100]}{'...' if len(rule.source) > 100 else ''}")
        if rule.kind == RuleKind.CONJUNCTION:
            print(f"    atoms ({len(rule.atoms)}):")
            for a in rule.atoms:
                print(f"      {a.label}")
        elif rule.kind == RuleKind.IMPLICATION:
            print(f"    antecedent ({len(rule.antecedent_atoms)}):")
            for a in rule.antecedent_atoms:
                print(f"      {a.label}")
            print(f"    consequent ({len(rule.consequent_atoms)}):")
            for a in rule.consequent_atoms:
                print(f"      {a.label}")
        elif rule.kind == RuleKind.INVARIANT:
            print(f"    atoms ({len(rule.atoms)}):")
            for a in rule.atoms:
                print(f"      {a.label}")

    # --- Evaluator tests ---
    print("\n=== Evaluator tests ===")

    conj_rules = [r for r in spec.rules if r.kind == RuleKind.CONJUNCTION]
    impl_rules = [r for r in spec.rules if r.kind == RuleKind.IMPLICATION]
    inv_rules  = [r for r in spec.rules if r.kind == RuleKind.INVARIANT]

    print(f"  {len(conj_rules)} conjunction, {len(impl_rules)} implication, "
          f"{len(inv_rules)} invariant")

    # Test first conjunction rule (auth bypass)
    if conj_rules:
        r1 = conj_rules[0]
        kv_pass_bypass = {
            'timeout': 'false', 'resp_malformed': 'false',
            'connection_closed': 'false', 'quit_sent': 'false',
            'ftp_command': 'cmdPASS', 'resp_code': '230',
            'user_sent': 'false',
        }
        kv_normal_pass = dict(kv_pass_bypass, user_sent='true')
        bypass_vec = [a.evaluator(kv_pass_bypass) for a in r1.atoms]
        normal_vec = [a.evaluator(kv_normal_pass) for a in r1.atoms]
        print(f"  {r1.name} bypass: {bypass_vec} -> all={all(bypass_vec)}")
        print(f"  {r1.name} normal: {normal_vec} -> all={all(normal_vec)}")
        assert all(bypass_vec), "Auth bypass should satisfy all atoms"
        assert not all(normal_vec), "Normal pass should NOT satisfy all atoms"

    # Test conjunction with disjunction atom (unauth file op)
    if len(conj_rules) >= 2:
        r2 = conj_rules[1]
        kv_unauth_retr = {
            'timeout': 'false', 'resp_malformed': 'false',
            'connection_closed': 'false', 'quit_sent': 'false',
            'user_logged_in': 'false', 'ftp_command': 'cmdRETR',
            'ftp_status_class': 'scSuccess',
        }
        vec = [a.evaluator(kv_unauth_retr) for a in r2.atoms]
        print(f"  {r2.name} unauth RETR: {vec} -> all={all(vec)}")
        assert all(vec)

    # Test implication rules
    for impl in impl_rules:
        # Find one with resp_code=227 (PASV rule)
        ante_labels = [a.label for a in impl.antecedent_atoms]
        if any('227' in l for l in ante_labels):
            kv_ok = {
                'timeout': 'false', 'resp_malformed': 'false',
                'connection_closed': 'false', 'quit_sent': 'false',
                'resp_code': '227', 'ftp_command': 'cmdPASV',
            }
            kv_bad = dict(kv_ok, ftp_command='cmdLIST')
            p_ok = all(a.evaluator(kv_ok) for a in impl.antecedent_atoms)
            q_ok = all(a.evaluator(kv_ok) for a in impl.consequent_atoms)
            p_bad = all(a.evaluator(kv_bad) for a in impl.antecedent_atoms)
            q_bad = all(a.evaluator(kv_bad) for a in impl.consequent_atoms)
            print(f"  {impl.name} (227+PASV): P={p_ok} Q={q_ok} viol={p_ok and not q_ok}")
            print(f"  {impl.name} (227+LIST): P={p_bad} Q={q_bad} viol={p_bad and not q_bad}")
            assert p_ok and q_ok, "227+PASV should not violate"
            assert p_bad and not q_bad, "227+LIST should violate"

        # Find one with port_number (PORT rule)
        cons_labels = [a.label for a in impl.consequent_atoms]
        if any('port_number' in l for l in cons_labels):
            kv_ok = {
                'timeout': 'false', 'connection_closed': 'false',
                'quit_sent': 'false', 'ftp_command': 'cmdPORT',
                'ftp_status_class': 'scSuccess', 'port_number': '8080',
            }
            kv_bad = dict(kv_ok, port_number='0')
            p_ok = all(a.evaluator(kv_ok) for a in impl.antecedent_atoms)
            q_ok = all(a.evaluator(kv_ok) for a in impl.consequent_atoms)
            p_bad = all(a.evaluator(kv_bad) for a in impl.antecedent_atoms)
            q_bad = all(a.evaluator(kv_bad) for a in impl.consequent_atoms)
            print(f"  {impl.name} (PORT ok):   P={p_ok} Q={q_ok} viol={p_ok and not q_ok}")
            print(f"  {impl.name} (PORT zero): P={p_bad} Q={q_bad} viol={p_bad and not q_bad}")
            assert p_ok and q_ok
            assert p_bad and not q_bad

    print("\n=== All tests passed ===")
