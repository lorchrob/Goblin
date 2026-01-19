# Syntax

A Goblin input is a context-free grammar annotated with semantic constraints at the production rule level. Below, we outline the syntax of Goblin inputs using an extension of BNF notation, where the `*` operator denotes zero or more instances, the `+` operator denotes one or more instances, and the square brackets denote optional elements.

### BNF Description

```
<S>               ::= <element>+
<element>         ::= <type_annotation> | <prod_rule> 
<type_annotation> ::= <nonterminal> :: <type> [ { <constraint>+ } ];
<prod_rule>       ::= <nonterminal>[(<inh-attr>, ..., <inh-attr>)] ::= <nonterminal>+ [ { (<constraint> | <synth-attr>)+ } ]
                   [| <nonterminal>+ [ { <constraint>+ } ]]+;
<constraint>      ::= <derived_field>; | <expr>;
<synth-attr>      ::= identifier := <expr>;
<inh-attr>        ::= identifier :: <type>
<derived_field>   ::= <nonterminal> <- <expr>
<expr>            ::= <expr> <binop> <expr> | <unop><expr> | <f>(<expr>, ..., <expr>)
                    | <nt_expr> | <nt_expr>(<expr>, ..., <expr>) | <nt_expr>.identifier | <constant>
<nt_expr>         ::= <nonterminal> | <nonterminal>.<nt_expr>
<nonterminal>     ::= < identifier >
<type>            ::= Bool | String | Int | BitVec(positive integer) | Set(<type>) | List(<type>) | Unit
<f>               ::= defined below
<unop>            ::= defined below
<binop>           ::= defined below
```

All the supported function symbols `<f>` and operators `<binop>/<unop>` are defined in the next section.
Note that we do not treat predicate symbols separately; they are simply function symbols for interpreted functions with return type `Bool`. 

Intuitively, a Goblin input is a list of grammar elements, where each grammar element is either a **type annotation** or a **production rule.**

A type annotation is comprised of a nonterminal and a type, e.g. `<my_int> :: Int;`, denoting that nonterminal `<my_int>` has type `Int` (mathematical integer).

A production rule is comprised of a mapping from a left-hand side nonterminal to a nonempty list of production rule options, where each production rule option is a nonempty list of nonterminals. For example,  `<A> ::= <B> | <B> <C> | <B> <C> <D>;` denotes that nonterminal `<A>` has three possible productions: `<B>`, `<B> <C>`, and `<B> <C> <D>`.

Additionally, type annotations and production rule options may carry **semantic constraints**, denoted by semicolons. For example, `<my_nat> :: Int { <my_nat> >= 0; };` denotes an integer that is greater than or equal to zero (in other words, a natural number). A type annotation or production rule option may carry multiple semantic constraints, e.g.,
`<my_subrange> :: Int { <my_subrange> >= 0; <my_subrange> <= 100; };`. Each constraint is terminated with a semicolon, and the constraints are interpreted conjunctively.

The production rule options for a particular nonterminal may include **synthesized attribute definitions** along with the semantic constraints. 
For example, `<my_list> ::= <E> <L> { len := 1 + <L>.len; } | <Nil> { len := 0 };` denotes a synthesized attribute `len` that is equal to the length of the list.

The production rule options for a particular nonterminal may also take **inherited attributes** as inputs, which can be used in the semantic constraints.
For example, `<my_list>(v :: Int) ::= <E> <L>(v) { <E> = v; } | <Nil>;` denotes a list where every element is equal to inherited attribute `v`.

The prior examples are instances of **SMT constraints**; however, one may also define a different kind of semantic constraint called a **derived field**. Derived fields are of the form `<nonterminal> <- <expr>;`, denoting that `<nonterminal>` can be computed by expression `<expr>`. Derived fields are equivalent to equality SMT constraints of the form `<nonterminal> = <expr>`, but they are different in the sense that Goblin will compute them after constraint solving rather than passing them to the SMT solver. Because of this, the set of supported function symbols for the `<expr>` in a derived field
is greater than the set supported symbols for SMT constraints, since the latter must have a straightforward translation
to SMT-LIB.

### Supported infix operators and function symbols

**SMT-LIB types:**
`Bool`, `Int`, `String`, `BitVec(n)` for any positive integer `n`

**Non-standard types (supported by cvc5):**
`List(Bool)`, `Set(String)`

**SMT-LIB operators (but infix style):**
`and`, `or`, `xor`, `=>`, `+`, `-`, `*`, `div`, `mod`, `>=`, `>`, `<=`, `<`, `bvult`, `bvor`, `bvxor`, `bvnot`

**Non-standard operators (infix style, compatible with SMT expressions):**
`bvulte`, `bvugt`, `bvugte`

**SMT-LIB function symbols:**
`true`, `false`, `not`, `str.++`, `str.prefixof`, `str.contains`, `str.in_re`, `str.to_re`, `str.len`, `re.range`, `re.union`, `re.*`, `re.++`,  `int_to_bv`, `ubv_to_int`, `sbv_to_int`

**Nonstandard function symbols supported by cvc5:**
`set.empty`, `set.union`, `set.member`, `set.inter`, `set.singleton`, `seq.len`

**Non-standard function symbols (incompatible with SMT expressions, only usable within derived fields):**
`length`

### Supporting new features

Adding support in Goblin for additional SMT-LIB (or cvc5-specific) types, operators, and function symbols is straightforward. Additionally, adding support for function symbols only compatible within derived fields (but not SMT expressions) is straightforward. 
To request a new feature, please open a GitHub issue or email `robert-lorch@uiowa.edu`.

*To request a new feature, either open a GitHub issue or email robert-lorch@uiowa.edu.*
