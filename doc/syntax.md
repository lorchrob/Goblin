A Goblin input is a context-free grammar annotated with semantic constraints at the production rule level.
Below, we outline the syntax of Goblin inputs using an extension of BNF notation, where
the `*` operator denotes zero or more instances, the `+` operator denotes one or more instances,
and the square brackets denote optional elements.

```
<S> ::= <element>+
<element> ::= <type_annotation> <prod_rule> 
<type_annotation> ::= <nonterminal> :: <type> [ { <constraint>+ } ]
...
```

Intuitively, a Goblin input is a list of grammar elements, where each grammar element is either a **type annotation** or a **production rule.**

A type annotation is comprised of a nonterminal and a type, e.g. `<my_int> :: Int;`, denoting that nonterminal `<my_int>` has type `Int` (mathematical integer).

A production rule is comprised of a mapping from a left-hand side nonterminal to a nonempty list of production rule options, where each production rule option is a nonempty list of nonterminals. For example,  `<A> ::= <B> | <B> <C> | <B> <C> <D>;` denotes that nonterminal `<A>` has three possible productions: `<B>`, `<B> <C>`, and `<B> <C> <D>`.
