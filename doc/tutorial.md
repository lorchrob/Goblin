### High-level Intuition

Goblin is a **context-sensitive** **input generation** tool intended to be used 
within the context of a **fuzzing workflow** (where the bolded terms will be explained 
in this document). 

To understand the high-level intuition for Goblin, consider the situation of 
fuzzing a `.pdf` file viewer. An example high-level workflow could proceed as follows:

```
                          (1)        (2)
`.pdf` file specification --> Goblin --> concrete `.pdf` file 
                                 ^              | (3)
                                 |              v         (4)
                                  ------- fuzzing driver  --> SUT (`.pdf` viewer)  
                                   (7)          ^                   | (5)
                                                |                   v
                                                -------------  SUT output 
                                                      (6)
```

In the above workflow, Goblin's job is to take as input a `.pdf` file specification 
and produce as output a concrete `.pdf` file that conforms to the `.pdf` file specification. 
Other parts of the workflow, including feeding the concrete `.pdf` file to the 
system under test (SUT) (in this case, the `.pdf` viewer) 
and analyzing the output of the SUT are not the responsibility of Goblin, 
but rather the fuzzing driver.
(In response to outputs from the SUT, the fuzzing driver may want to update the 
`.pdf` file specification used by Goblin, which explains the purpose of arrow 7. 
Strictly speaking, arrow 7 is optional.)

The above workflow illustrates how Goblin is an **input generation** tool to be used 
within a **fuzzing workflow**. 
The rest of the tutorial will focus on Goblin's input-output interface --- 
namely, how to specify Goblin's input based on **context-sensitive** grammars (arrow 1), 
and how to understand Goblin's outputs (arrow 2). 

### Goblin Input

#### Context-free grammars

Suppose we wish to produce outputs which are pairs of integers of the form 
`<i1, i2>`. For example, `<3, 4>`, `<0, -2>`, and so on. 

A traditional way of capturing input structure is with **context-free grammars** (CFGs), 
where the set of valid inputs is captured by the language of the grammar.
In other words, we need to construct a grammar where the strings producible 
from the grammar production rules correspond to pairs of integers.

A standard CFG (NOT Goblin input) to capture pairs of integers 
could proceed as follows:

```
Pair   ::= "<" Int "," Int ">"
Int    ::= Sign Digits
Sign   ::= "+" | "-" | ε
Digits ::= Digit Digits | Digit
Digit  ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
```

The `Pair` production rule produces a pair of integers, 
each defined with the `Int` production rule, and so on.
The vertical bars denote options; e.g., a `Digit` is either `0`, `1`, `2`, ..., `9`.

The Goblin encoding is actually much simpler than the standard CFG representation:

```
<S> ::= <I> <I>;
<I> :: Int;
```

(In Goblin, each nonterminal symbol must be enclosed in angle brackets (`<`, `>`).)

The first line defines the start symbol `<S>` with a single production rule (with double colon 
equals `::=`)
of two `<I>` nonterminals, and the second line ascribes symbol `<I>` with a 
**type annotation** (with double colon `::`) denoting that each `<I>` should produce an integer. 
The Goblin input is much simpler for two reasons: 
(i) Goblin works at the level of 
**abstract syntax** rather than **concrete syntax**, and 
(ii) Goblin views grammars as producers of **algebraic data types** (with type annotations such as `Int`, `Bool`, and so on) rather than bare strings. 
In fact, our workflow above was over-simplified. In fact, we should have:

```
`.pdf` file specification --> Goblin --> abstract `.pdf` file --> serializer --> concrete `.pdf` file
             ...
```

Above, the concrete `.pdf` file is produced from the abstract `.pdf` file outside of Goblin (using some separate serializer).
Now, let's return to the Goblin input: 

```
<S> ::= <I> <I>;
<I> :: Int;
```

Now is a good time to try to run Goblin --- copy the above text into a new file, 
and invoke `goblin --file <path_to_file>` (make sure you have built Goblin by 
running `make` from the top-level project directory).

The above command produces output `(S0 (I0 0) (I1 0))`, representing the pair `<0, 0>`.
Here, Goblin's output `(S0 (I0 0) (I1 0))` is a term representing the **abstract syntax**, 
while `<0, 0>` represents the **concrete syntax**.
Goblin works at the level of **abstract syntax** because it eases the handling 
of semantic constraints, which will be discussed in the next section. 
But, for now, it is just important to understand that with Goblin inputs, 
we wish to capture only the **logical structure** of the input specification, 
omitting purely syntactic details. 

From now until the beginning of the Goblin Output section, we will focus 
purely on abstract syntax, since that is the focus of Goblin.

Consider another example, a Goblin input capturing arbitrary-length lists of integers: 

```
<L> ::= <I> <L> | <Nil>; 
<I> :: Int;
<Nil> :: Unit;
```

Note that the nonterminal on the left-hand side of the first production rule of the input file 
(in this case, `<L>`) will be interpreted as the start symbol.
The first production rule option denotes prepending an integer `<I>` to the output list, 
and the second production rule option producing `<Nil>` denotes terminating the list.

Goblin may produce output `(L0 (Nil0 ()))` denoting the empty list, 
`(L0 (I0 0) (L0 (Nil0 ())))` denoting a list with one integer `0`, 
a list `(L0 (I0 0) (L0 (I0 1) (L0 (I0 2) (L0 (Nil0 ())))))` denoting the list `[0; 1; 2]`, 
and so on.

Nil's type, `Unit`, is borrowed from functional programming 
and represents that `Nil` does not carry any meaningful value --- 
it is analogous to the empty string `""` in a standard CFG.

Notice that we did not explicitly ascribe a type to `<L>`. 
The rule is that each nonterminal symbol either 
(i) produces a non-empty set of production rule options 
(of the form `<NT> ::= option1 | option2 | ...`), (exclusive) or 
(ii) has exactly one type annotation (of the form `<NT> :: Type`).
Note that each production rule option from (i) must be comprised solely of nonterminals --- 
again, Goblin does not process concrete syntax, only abstract syntax.

Goblin is **nondeterministic**, meaning that multiple invocations of Goblin on the same 
input may produce different outputs (but all outputs will be in the language of the input 
grammar). To ensure reproducible output, you can use the `--seed` command-line argument 
to set a seed, which will make Goblin consistently produce the same outputs for the given input.

To produce multiple outputs for a given input grammar, you can use the `--multiple-solutions` 
flag, which will produce outputs delimited by a dollar sign (`$`).
By default, this flag will cause Goblin to produce outputs indefinitely; 
to fix the number of solutions, use `--num-solutions n` for some natural number `n`.

For more information on Goblin's command-line arguments, use `--help`.

#### Context-sensitive grammars

To take advantage of the full power of Goblin, we must move beyond context-free grammars to 
**context-sensitive** grammars. 
In our setting, a **context-sensitive grammar** is simply a CFG, 
but with additional semantic constraints (serving as well-formedness requirements) annotated
on top of the grammar rules. 

Consider the integer pair example from earlier: 

```
<S> ::= <I> <I>;
<I> :: Int;
```

Say we want to encode a grammar of all the integer pairs that sum to 100, e.g., 
`<49, 51>` and `<-1, 101>`, but NOT `<50, 49>`. 
For this example, the syntactic requirements are identical (we are still producing pairs 
of integers), but we want to encode a **semantic** constraint which restricts the language 
of the grammar. In Goblin, we encode this as follows: 

```
<S> ::= <I> <I> { <I> + <I> = 100; } ;
<I> :: Int;
```

The context-free portion of the grammar is exactly the same, 
but we also added the semantic constraint `<I> + <I> = 100` 
within curly braces on the corresponding production rule. 
The semantics are that whenever the production rule `<S> ::= <I> <I>` 
is taken in a derivation, 
we must only generate terms such that `<I> + <I>` is equal to `100`. 

How about our earlier list example? How would we encode a context-sensitive grammar 
describing lists of integers that sum to 100? 
We will start with an easier problem --- encoding lists of integers that are all odd: 

```
<L> ::= <I> <L> | <Nil>; 
<I> :: Int { <I> mod 2 = 1; } ;
<Nil> :: Unit;
```

Alternatively, we could say:

```
<L> ::= <I> <L> { <I> mod 2 = 1; } | <Nil>; 
<I> :: Int; 
<Nil> :: Unit;
```

Both examples encode a list of integers `<I>`, where 
each `<I>` is odd. 

Notice that it is possible to encode context-sensitive grammars
with constraint sets that yield an empty language 
(ie, it is not possible to generate **any** terms in 
the language of the grammar that satisfy the constraints):

```
<L> ::= <I> <L> { <I> mod 2 = 1; } | <I> { <I> mod 2 = 1; } ; 
<I> :: Int { <I> mod 2 = 0; }; 
<Nil> :: Unit;
```

On the above example, Goblin reports `infeasible` since it is not possible for an integer to be 
simultaneously odd and even (notice that I updated the base case of the list rule to rule out empty lists).

Now, back to the harder problem -- encoding an (arbitrary-length) list of integers 
that all sum to 100: 

```
<S> ::= <L> { <L>.<_sum> = 100; };
<L> ::= <_sum> <I> <L>
      { <_sum> = <I> + <L>.<_sum>; } 
      | <_sum> <Nil> { <_sum> = 0; }; 
<I> :: Int; 
<Nil> :: Unit;
<_sum> :: Int;
```

The above example is a lot to take in. 
First, notice that we introduced a new nonterminal `<_sum>`. 
It is prefixed by a space to (informally) denote that it is a **ghost variable**
--- it is used for specifying constraints, 
but should not be interpreted as part of the generated term. 
`<_sum>` tracks the sum of the list "so far", akin to accumulator arguments in recursive functions.
More concretely, in line 4, `<_sum>` is set to `0` because the sum of the empty list is zero, 
and in line `3`, it is set to the value of `<I>` plus the sum of the remaining list elements. 
Then, we introduced a new start symbol `<S>` for the purposes of 
constraining the top-level sum of the list to be 100 (on line 1).

Notice the usage of the dot operator `<L>.<_sum>` -- this does not refer to the value of 
`<_sum>` of the current instance of the production rule, 
but rather the instance of `<_sum>` that is reached after expanding `<L>`
(in other words, it refers to `<L>`'s child `<_sum>` in the derivation).
The dot operator is deceivingly complex --- 
eg, what happens if `<L>` does not have a child `<_sum>`? 
What if there are multiple children called `<_sum>`? 
What if one expansion option for `<L>` has a child `<_sum>`, but another does not? 
The semantics of the dot operator will be discussed in more detail in a later section.

When running Goblin on the above grammar, a possible output is 
`(S0 (L0 (_sum0 100) (I0 101) (L0 (_sum0 (- 1)) (I0 (- 1)) (L0 (_sum1 0) (Nil0 ())))))`. 
This is verbose and a bit hard for humans to read, but it denotes the list [101, 1]. 
Also, we can confirm that the `_sumN` variables indeed track the list sum "so far".
Notice that when mapping Goblin's output to a concrete term, 
I ignored the `_sumN` variables since they are ghost.

#### BitVectors

One class of use cases for Goblin is network protocol input generation. 
A unique aspect of network protocol fuzzing is that network packets often involve **bit-level**
syntax and constraints --- say, one may need to model network packet fields as 16-bit machine integers 
rather than mathematical integers. 
To model machine integers and support bitwise operators in constraints (eg, bit complement, 
left and right shifts, bitwise xor, and so on), 
Goblin uses bitvector types `BitVec(n)` for concrete, positive values of `n` 
(eg `BitVec(16)`, `BitVec(32)`).
`BitVec` is a **dependent type** in the sense that the type is parametric with respect to 
the length of the bitvector. 

To illustrate example bitvector constraints, consider the following toy network packet in Goblin: 

```
<Packet> ::= <Type> <Len> <Payload> 
{ <Len> <- int_to_bv(8, 16 + length(<Payload>));
  <Type> = int_to_bv(8, 1) => 
        ubv_to_int(<Payload>.<Byte>) < 32; };
<Payload> ::= <Byte> <Payload> 
| <Byte> { (<Byte> bvand 0b11110000) 
             = 0b10100000; };
<Type> :: BitVec(8)  
{ <Type> = int_to_bv(8, 0) or 
  <Type> = int_to_bv(8, 1); };
<Len> :: BitVec(8);
<Byte> :: BitVec(8);
```

There are three notable aspects of the above example.

First, the example illustrates the use of a few bitvector-specific functions.
For example, the `int_to_bv(len, n)` function casts integer `n` to a 
bitvector of length `len`.
Goblin is **statically** and **strongly** typed, so performing the comparison 
`<Type> = 0` without the cast would result in a type error, 
as `<Type>` has type `BitVec(8)`, while `0` has type `Int`. 
Also notice the bitwise operator `bvand` in line 6, as well as the bitvector constants 
`0b11110000` and `0b101000001`.

Second, `length(.)` (see line 2) is a special function in Goblin. 
In this example, it takes as input nonterminal `<Payload>`, 
which does not have a type annotation --- 
instead, it is defined by a production rule. 
`length(.)` will return the **total** length of its input nonterminal, 
computed by summing the lengths of all the descendant bitvectors and bit lists.

Third, the line `<Len> <- int_to_bv(8, 16 + length(<Payload>))` uses a special operator `<-`. 
The arrow operator `<-` is semantically equivalent to equality --- 
every occurrence of `<-` can be replaced with `=` without changing the language of the input grammar. 
However, `<-` can only be used in constraints of the form `<nt> <- ...`, 
ie, with a single nonterminal symbol on the left-hand side, and any arbitrary expression on the right-hand side. 
`<-` is a hint to Goblin that `<nt>` should be computed without invoking an underlying SMT solver, 
which may result in a performance boost. 
In fact, performing computation outside the SMT solver may also hinder performance, 
so we leave it to the user to decide whether to use `=` or `<-`.
Additionally, the usage of `<-` allows the right-hand side expression to contain 
functions unsupported by SMT solvers.


#### Bit Lists

To model bitvectors with arbitrary width (ie, may grow or shrink), 
Goblin supports the bit list type `List(Bool)`. 
Here, `List` is a type constructor that takes one type as input 
and returns an output type representing a list with the given element type.

Below is a simple example using a bit list:

```
<S> ::= <Len> <BL> { <Len> = seq.len(<BL>); seq.len(<BL>) > 2; };
<BL> :: List(Bool);
<Len> :: Int;
```

An example output term is 
`(S0 (Len0 3) (BL0 (seq.++ (seq.unit false) (seq.unit false) (seq.unit false))))`,
which is the bit list `[false; false; false]` with a correctly reported length of 3.
The output is presented as the concatenation (`seq.++`) of three singleton lists (`seq.unit`) 
each containing a single element `false`.
The supported operators are those from the SMT-LIB theory of sequences. 
Here, the length function `seq.len(.)` differs from the length function `length(.)` 
in the previous example --- `seq.len(.)` only works with expressions with `List(.)` types, 
while `length(.)` is polymorphic. 
For now, due to the polymorphism associated with `length(.)`, it is only supported 
on the right-hand side of an arrow operator `<-`, 
while `seq.len(.)` can be used directly in SMT constraints.

#### Dot Notation

As described in the previous section, the dot operator `.` can be used to reference 
child nonterminals further down in the derivation tree. 
However, in certain situations, the dot operator does not have obvious semantics, 
so we will provide some intuition with a few examples.

```
<A> ::= <B> <B> <C> { <B>.<D> > <C>; };
<B> ::= <D> <D>;
<D> :: Int;
```

Above, the **nonterminal expression** `<B>.<D>` intuitively could refer to 
either the first or second child `<D>` of either the first or second occurrence 
of `<B>`. The system treats all ambiguous references of this form 
as **implicitly universally quantified** over the structure of generated terms --- that is, 
one can view the above constraint as internally desugaring to 
`<B>[0].<D>[0] > <C>[0]; <B>[0].<D>[1] > <C>[0]; <B>[1].<D>[0] > <C>[0]; <B>[1].<D>[1] > <C>[0]`,
where the bracket notation `[i]` of a nonterminal symbol uniquely indicates which occurrence of the nonterminal 
symbol is being referenced. 

More concretely, `(A0 (B0 (D0 1) (D1 1)) (B1 (D0 1) (D1 1)) (C0 0))` is a member of the input grammar because
in `<A>`'s expansion, child `<C>` is less than every child `<D>` of every child `<B>`.

Furthermore, also consider the following example, where `<B>` gets a separate production rule
also referencing `<D>`.

```
<A> ::= <B> <B> <C> { <B>.<D> > <C>; };
<B> ::= <D> <D> | <D>;
<D> :: Int;
```

At term generation time, if `<B>`'s second production rule is chosen, 
then (e.g.) constraint `<B>[0].<D>[1] > <C>[0]` is considered trivially satisfied, 
since `<B>[0]` does not have a child `<D>[1]` (instead, it has a single child, `<D>[2]`).
Or, perhaps more intuitively, the constraint applies regardless of which production rule option is chosen 
for `<B>`.

Moreover, the dot operator is legal as long as `<B>` has at least one production rule option containing `<D>`
(the others may omit `<D>`).
Below, the constraint `<B>.<D> > <C>` is considered trivially satisfied if `<B>`'s second production 
rule option is chosen, since there is no `<D>` to constrain.

```
<A> ::= <B> <B> <C> { <B>.<D> > <C>; };
<B> ::= <D> <D> | <E>;
<E> :: Int;
<D> :: Int;
```

### Goblin Output

As informally described earlier, Goblin outputs terms in an abstract syntax called 
**S-expressions**. The structure of an S-expression is very simple: 
an opening paren, followed by a constructor name (in our setting, denoting the 
name of some nonterminal), followed by a list of child S-expressions, 
followed by a closing paren.
S-expressions, especially large S-expressions, can be hard to parse by human eyes 
due to the abundance of parens. 
However, the parens serve to completely disambiguate precedence,
and they are easy to parse for programs interacting with Goblin's output.

Output S-expressions unambiguously reveal the derivation strategy that was used 
to generate the term.
For example, consider the S-expression `(NT1 (NT2 2) (NT3 3))`. 
You can read this to learn 
  * The start symbol is `NT1`.
  * The derivation took a production rule option producing child nonterminals `NT2` and `NT3`.
  * Nonterminal `NT2` has an integer type, and it was expanded to value `2`. 
  * Nonterminal `NT3` has an integer type, and it was expanded to value `3`.

### Placeholders 

Some constraints are too onerous to reasonably encode in Goblin grammars. 
For example, consider a field that is supposed to hold the SHA-256 hash of 
another field. 
In theory, one could encode the calculation of the SHA-256 hash as a semantic constraint 
in Goblin. 
However, this would be a nontrivial effort, and there may be examples that rely 
on outer protocol context that cannot directly be encoded in the input. 

Thankfully, in practice, these fields are often **derived fields** 
(similar to those defined with the `<-` syntax) 
in the sense that they can be computed using information from the rest of the generated term. 
As an alternative to using the arrow `<-` to define derived fields within Goblin, 
one can simply use a **placeholder** to denote that the term has an unfinished component 
that needs to be computed outside the tool. 

To illustrate, consider the simple grammar: 

```
<S> ::= <Payload> <Hash> 
<Payload> :: BitVec(512);
<Hash> :: Placeholder { <Hash> <- "HASH_PH"; } ; 
```

This will compute (eg) the output term `(S (Payload 0b...) (Hash "HASH_PH"))`.
Then, the serializer from the above workflow can compute the value in the 
`Hash` field during serialization.

### More Examples 

See `evaluation` and `test/test_cases` for example `.gbl` files (Goblin input files). More intuitive examples with English language descriptions to come.

### How does Goblin work?

STUB


























