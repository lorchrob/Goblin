### High-level Intuition

Goblin is a **context-sensitive**, **input generation** tool intended to be used 
within the context of a **fuzzing** workflow. 

To understand the high-level intuition for Goblin, consider the situation of 
fuzzing a `.pdf` file viewer. An example high-level workflow could proceed as follows:

                          (1)        (2)
`.pdf` file specification --> Goblin --> concrete `.pdf` file 
                                 ^              | (3)
                                 |              v         (4)
                                  ------- fuzzing driver  --> SUT (`.pdf` viewer)  
                                   (7)          ^                   | (5)
                                                |                   v
                                                -------------  SUT output 
                                                      (6)

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
within a **fuzzing** workflow. 
The rest of the tutorial will focus on Goblin's input-output interface --- 
namely, how to specify Goblin's input based on **context-sensitive** grammars (arrow 1), 
and how to produce concrete SUT inputs from Goblin's output (arrow 2).

### Goblin Input

##### Context-free grammars

Suppose we wish to produce outputs which are pairs of integers of the form 
`<i1, i2>`. For example, `<3, 4>`, `<0, -2>`, and so on. 

A traditional way of capturing input structure is with **context-free grammars**, 
where the set of valid inputs is captured by the language of the grammar.
In other words, we need to construct a grammar where the strings producible 
from the grammar production rules correspond to pairs of integers.

A standard context-free grammar (NOT Goblin input) would proceed as follows:

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

The first line defines the start symbol `<S>` with a single production rule 
of two `<I>` nonterminals, and the second line ascribes symbol `<I>` with a 
**type annotation** denoting that each `<I>` should produce an integer. 
The Goblin input is much simpler because Goblin works at the level of 
**abstract syntax** rather than **concrete syntax**. 
In fact, our workflow above was over-simplified. In fact, we should have:

`.pdf` file specification --> Goblin --> abstract `.pdf` file --> concrete `.pdf` file
                                    ...

where the concrete `.pdf` file is produced from the abstract `.pdf` file outside of Goblin.
Returning to the Goblin input: 
```
<S> ::= <I> <I>;
<I> :: Int;
```
The above produces output `(S0 (I0 0) (I1 0))`, representing the pair `<0, 0>`.
Here, Goblin's output `(S0 (I0 0) (I1 0))` is a term representing the **abstract syntax**, 
while `<0, 0>` represents the **concrete syntax**.
Goblin works at the level of **abstract syntax** because it eases the handling 
of semantic constraints, which will be discussed in the next section. 
But, for now, it is just important to understand that with Goblin inputs, 
we wish to capture only the **logical structure** of the input specification, 
omitting purely syntactic details.

##### Context-sensitive grammars

##### Derived fields

### More Examples 

See `evaluation` and `test/test_cases` for example `.gbl` files (Goblin input files). More intuitive examples with English language descriptions to come.
