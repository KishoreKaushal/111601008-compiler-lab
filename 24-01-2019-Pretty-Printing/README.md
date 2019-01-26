# Parsing

### Context-free Grammars

A _language_ is a set of _strings_; each string is a finite sequence of _symbols_ taken from a finite _alphabet_. For parsing, the srings are source programs, the symbols are lexical tokens, and the alphabet is the set of token types returned by the lexical analyzer.

A __CFG__ grammar describes a language. A grammar has a set of _productions_ of the form:

* _symbol_ --> _symbol_ _symbol_ ... _symbol_

where there are zero or more symbols on the right-hand side. Each symbol is either _terminal_, meaning that it is a token from the alphabet of strings in the language, or _nonterminal_, meaning that it appears on the left-hand side of some production. No token can ever appear on the left-hand side of a production. Finally, one of the nonterminals is distinguished as the _start symbol_ of the grammar.

### Derivations

There are many different derivation of the same sentence. A _leftmost_ derivation is one in which the leftmost nonterminal symbol is always the one expanded; in a _rightmost_ derivation, the rightmost nonterminal is always next to be expanded.

### Parse Trees

A _parse tree_ is made by connecting each symbol in a derivation to the one from which it was derived. Two _different_ derivations can hace the same parse tree.

### Ambiguous Grammar

A grammar is _ambiguous_ if it can derive a sentence with two different parse trees. In compiler design we like to deal with _unambiguous_ grammar or a grammar which can be easily transformed to _unambiguous_ grammar.

The grammar given below grammar is ambiguous:

![Ambiguous Grammar](img/grammar35.png)

Let us find an unambiguous grammar that accepts the same language as above grammar. 

1. `*` _binds tighter_ than `+`, or has __higher precedence__.
2. Each operator _associates to the left_.

![Unambiguous Grammar](img/unambiguous.png)

Note: Not all ambiguous grammar can be converted to unambiguous grammar.

### End-Of-File Marker

We will use `$` to represent end of the file. Suppose `S` is the start symbol of the grammar. To indicate that `$` must come after a complete `S-phrase`, we augment the grammar with a new start symbol `S'` and a new production `S' --> S$`.

### Predictive Parsing - Recursive Descent

Some grammars are easy to parse using a simple algorithm known as recursive descent. In essence, each grammar production turns into one clause of a recursive function. A recursive-descent parser for the language given below has one function for each non-terminal and one clause for each production.

![](img/predictiveParsingGrm.png)


```
datatype token  = IF
                | THEN
                | ELSE
                | BEGIN
                | END 
                | PRINT
                | SEMI 
                | NUM
                | EQ

val tok = ref (getToken())

fun advance() = tok := getToken()

fun eat(t) = if (!tok = t) then
                advance()
            else error()

fun S() = case !tok
            of IF => (eat(IF); E();
                        eat(THEN);
                        S();
                        eat(ELSE);
                        S())
            | BEGIN => (eat(BEGIN);
                        S(); L())
            | PRINT => (eat(PRINT); E())
and L() = case !tok
            of END => (eat(END))
            | SEMI => (eat(SEMI); S(); L())
and E() = (eat(NUM); eat(EQ); eat(NUM))
```

With suitable definitions of `error` and `getToken`, this program will parse very nicely.

Let us try it with _unabiguous_ grammar given in previous section. 

```
fun S() = 
```