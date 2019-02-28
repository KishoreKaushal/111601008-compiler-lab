Compiler course project: Designing a compiler for converting C- to MIPS.

## Language Specification

### Tokens:

```
letter = [a-zA-Z]

digit = [0-9]

letdig = digit | letter 

ID = letter letdig*

NUMCONST = digit+

CHARCONST = is any representation for a single character by placing that character in single quotes. A backslash is an escape character. Any character preceded by a backslash is interpreted as that character. For eg. \x is the letter x, \' is the single quote, \\ is the single backslash. There are
only two exceptions to this rule: \n is a newline character and \0 is the null character.

Whitespaces (a sequence of blanks and tabs) is ignored. Whitespace may be required to separate some tokens in order to get the scanner not to collapse them into one token.  For example: “intx” is a single ID while “int x” is the type int followed by the ID x.  The scanner, by its nature, is a greedy matcher.

**Comments** are ignored by the scanner.  Comments begin with //
and run to the end of the line. No nested or multi-line comments is supported.

```

