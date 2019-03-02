Compiler course project: Designing a compiler for converting C- to MIPS.

## Language Specification

[C-Grammar.pdf](http://marvin.cs.uidaho.edu/Teaching/CS445/c-Grammar.pdf)

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

Comments are ignored by the scanner.  Comments begin with //
and run to the end of the line. No nested or multi-line comments is supported.

```

### Grammar : 

* program           :   declarationList

* declarationList   :   declarationList declaration
                    |   declaration

* declaration       :   varDeclaration
                    |   funDeclaration
                    |   recDeclaration

---------

* recDeclaration    :   **record ID** **{** localDeclarations **}**

---------

* varDeclaration    :   typeSpecifier varDeclList **;**

* scopedVarDeclaration  :   scopedTypeSpecifier varDeclList **;**

* varDeclList       :   varDeclList **,** varDeclInitialize
                    |   varDeclInitialize

* varDeclInitialize :   varDeclId
                    |   varDeclId **:** simpleExpression

* varDeclId         :   **ID**
                    |   **ID [ NUMCONST ]**

* scopedTypeSpecifier   :   **static** typeSpecifier
                        |   typeSpecifier

* typeSpecifier     :   returnTypeSpecifier
                    |   **RECTYPE**

* returnTypeSpecifier   :   **int**
                        |   **bool**
                        |   **char**

-----

* funDeclaration    :   typeSpecifier **ID** **(** params **)** statement
                    |   **ID** **(** params **)** statement

* params            :   paramList 
                    |   

* paramList         :   paramList **;** paramTypeList
                    |   paramTypeList

* paramTypeList     :   typeSpecifier paramIdList

* paramIdList       :   paramIdList **,** paramId
                    |   paramId

* paramId           :   **ID**
                    |   **ID** **[** **]**

-----

* statement         :   expressionStmt
                    |   compoundStmt
                    |   selectionStmt
                    |   iterationStmt
                    |   returnStmt
                    |   breakStmt

* compoundStmt      :   **{** localDeclarations statementList **}**

* localDeclarations :   localDeclarations scopedVarDeclaration
                    |   

* statementList     :   statementList statement
                    |   

* expressionStmt    :   expression **;**
                    |   **;**

* selectionStmt     :   **if** **(** simpleExpression **)** **{** statement **}**
                    |   **if** **(** simpleExpression **)** **{** statement **}** **else** **{** statement **}**

* iterationStmt     :   **while** **(** simpleExpression **)** **{** statement **}**

* returnStmt        :   **return** **;** 
                    |   **return** expression **;**

* breakStmt         :   **break** **;**


-----

* expression        :   mutable __:=__ expression
                    |   simpleExpression

* simpleExpression  :   simpleExpression **or** andExpression 
                    |   andExpression

* andExpression     :   andExpression **and** unaryRelExpression
                    |   unaryRelExpression

* unaryRelExpression:   **not** unaryRelExpression 
                    |   relExpression

* relExpression     :   sumExpression relop sumExpression
                    |   sumExpression

* relop             :   **<=**
                    |   **<**
                    |   **>**
                    |   **>=**
                    |   **==**
                    |   **!=**

* sumExpression     :   sumExpression sumop term
                    |   term

* sumop             :   **+**
                    |   **-**

* term              :   term mulop unaryExpression
                    |   unaryExpression

* mulop             :   __*__
                    |   __/__
                    |   __%__

* unaryExpression   :   unaryop unaryExpression
                    |   factor

* unaryop           :   **-**
                    |   __*__

* factor            :   immutable
                    |   mutable

* mutable           :   **ID**
                    |   mutable **[** expression **]**
                    |   mutable **.** **ID**

* immutable         :   **(** expression **)** 
                    |   call
                    |   constant

* call              :   **ID** **(** args **)**

* args              :   argList
                    |

* argList           :   argList **,** expression 
                    |   expression

* constant          :   **NUMCONST**
                    |   **CHARCONST**
                    |   **true**
                    |   **false**