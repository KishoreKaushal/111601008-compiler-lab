structure Ast = struct 

    type ID = string

    datatype binop = PLUS | MINUS | TIMES | DIVIDE | MOD

    datatype relop = GT | GE | LT | LE | EQ | NEQ

    datatype unaryop = UMINUS | NOT | ASTERISK

    datatype boolop = AND | OR | XOR

    datatype program = declaration list

    and declaration = VARDEC of varDeclaration
                    | FUNDEC of funDeclaration

    and funDeclaration = RETFUNC of (typeSpecifier * ID * param list * statement)

    and param = PARAM of ( typeSpecifier * paramId list )

    and paramId = PARAM_IDEN of ID 
                | PARAM_ARR_IDEN of ID

    and statement   = EXPR of (expression option)
                    | SEL of selectionStmt
                    | RET of returnStmt

    and expression  = SIMP of simpleExpression
                    | ASSIGNMENT of (mutable * expression)
    
    and simpleExpression = OPERATION of (expression * binop * expression)
                        | RELATION of (expression * relop * expression)
                        | BOOLEAN of (expression * boolop * expression)
                        | UNARY of (unaryop * expression) 

    and mutable = MUT_IDEN of ID
                | MUT_ARR of (mutable * expression)

    and selectionStmt   = IF of (simpleExpression * statement)
                        | IF_ELSE of (simpleExpression * statement * statement)

    and returnStmt = RETNOTHING
                    | RETEXPR of (expression)
    
    and typeSpecifier = INT | BOOL | CHAR

end