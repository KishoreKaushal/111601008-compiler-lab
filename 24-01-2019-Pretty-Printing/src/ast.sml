structure Ast = struct
    datatype binop = PLUS | MINUS | TIMES | DIVIDE

    type ID = string

    datatype exp    = INT of int
                    | STRING of string
                    | BINOP of (exp * binop * exp)
                    | ASSIGNMENT of (lvalue * exp)

    and

    lvalue  = LVAL_IDEN of ID
            | FIELD of (lvalue * ID)
            | ELEMENT of (lvalue * exp)

    datatype program = EXPS of (exp list)

    fun binopDenote x = case x of
                            PLUS    =>  "+"
                        |   MINUS   =>  "-"
                        |   TIMES   =>  "*"
                        |   DIVIDE  =>  "/"
end
