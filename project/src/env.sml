structure A = Ast
structure Err = ErrorMsg
structure T = Types
structure S = Symbol

structure Env :> ENV = 
struct
    type access = unit
    type ty = Types.ty

    datatype enventry = VarEntry of {ty: ty, read_only: bool}
                      | FunEntry of {formals: ty list, result : ty}

    val base_tenv = (* predefined types *)
        let
            fun addtotable ((s, t), table) = S.enter(table, S.symbol s, t)
            val toadd = [("int", T.INT), ("bool", T.BOOL), ("string", T.STRING)]
        in
            foldr addtotable S.empty toadd
        end

    val base_venv = (* predefined functions *)
        let
            fun addtotable ((s, t), table) = S.enter(table, S.symbol s, t)
            val toadd = [
                            ("printInt", FunEntry ({formals=[T.INT], result=T.VOID})),
                            ("printBool", FunEntry ({formals=[T.BOOL], result=T.VOID})),
                            ("exit", FunEntry ({formals=[T.INT], result=T.UNIT}))
                        ]
        in
            foldr addtotable S.empty toadd
        end
end
