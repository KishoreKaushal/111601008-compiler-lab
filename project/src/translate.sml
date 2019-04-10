structure Translate =
struct

fun transDec dec = case dec of Ast.VARDEC (_ , varDecIdList) => (transVarDecIdList varDecIdList)
                    |   Ast.FUNDEC (funDec) => ()

and transVarDecIdList [] = ()
|   transVarDecIdList (varDecId :: varDecIdList) = (transVarDeclId (varDecId); transVarDecIdList(varDecIdList))

and transVarDeclId (varDecId) = case varDecId of Ast.VARDEC_IDEN(varId) => (print ("var " ^ varId ^ " ; \n"))
|  Ast.VARDEC_ARR_IDEN (varId, numConst) => (print ("var " ^ varId ^ " = new Array(" ^ Int.toString(numConst) ^") ; \n"))
|  Ast.VARDEC_INIT (varId, simExp) => ( print ("var " ^ varId ^ " = "); transSimpleExpr(simExp); print " ; \n")

and transSimpleExpr (Ast.IMMUTABLE (immut)) = (transImmutable immut)
    | transSimpleExpr (Ast.OPERATION (simExp1 , binOP , simExp2)) = ()

and transImmutable (Ast.NUM_CONST (numConst)) = (print (Int.toString(numConst)))
|   transImmutable (Ast.STRING_CONST (stringConst)) = (print (stringConst))
|   transImmutable (Ast.BOOL_CONST (boolConst)) = (if (boolConst) then print ("true") else print("false"))

(* compilation starts here *)

fun compile [] = ()
|   compile (dec :: decList) = (transDec (dec); compile decList)

end
