structure Translate =
struct

fun transDec (Ast.VARDEC (_ , varDecIdList)) = (transVarDecIdList varDecIdList)
|   transDec (Ast.FUNDEC (funDec)) = (transFunDec funDec)

and transFunDec (Ast.RETFUNC (ty, id, paramList, stmtList)) = (
    print "function ";
    print id; 
    print "(";
    transParamList paramList;
    print ") {";
    transStmtList stmtList;
    print "}"
)

and transParamList [] = ()
|   transParamList (param :: paramList) = ( transParam param; transParamList paramList)

and transParam (Ast.PARAM (ty , paramIdList)) = (transParamIdList paramIdList)

and transParamIdList [] = ()
|   transParamIdList (paramId :: paramIdList) = (transParamId paramId; 
                                                    if (List.null(paramIdList) = false) then print ", " else (); 
                                                    transParamIdList paramIdList)

and transParamId (Ast.PARAM_IDEN(id)) = (print id)
|   transParamId (Ast.PARAM_ARR_IDEN (id)) = (print id)

and transStmtList [] = ()
|   transStmtList (stmt :: stmtList) = (transStmt stmt; print "; \n"; transStmtList stmtList)

and transStmt (Ast.EXPR_STMT(NONE)) = ()
|   transStmt (Ast.EXPR_STMT(SOME(expr))) = (transExpression(expr))
|   transStmt (Ast.LOCAL_VARDEC(_,varDecIdList)) = (transVarDecIdList varDecIdList)
|   transStmt (Ast.SEL_STMT(selStmt)) = ()
|   transStmt (Ast.ITR_STMT(itrStmt)) = ()
|   transStmt (Ast.RET_STMT(retStmt)) = ()

and transExpression (Ast.SIMP_EXP(simExp)) = (transSimpleExpr(simExp))
|   transExpression (Ast.ASSIGNMENT(mut, expr)) = (transMutable mut; print " = "; transExpression expr)

and transMutable (Ast.MUT_IDEN(id)) = (print id)
|   transMutable (Ast.MUT_ARR(mut, expr)) = (transMutable mut; print "["; transExpression expr; print "]")

and transVarDecIdList [] = ()
|   transVarDecIdList (varDecId :: varDecIdList) = (transVarDeclId (varDecId); transVarDecIdList(varDecIdList))

and transVarDeclId (varDecId) = case varDecId of Ast.VARDEC_IDEN(varId) => (print ("var " ^ varId ^ " ; \n"))
|  Ast.VARDEC_ARR_IDEN (varId, numConst) => (print ("var " ^ varId ^ " = new Array(" ^ Int.toString(numConst) ^") ; \n"))
|  Ast.VARDEC_INIT (varId, simExp) => ( print ("var " ^ varId ^ " = "); transSimpleExpr(simExp); print " ; \n")

and transSimpleExpr (Ast.IMMUTABLE (immut)) = (transImmutable immut)
    | transSimpleExpr (Ast.OPERATION (simExp1 , binOP , simExp2)) = (transSimpleExpr(simExp1); transBinOp(binOP); transSimpleExpr(simExp2))

and transImmutable (Ast.NUM_CONST (numConst)) = (print (Int.toString(numConst)))
|   transImmutable (Ast.STRING_CONST (stringConst)) = (print (stringConst))
|   transImmutable (Ast.BOOL_CONST (boolConst)) = (if (boolConst) then print ("true") else print("false"))

and transBinOp Ast.PLUS = (print "+")
|   transBinOp Ast.MINUS = (print "-")
|   transBinOp Ast.TIMES = (print "*")
|   transBinOp Ast.DIVIDE = (print "/")
|   transBinOp Ast.MOD = (print "%")

(* compilation starts here *)

fun compile [] = ()
|   compile (dec :: decList) = (transDec (dec); compile decList)

end
