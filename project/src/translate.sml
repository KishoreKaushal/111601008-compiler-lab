structure Translate =
struct

(* helper functions *)

fun transVarDeclId (varDecId) 
= case varDecId of Ast.VARDEC_IDEN(varId) => (print ("var " ^ varId ^ " ; \n"))
    |  Ast.VARDEC_ARR_IDEN (varId, numConst) => (print ("var " ^ varId ^ "= new Array(" ^ Int.toString(numConst) ^") ; \n"))
    |  Ast.VARDEC_INIT (varId, simExp) => ()


(* portion responsible for translating the program *)

fun transDec dec = case dec of Ast.VARDEC (_ , varDecIdList) => (transVarDecIdList varDecIdList)
                    |   Ast.FUNDEC (funDec) => ()

and transVarDecIdList [] = ()
|   transVarDecIdList (varDecId :: varDecIdList) = (transVarDeclId (varDecId); transVarDecIdList(varDecIdList))

(* compilation starts here *)

fun compile [] = ()
|   compile (dec :: decList) = (transDec (dec); compile decList)

end
