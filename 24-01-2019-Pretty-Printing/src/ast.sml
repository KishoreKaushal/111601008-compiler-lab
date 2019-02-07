structure Ast = struct 
  type id = string
  
  datatype exp = 
               (* Literals *)
                 NIL
               | INTEGER of int
               | STRING of string
               (* Array and record creations *)
               | ARRAY_CREATE of (id * exp * exp)
               | RECORD_CREATE of (id * (id * exp) list)

               (* Operations *)
               | OP of (exp * op * exp)
               | OPS of exps
               | NEG of (exp)
               
               (* Varibles, fields, elements of an array *)
               | LVALUE of lvalue

               (* Function call *)
               | FUNCTION_CALL of (id * exp list)

               (* Method call *)
               | METHOD_CALL of (lvalue * id * exp list)

               (* Assignment *)
               | ASSIGNMENT of (lvalue * exp)

               (* Control structures *)
               | IF_THEN of (exp * exp * exp)
               | WHILE of (exp * exp)
               | FOR of (id * exp * exp * exp)
               | BREAK 
               | LET of (decs * exps) 

  and

  datatype decs = DEC_LIST of (dec list)

  and 

  datatype dec  = TYPE_DEC of (id * ty)
                (* variable declaration *)
                | VAR_DEC of (id * exp)
                (* function declaration *)
                | FUNC_DEC of (id * tyfields * exp)

  and 

  datatype ty   = 
                (* type alias *)
                  TYPEID of id
                (* record type definition *)
                | TYFIELDS of (id * id) list
		| ARRAY of id

  and 

  datatype op = PLUS | MINUS | TIMES 
                     | DIVIDE | EQ | NE 
                     | GT | LT | GE | LE 
                     | AND | OR

  and 

  datatype lvalue    = LVAL_IDEN of id
                     | FIELD of (lvalue * id)
                     | ELEMENT of (lvalue * exp)

  and 

  datatype exps = exp list

  and

  datatype program 	= EXPR of exp
			| DECS of decs


  fun array_create typeid exp1 exp2 = ARRAY_CREATE (typeid , exp1, exp2)
  (* fun record_create *)
  
  fun lvalue_create = 
  
end
