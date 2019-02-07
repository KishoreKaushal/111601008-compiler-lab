structure Ast = struct 
  datatype exp = NIL
               | INT of int
               | STRING of string
               | typeid LBRACK exp RBR 


end
