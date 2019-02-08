structure Ast = struct 
    datatype exp  = INT of int 
                | PLUS of (exp * exp)
                | MINUS of (exp * exp)
                | TIMES of (exp * exp)
                | DIVIDE of (exp * exp)
                
    datatype program = EXPS of (exp list)
end
