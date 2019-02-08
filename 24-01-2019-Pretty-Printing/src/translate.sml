structure Translate =
struct

val indent = ref 0

fun print_expression (Ast.INT x) = print (Int.toString (x))
|   print_expression (Ast.BINOP (x , bop , y)) =
            (   print_expression x ;
                print (" " ^ (Ast.binopDenote bop) ^ " ") ;
                print_expression y
            )
|   print_expression (Ast.STRING x) = print(x)
|   print_expression (Ast.ASSIGNMENT (lval , x)) =
        ( print_lvalue lval ; print " := " ; print_expression x )

and

print_lvalue (Ast.LVAL_IDEN x) = print(x)
|   print_lvalue (Ast.FIELD (lval , id)) =
        ( print_lvalue lval ; print "." ; print id )
|   print_lvalue (Ast.ELEMENT (lval, x)) =
        ( print_lvalue lval ; print "[" ; print_expression x ; print "]")


fun compile []        = ()
  | compile (x :: xs) = (print_expression x ; print ";\n" ; compile xs)

end
