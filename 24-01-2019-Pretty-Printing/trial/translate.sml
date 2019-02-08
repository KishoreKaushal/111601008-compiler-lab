structure Translate =
struct

val indent = ref 0

fun print_expression (Ast.INT x) = print (Int.toString (x))
    | print_expression (Ast.PLUS (x , y)) = ( print_expression x ; print(" + ") ; print_expression y)
    | print_expression (Ast.TIMES (x , y)) = ( print_expression x ; print(" * ") ; print_expression y)
    | print_expression (Ast.DIVIDE (x , y)) = ( print_expression x ; print(" / ") ; print_expression y)
    | print_expression (Ast.MINUS (x , y)) = ( print_expression x ; print(" - ") ; print_expression y)

fun compile []        = ()
  | compile (x :: xs) = (print_expression x ; print "\n" ; compile xs)

end