structure Translate =
struct

val indent = ref 0

val bktab = "\b\b\b\b";

fun get_tabs n = if n = 0 then "" else "    " ^ get_tabs (n-1)

fun new_line n = ("\n" ^ get_tabs(n))

fun print_expression (Ast.INT x) = print (Int.toString (x))
|   print_expression (Ast.BINOP (x , bop , y)) =
            (   print_expression x ;
                print (" " ^ (Ast.binopDenote bop) ^ " ") ;
                print_expression y
                )
|   print_expression (Ast.STRING x) = print(x)
|   print_expression (Ast.ASSIGNMENT (lval , x)) =
        ( print_lvalue lval ; print " := " ; print_expression x )

|   print_expression(Ast.LET (dec_lst , exp_lst)) =
        (   indent := (!indent + 1);

            print ("let" ^ new_line(!indent));

            print_decs (dec_lst);

            print ("in");

            print (new_line(!indent));

            print_exps (exp_lst);

            print ("end");

            indent := (!indent - 1)
            )

and

print_decs (dec::dec_lst) =
        (   print_dec (dec);
            print (new_line(!indent));
            print_decs(dec_lst))
|   print_decs []   = (print (bktab))

and

print_exps (x::exp_lst)   =
    (   print_expression (x);
        print (";");
        print (new_line(!indent));
        print_exps(exp_lst))
|   print_exps []   = (print (bktab))

and

print_dec (Ast.TYPE (id , ty)) =
    (   print ("type " ^ id ^ " = ") ;
        print_ty(ty) )
| print_dec (Ast.VARDEC (id , x)) =
    (   print ("var " ^ id ^ " := ");
        print_expression (x) )

and

print_ty (Ast.TYPEID(id)) = (print(id))

and

print_lvalue (Ast.LVAL_IDEN x) = print(x)
|   print_lvalue (Ast.FIELD (lval , id)) =
        ( print_lvalue lval ; print "." ; print id )
|   print_lvalue (Ast.ELEMENT (lval, x)) =
        ( print_lvalue lval ; print "[" ; print_expression x ; print "]")


fun compile []        = ()
  | compile (x :: xs) = (print_expression x ; print (";" ^ new_line (!indent)) ; compile xs)

end
