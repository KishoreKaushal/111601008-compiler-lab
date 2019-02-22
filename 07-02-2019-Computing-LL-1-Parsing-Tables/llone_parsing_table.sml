use "first_follow_set.sml";

(*
    These reference variables are available for further processing:
        FIRST
        FOLLOW
        NULLABLE

    Grammar related variables are :
        Grm
        sym_list
        tok_list
*)

structure LLONE_KEY : ORD_KEY = struct
    (* complete this *)
    type ord_key = (Atom.atom * Atom.atom)
    fun compare ((a:ord_key , b:ord_key))   = case Atom.lexCompare((#1 a), (#1 b)) of 
                                                EQUAL => ( Atom.lexCompare((#2 a), (#2 b)) )
                                            |   LESS => LESS 
                                            |   GREATER => GREATER
end
;

structure LLONE_TBL_MAP = RedBlackMapFn (LLONE_KEY)

type lloneParsingTable = Productions LLONE_TBL_MAP.map


fun first_set ([]) = AtomSet.empty
|   first_set (x::rhs) = let 
                            val first_of_x = (AtomMap.lookup(!FIRST, x) handle NotFound => AtomSet.empty)
                         in
                            if (isNullable (x)) then (AtomSet.union(first_set(rhs), first_of_x))
                            else first_of_x 
                         end

fun follow_set_sym x =  (AtomMap.lookup(!FOLLOW, x) handle NotFound => AtomSet.empty)

fun printAtomTuple (a,b) = (print ("(" ^ Atom.toString(a) ^ ", " ^ Atom.toString(b) ^ ")"))


fun printRHSSetHelper ([]) = (print "\b\b")
|   printRHSSetHelper (p::prod_lst) = (printAtomList(p); print ", "; printRHSSetHelper(prod_lst))

fun printRHSSet st = printRHSSetHelper(RHSSet.listItems (st))

fun printLLoneTableHelper ([]) = ()
|   printLLoneTableHelper ((tup::ll_lst): (LLONE_KEY.ord_key * Productions) list) 
                                        =   let 
                                                val (k , v) = (#1 tup , #2 tup)
                                            in
                                                printAtomTuple(k);
                                                print " : { ";
                                                printRHSSet (v);
                                                print " }\n";
                                                printLLoneTableHelper(ll_lst)
                                            end


fun printLLoneTable lpt_ = (print "==== LLONE PARSING TABLE ====\n"; 
                            printLLoneTableHelper (LLONE_TBL_MAP.listItemsi(lpt_));
                            print "=============================\n")

val lpt : lloneParsingTable ref = ref LLONE_TBL_MAP.empty;

let 
    val sym_set = AtomSet.fromList sym_list ;
    val sym = ref sym_list
in
    while (List.null (!sym) = false) do (
        let
            val x = hd(!sym);
            val x_prods = ref (RHSSet.listItems ( AtomMap.lookup((#rules Grm) , x ) handle NotFound => RHSSet.empty ))
        in
            while (List.null (!x_prods) = false) do (
                let 
                    val rhs = ref (List.hd (!x_prods));
                    val first_of_x = first_set (!rhs);
                    val follow_of_x = follow_set_sym x;
                    val addEntry = fn t => let 
                                                val tup = ( (LLONE_TBL_MAP.remove (!lpt , (x,t))) 
                                                            handle LibBase.NotFound => (!lpt, RHSSet.empty));
                                                val (mp , el) = (ref (#1 tup ) , ref (#2 tup))
                                            in 
                                                lpt := !mp;
                                                el := RHSSet.union(!el , RHSSet.fromList([!rhs]));
                                                lpt := LLONE_TBL_MAP.insert (!lpt , (x,t) , !el)
                                            end;
                    val T = ref first_of_x
                in
                    if (isProductionNullable(!rhs)) then (T := AtomSet.union (!T , follow_of_x))
                    else ();
                    T := AtomSet.difference(!T , sym_set);
                    AtomSet.app (addEntry) (!T)
                end;
                x_prods := List.tl(!x_prods)
            )
        end;
        sym := List.tl(!sym)
    )

end
;

(* printing the LLONE parsing table *)

printLLoneTable (!lpt)