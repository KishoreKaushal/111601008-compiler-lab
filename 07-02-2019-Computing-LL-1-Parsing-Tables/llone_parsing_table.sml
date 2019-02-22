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

val lpt : lloneParsingTable ref = ref LLONE_TBL_MAP.empty;

let 
    val sym = ref sym_list
in
    while List.null (!sym) = false do (
        let
            val x = hd(!sym);
            val prods = ref (RHSSet.listItems ( AtomMap.lookup((#rules Grm) , x ) handle NotFound => RHSSet.empty ))
        in

        end;

        sym := List.tl(!sym)
    )
end
