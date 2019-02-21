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

val lpt : lloneParsingTable ref = ref LLONE_TBL_MAP.empty;

