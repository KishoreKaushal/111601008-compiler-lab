type RHS = Atom.atom list  (* The RHS γ of a rule A -> γ *)

(*

We have the structures AtomSet and AtomMap to represent sets and maps
of Atoms. For any type t if we want sets and maps (dictionaries) we
need an ordering structure on the elements.  We would like to create
the set structure on RHS's. For this you first need to define a
structure of signature ORD_KEY for RHS.

*)

structure RHS_KEY : ORD_KEY = struct
    (* complete this *)
    type ord_key = Atom.atom list
    fun compare (a , b)  = case (a , b) of
                            ([], [])    => EQUAL
                        |   ([], x::xs) => LESS
                        |   (x::xs, []) => GREATER
                        |   (x::xs, y::ys)
                                        =>  let
                                                val temp = Atom.lexCompare(x,y)
                                            in
                                                case temp of
                                                    EQUAL   => compare(xs , ys)
                                                |   GREATER => GREATER
                                                |   LESS    => LESS
                                            end
end


(*

Use the above structure to create a set of rhs's

*)

structure RHSSet = RedBlackSetFn (RHS_KEY)

type Productions = RHSSet.set

(* The rules of the grammar are a dictionary whose keys are the symbol
   and the values are the Productions associated with the grammar.
*)

type Rules = Productions AtomMap.map


type Grammar    = { symbols : AtomSet.set, tokens : AtomSet.set, rules : Rules }


val sym = ref AtomSet.empty ;
sym := AtomSet.add (!sym , Atom.atom "X") ;
sym := AtomSet.add (!sym , Atom.atom "Y") ;
sym := AtomSet.add (!sym , Atom.atom "Z") ;

val tok = ref AtomSet.empty ;
tok := AtomSet.add (!tok , Atom.atom "a") ;
tok := AtomSet.add (!tok , Atom.atom "c") ;
tok := AtomSet.add (!tok , Atom.atom "d") ;

val X_prod : Productions = RHSSet.fromList ([
        [Atom.atom "Y"],
        [Atom.atom "a"]
    ])

val Y_prod : Productions = RHSSet.fromList ([
        [],
        [Atom.atom "c"]
    ])

val Z_prod : Productions = RHSSet.fromList ([
        [Atom.atom "d"],
        [Atom.atom "X" , Atom.atom "Y", Atom.atom "Z"]
    ])

val rul : Rules ref = ref AtomMap.empty ;
rul := AtomMap.insert (!rul , Atom.atom "X" , X_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "Y" , Y_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "Z" , Z_prod) ;

val Grm : Grammar = {
    symbols = !sym,
    tokens = !tok,
    rules = !rul
}

val FIRST : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val FOLLOW : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val NULLABLE : bool AtomMap.map ref = ref AtomMap.empty;

(*
    An example to show how to insert in FIRST
    FIRST := AtomMap.insert (!FIRST , Atom.atom "X" , AtomSet.fromList([Atom.atom "a" , Atom.atom "b"]))
*)

(* Initializing FIRST, FOLLOW & NULLABLE *)

fun initialize_first_sym [] = ()
|   initialize_first_sym (y::sym_list) = (
        FIRST := AtomMap.insert (!FIRST , y , AtomSet.empty);
        initialize_first_sym sym_list
    )

fun initialize_first_tok [] = ()
|   initialize_first_tok (y::tok_list) = (
        FIRST := AtomMap.insert (!FIRST , y , AtomSet.singleton(y));
        initialize_first_tok tok_list
    )

fun initialize_follow [] = ()
|   initialize_follow (y::sym_list) = (
        FOLLOW := AtomMap.insert (!FOLLOW , y , AtomSet.empty);
        initialize_follow sym_list
    )

fun initialize_nullable [] = ()
|   initialize_nullable (y::sym_list) = (
        NULLABLE := AtomMap.insert (!NULLABLE , y , false);
        initialize_nullable sym_list
    )


fun initialize () = let
                        val sym_list = AtomSet.listItems (#symbols Grm)
                        val tok_list = AtomSet.listItems (#tokens Grm)
                    in
                        (   initialize_first_sym sym_list;
                            initialize_first_tok tok_list;
                            initialize_follow sym_list;
                            initialize_nullable sym_list )
                    end


(* Loop Until the FIRST , FOLLOW and NULLABLE converges *)



val repeat : bool ref = ref true;
val modified : bool ref = ref true;

while !repeat = true do (
    modified := false;



    if (!modified = true) then
        repeat := true
    else repeat := false
)
