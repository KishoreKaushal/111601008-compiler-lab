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

val sym_list = AtomSet.listItems (#symbols Grm)
val tok_list = AtomSet.listItems (#tokens Grm)

val FIRST : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val FOLLOW : AtomSet.set AtomMap.map ref = ref AtomMap.empty;
val NULLABLE : bool AtomMap.map ref = ref AtomMap.empty;

(*
    An example to show how to insert in FIRST
    FIRST := AtomMap.insert (!FIRST , Atom.atom "X" , AtomSet.fromList([Atom.atom "a" , Atom.atom "b"]))
*)

(* Initializing FIRST, FOLLOW & NULLABLE *)

fun initialize_first_tok [] = ()
|   initialize_first_tok (y::tok_l) = (
        FIRST := AtomMap.insert (!FIRST , y , AtomSet.singleton(y));
        initialize_first_tok tok_l
    )

fun initialize_first_sym [] = ()
|   initialize_first_sym (y::sym_l) = (
        FIRST := AtomMap.insert (!FIRST , y , AtomSet.empty);
        initialize_first_sym sym_l
    )

fun initialize_follow [] = ()
|   initialize_follow (y::sym_l) = (
        FOLLOW := AtomMap.insert (!FOLLOW , y , AtomSet.empty);
        initialize_follow sym_l
    )

fun initialize_nullable [] = ()
|   initialize_nullable (y::sym_l) = (
        NULLABLE := AtomMap.insert (!NULLABLE , y , false);
        initialize_nullable sym_l
    )

fun initialize () = (   initialize_first_sym sym_list;
                        initialize_first_tok tok_list;
                        initialize_follow sym_list;
                        initialize_nullable sym_list )

(* Loop Until the FIRST , FOLLOW and NULLABLE converges *)

val repeat : bool ref = ref true;
val modified : bool ref = ref true;

fun isNullable s = (AtomMap.lookup (!NULLABLE, s))
fun isProductionNullable rhs = (all isNullable prods)

while !repeat = true do (
    modified := false;
    let
        val sym = ref (AtomMap.listKeys (#rules Grm));
    in
        while !sym <> [] do (
            let
                val x = hd(!sym);
                val rhs = ref (AtomSet.listItems ( AtomMap.lookup((#rules Grm), x )))
            in
                while !rhs <> [] do (
                        (* For each rhs production *)
                        if ((isProductionNullable (!rhs)) orelse (length(!rhs) = 0)) then
                            let
                                val t = (AtomMap.remove (!NULLABLE , x));
                            in
                                if (#2 t) = false then
                                    NULLABLE := (#1 t);
                                    NULLABLE := AtomMap.insert (!NULLABLE , x , true);
                                    modified := true
                                else ()
                            end
                        else    ();

                        let
                            val i = ref 0;
                            val j = ref 0;
                            val k = length(!rhs)
                        in
                            while !i < k do (
                                if ((!i = 0) orelse (isProductionNullable (List.take (!rhs, !i - 1)))) then
                                    let
                                        val yi = List.nth(!rhs ,!i);
                                        val t = (AtomMap.remove (!FIRST , x));
                                        val (mp , el) = (ref (#1 t ) , ref (#2 t))
                                    in
                                        FIRST := !mp;
                                        el :=  AtomSet.union(!el , AtomMap.lookup(!FIRST, yi));
                                        FIRST := AtomMap.insert (!FIRST , x , !el);

                                        if (AtomSet.equal (!el , (#2 t))) = false then
                                            modified := true    (* need check here *)
                                        else ()
                                    end;
                                else ();

                                if ((!i = k-1) orelse (isProductionNullable (List.drop (!rhs, !i)))) then
                                    let
                                        val yi = List.nth(!rhs , !i);
                                        val t = (AtomMap.remove (!FOLLOW , yi));
                                        val (mp , el) = (ref (#1 t ) , ref (#2 t))
                                    in
                                        FOLLOW := !mp;
                                        el :=  AtomSet.union(!el ,  AtomMap.lookup (!FOLLOW , x));
                                        FOLLOW := AtomMap.insert (!FOLLOW , yi , !el)

                                        if (AtomSet.equal (!el , (#2 t))) = false then
                                            modified := true    (* need check here *)
                                        else ()
                                    end;
                                else ();

                                j := !i + 1;
                                while !j < k do (
                                    if ((!i + 1 = !j) orelse (isProductionNullable ( List.drop(List.take(!rhs , !j - 1) , !i) ))) then
                                        let
                                            val yj = List.nth(!rhs, !j);
                                            val yi = List.nth(!rhs , !i);
                                            val t = (AtomMap.remove (!FOLLOW , yi));
                                            val (mp , el) = (ref (#1 t ) , ref (#2 t))
                                        in
                                            FOLLOW := !mp;
                                            el :=  AtomSet.union(!el , AtomMap.lookup(!FIRST, yj));
                                            FOLLOW := AtomMap.insert (!FOLLOW , yi , !el);

                                            if (AtomSet.equal (!el , (#2 t))) = false then
                                                modified := true    (* need check here *)
                                            else ()
                                        end;
                                    else ();

                                    j := !j + 1
                                );
                                i := !i + 1
                            )
                        end
                    ;
                    rhs := tl(!rhs)
                )
            end
            ;
            sym := tl(!sym)
        )
    end;

    if (!modified = true) then
        repeat := true
    else repeat := false
)
