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
                        initialize_nullable sym_list );

fun isNullable s = (AtomMap.lookup (!NULLABLE, s) handle NotFound => false);
fun isProductionNullable rhs = (List.all isNullable rhs);

fun printAtomList (x::xs) = (print ((Atom.toString(x)) ^ " " ); printAtomList xs)
|   printAtomList []    = (print "\n");

fun printAtomSet at_set = (printAtomList(AtomSet.listItems(at_set)))

fun printNullableHelper [] = (print "=== ======== ===\n")
|   printNullableHelper (x::xs) = (let
                                    val (k , v) = x
                                in
                                    (print ((Atom.toString k) ^ " : " ^ (Bool.toString v) ^ "\n");
                                    printNullableHelper xs)
                                end);

fun printNullable () =  (let
                            val nullable_lst = AtomMap.listItemsi (!NULLABLE)
                        in
                            (print ("\n=== NULLABLE ===\n");
                            printNullableHelper nullable_lst)
                        end);



fun printFirstFollowHelper [] =  (print "=== ======== ===\n")
|   printFirstFollowHelper (x::xs) = (let
                                        val (k , v) = x
                                    in
                                        (print ((Atom.toString k) ^ " : " );
                                        printAtomSet(v);
                                        printFirstFollowHelper xs)
                                    end);

fun printFollow () = (let
                            val follow_lst = AtomMap.listItemsi (!FOLLOW)
                        in
                            (print ("\n=== FOLLOW ===\n");
                            printFirstFollowHelper follow_lst)
                        end);

fun printFirst () = (let
                            val first_lst = AtomMap.listItemsi (!FIRST)
                        in
                            (print ("\n=== FIRST ===\n");
                            printFirstFollowHelper first_lst)
                        end);


printNullable ();
printFirst ();
printFollow ();

val repeat : bool ref = ref true;
val modified : bool ref = ref true;
(* val itr : int ref = ref 0; *)

initialize ();

(* Loop Until the FIRST , FOLLOW and NULLABLE converges *)

while (!repeat = true) do (
    (* itr := !itr + 1;
    print (Int.toString(!itr) ^ "\n"); *)

    modified := false;

    let
        val sym = ref (AtomMap.listKeys (#rules Grm));
    in
        (* (print "Symbols: "; printAtomList (!sym)); *)

        while (List.null(!sym) = false) do (

            let
                val x = hd(!sym);
                val prods = ref (RHSSet.listItems ( AtomMap.lookup((#rules Grm) , x ) handle NotFound => RHSSet.empty ))
            in
                (* (print ("Current Head: " ^ (Atom.toString x) ^ " ")); *)
                while (List.null(!prods) = false) do (
                    let
                        val rhs = ref (List.hd(!prods))
                    in
                        (* (print "\nRHS: "; printAtomList (!rhs)); *)
                        (* For each rhs production *)
                        if ((length(!rhs) = 0) orelse (isProductionNullable (!rhs))) then
                            let
                                val t = (AtomMap.remove (!NULLABLE , x) handle LibBase.NotFound => (print ((Atom.toString x) ^ "\n") ; (AtomMap.empty, false)))
                            in
                                if (#2 t) = false then (
                                    NULLABLE := (#1 t);
                                    NULLABLE := AtomMap.insert (!NULLABLE , x , true);
                                    modified := true
                                ) else ()
                            end
                        else    ();

                        let
                            val i = ref 0;
                            val j = ref 0;
                            val k = length(!rhs)
                        in
                            while !i < k do (
                                if ((!i = 0) orelse (isProductionNullable (List.take (!rhs, !i )))) then (
                                    let
                                        val yi = List.nth(!rhs ,!i);
                                        val t = ((AtomMap.remove (!FIRST , x)) handle LibBase.NotFound => (!FIRST, AtomSet.empty));
                                        val (mp , el) = (ref (#1 t ) , ref (#2 t))
                                    in
                                        FIRST := !mp;
                                        el :=  AtomSet.union(!el , AtomMap.lookup(!FIRST, yi) handle NotFound => AtomSet.empty);
                                        FIRST := AtomMap.insert (!FIRST , x , !el);

                                        if ((AtomSet.equal (!el , (#2 t))) = false) then
                                            modified := true    (* need check here *)
                                        else ()
                                    end
                                ) else ();

                                if ((!i = k-1) orelse (isProductionNullable (List.drop (!rhs, !i + 1)))) then (
                                    let
                                        val yi = List.nth(!rhs , !i);
                                        val t = (AtomMap.remove (!FOLLOW , yi) handle LibBase.NotFound => (!FOLLOW, AtomSet.empty));
                                        val (mp , el) = (ref (#1 t ) , ref (#2 t))
                                    in
                                        FOLLOW := !mp;
                                        (* print ("follow on removing : " ^ (Atom.toString yi));
                                        printFollow(); *)

                                        el :=  AtomSet.union(!el ,  AtomMap.lookup (!FOLLOW , x) handle NotFound => AtomSet.empty);
                                        FOLLOW := AtomMap.insert (!FOLLOW , yi , !el);

                                        (* print ("follow on inserting : " ^ (Atom.toString yi));
                                        printFollow(); *)


                                        if ((AtomSet.equal (!el , (#2 t))) = false) then
                                            modified := true    (* need check here *)
                                        else ()
                                    end
                                ) else ();

                                j := !i + 1;
                                while !j < k do (
                                    if ((!i + 1 = !j) orelse (isProductionNullable ( List.drop(List.take(!rhs , !j ) , !i + 1) ))) then (
                                        let
                                            val yj = List.nth(!rhs, !j);
                                            val yi = List.nth(!rhs , !i);
                                            val t = (AtomMap.remove (!FOLLOW , yi) handle LibBase.NotFound => (!FOLLOW, AtomSet.empty));
                                            val (mp , el) = (ref (#1 t ) , ref (#2 t))
                                        in
                                            FOLLOW := !mp;
                                            el :=  AtomSet.union(!el , AtomMap.lookup(!FIRST, yj) handle NotFound => AtomSet.empty);
                                            FOLLOW := AtomMap.insert (!FOLLOW , yi , !el);

                                            if ((AtomSet.equal (!el , (#2 t))) = false) then
                                                modified := true    (* need check here *)
                                            else ()
                                        end
                                    ) else ();

                                    j := !j + 1
                                );
                                i := !i + 1
                            )
                        end
                    end;
                    prods := tl(!prods)
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
;
printNullable();
printFirst();
printFollow();
