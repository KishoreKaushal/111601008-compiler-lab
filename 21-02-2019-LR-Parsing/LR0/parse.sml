use "./type.sml";
use "./grammar.sml";

val stateCounter : int ref = ref 0;
val prodCounter : int ref = ref 0;
val stateIdx : StateMapToInt ref = ref StateMap.empty;
val prodIdx : ProdMapToInt ref = ref ProdMap.empty;

(* functions for mapping productions to int *)

fun mapProdToIntProcessRhsLst ([], X) = ()
|   mapProdToIntProcessRhsLst (rhs::rhsLst, X) = (
    let 
        val prod : simpleProd = {
            left = X,
            right = rhs
        } 
    in 
        prodCounter := !prodCounter + 1;
        prodIdx := ProdMap.insert(!prodIdx, prod, !prodCounter);
        (* printing the production mapping *)
        print ("========== Production: "^Int.toString(!prodCounter)^" ==========");
        printSimpleProd(prod);
        print "\n";
        mapProdToIntProcessRhsLst(rhsLst,X)
    end 
)

fun mapProdToIntProcessRulesLst ([]) = ()
|   mapProdToIntProcessRulesLst (r::rulesLst) 
=   let 
        val (X, rhsSet) = r
    in 
        mapProdToIntProcessRhsLst(RHSSet.listItems(rhsSet), X);
        mapProdToIntProcessRulesLst(rulesLst)
    end    

fun mapProdToInt (Grm : Grammar) = (
    let 
        val rules = (#rules Grm);
        val rulesLst = AtomMap.listItemsi(rules)
    in
        mapProdToIntProcessRulesLst(rulesLst)
    end
);

mapProdToInt(Grm);

(* functions for computing closure *)

fun closureInnerLoopHelper ([], X : Atom.atom, I : State ref) = ()
|   closureInnerLoopHelper (prod::prodList, X : Atom.atom, I : State ref) 
=   let
        val newIt : Item = {
            lhs = X,
            bef = [],
            aft = prod
        }
    in 
        I := ItemSet.add(!I, newIt);
        closureInnerLoopHelper(prodList, X, I)
    end

fun closureProcessItem (It : Item, I : State ref, Grm : Grammar)
=   let
        val { lhs, bef, aft } = It;
        val X = List.hd (aft) handle Empty => Atom.atom "";
        val {symbols, tokens, rules} = Grm
    in
        if (AtomSet.member(symbols, X)) then (
            let 
                val prodList = RHSSet.listItems( AtomMap.lookup (rules, X) 
                                                handle NotFound => RHSSet.empty)
            in 
                closureInnerLoopHelper(prodList, X, I)
            end
        ) else ()
    end

fun closureOuterLoopHelper ([] , I : State ref, Grm : Grammar) = ()
|   closureOuterLoopHelper (It::ItemList , I : State ref, Grm : Grammar) 
=   (closureProcessItem(It, I, Grm); closureOuterLoopHelper(ItemList, I, Grm))

fun closure (I : State ref, Grm : Grammar)
=   let 
        val itemList = ItemSet.listItems(!I);
        val initSet = (!I)
    in
        closureOuterLoopHelper(itemList, I, Grm);
        if (ItemSet.equal (initSet , !I) = false) then (closure (I, Grm); ())
        else ()
    end;

(* functions for computing goto and shift items *)

fun gotoProcessItem (It : Item,  J : State ref, X : Atom.atom)
=   let 
        val {lhs, bef, aft} = It;
        val aftHd = List.hd(aft) handle Empty => Atom.atom "";
        val newItem : Item = {
            lhs = lhs,
            bef = if(Atom.compare(Atom.atom "", aftHd) = EQUAL) then bef else aftHd::bef,
            aft = List.tl(aft) handle Empty => []
        }
    in
        if (Atom.same(aftHd,X)) then J := ItemSet.add (!J, newItem)
        else ()
    end

fun gotoLoopHelper ([] , J : State ref, X : Atom.atom) = ()
|   gotoLoopHelper (It::ItemList , J : State ref, X : Atom.atom)
=   (gotoProcessItem(It, J, X); gotoLoopHelper(ItemList, J, X))

fun goto (I : State, X : Atom.atom, Grm : Grammar)
=   let 
        val itemList = ItemSet.listItems(I);
        val J : State ref = ref ItemSet.empty
    in 
        gotoLoopHelper(itemList, J, X);
        closure(J, Grm);
        (!J)
    end;

(* computing the shift and goto actions *)

fun computeShiftAndGotoInnerLoopHelper ([], I : State, T : StateSet.set ref, E : EdgeSet.set ref, Grm : Grammar) = ()
|   computeShiftAndGotoInnerLoopHelper (It::ItemList, I : State, T : StateSet.set ref, E : EdgeSet.set ref, Grm : Grammar) = (
    let 
        val { lhs, bef, aft } = It
    in
        if (List.null(aft) = false) then (
            let 
                val X = List.hd (aft);
                val J = goto(I, X, Grm);
                val newEdge : Edge = {
                    from = I,
                    to = J,
                    on = X
                }
            in 
                if (StateMap.inDomain(!stateIdx, J) = false) then (
                    T := StateSet.add (!T, J);
                    (* updating global variables *)
                    stateCounter := !stateCounter + 1;
                    print("Inserting State : "^Int.toString(!stateCounter)^"\n");
                    stateIdx := StateMap.insert(!stateIdx, J, !stateCounter)
                ) else ();
                E := EdgeSet.add (!E, newEdge)
            end
        ) else ()
    end;
    computeShiftAndGotoInnerLoopHelper(ItemList, I, T, E, Grm)
)


fun computeShiftAndGotoProcessState (I : State, T : StateSet.set ref, E : EdgeSet.set ref, Grm : Grammar) = (
    let 
        val itemList = ItemSet.listItems(I);
    in
        computeShiftAndGotoInnerLoopHelper(itemList, I, T, E, Grm)
    end
)

fun computeShiftAndGotoOuterLoopHelper ([], T : StateSet.set ref, E : EdgeSet.set ref, Grm : Grammar) = ()
|   computeShiftAndGotoOuterLoopHelper (st::stLst, T : StateSet.set ref, E : EdgeSet.set ref, Grm : Grammar) 
= (computeShiftAndGotoProcessState(st, T, E, Grm); computeShiftAndGotoOuterLoopHelper(stLst, T, E, Grm))

fun computeShiftAndGotoHelper (T : StateSet.set ref, E : EdgeSet.set ref, Grm : Grammar)
= (
    let 
        val stateList = StateSet.listItems (!T);
        val initStateSet = (!T);
        val initEdgeSet = (!E);
    in 
        computeShiftAndGotoOuterLoopHelper(stateList, T, E, Grm);
        if ((StateSet.equal (initStateSet, !T) = false) 
                orelse 
            (EdgeSet.equal (initEdgeSet, !E) = false))
        then (computeShiftAndGotoHelper(T, E, Grm) ; ())
        else ()
    end
)

fun computeShiftAndGoto (T : StateSet.set ref, E : EdgeSet.set ref, 
startItem : Item, Grm : Grammar) = ( 
    E := EdgeSet.empty ;
    T := StateSet.empty ;
    let 
        val I : State ref = ref ItemSet.empty
    in 
        I := ItemSet.add (!I, startItem);
        closure(I, Grm);
        T := StateSet.add(!T, !I)
    end;
    (* Initialization done. *)
    computeShiftAndGotoHelper(T, E, Grm)
);

(* computing the reduce actions *)

fun computeReduceActionsProcessItems ([], I: State, R : ReduceActions ref) = ()
|   computeReduceActionsProcessItems (It::ItemList, I: State, R : ReduceActions ref) = (
    let 
        val { lhs, bef, aft } = It;
        val newProd : simpleProd = { left = lhs, right = (List.rev bef)};
        val newRedAct : ReduceAction = {state = I, prod = newProd}
    in 
        ( 
            if (List.null(aft)) then (
                R := ReduceActionSet.add (!R , newRedAct)
            ) else ()
        );
        computeReduceActionsProcessItems (ItemList, I, R)
    end 
)

fun computeReduceActionsProcessStates ([], R : ReduceActions ref) = ()
|   computeReduceActionsProcessStates (I::stateList, R : ReduceActions ref) = (
    let 
        val itemList = ItemSet.listItems(I);
    in 
        computeReduceActionsProcessItems (itemList, I, R);
        computeReduceActionsProcessStates (stateList, R)
    end
)

fun computeReduceActions (T : StateSet.set ref, R : ReduceActions ref) = (
    R := ReduceActionSet.empty ;
    let 
        val stateList = StateSet.listItems (!T)
    in 
        computeReduceActionsProcessStates(stateList, R)
    end 
)

(* LR0 parsing table *)

val T : StateSet.set ref = ref StateSet.empty;
val E : EdgeSet.set ref = ref EdgeSet.empty;
val R : ReduceActions ref = ref ReduceActionSet.empty;

computeShiftAndGoto(T, E, startItem, Grm);
computeReduceActions(T, R);

(* printing results *)

fun printFinalStatesHelper ([]) = (print "======== State List End ========\n")
|   printFinalStatesHelper (I::stateList) = (
    let val curr_state_idx = StateMap.lookup(!stateIdx,I) handle NotFound => ~1 in 
        print ("======== State Idx: " ^ Int.toString(curr_state_idx) ^ " ========\n");
        printItemSet(I);
        printFinalStatesHelper(stateList)
    end 
)

fun printFinalStates (T : StateSet.set ref) = (
    let 
        val stateList = StateSet.listItems (!T) 
    in 
        printFinalStatesHelper(stateList)
    end 
);

printFinalStates(T)