use "./type.sml";
use "./grammar.sml";

val stateCounter : int ref = ref 0;
val stateIdx : StateMapToInt ref = ref StateMap.empty;

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
        val X = List.hd (aft);
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

(* Test for closure *)
(* 
val I : State ref = ref ItemSet.empty ;


val It : Item = {
    lhs = Atom.atom "E'",
    bef = List.map Atom.atom [] ,
    aft = List.map Atom.atom ["E", "$"] 
};

I := ItemSet.add (!I , It);

closure(I, Grm);

printItemSet(!I);   *)

fun gotoProcessItem (It : Item,  J : State ref, X : Atom.atom)
=   let 
        val {lhs, bef, aft} = It;
        val aftHd = List.hd(aft);
        val newItem : Item = {
            lhs = lhs,
            bef = List.hd(aft)::bef,
            aft = List.tl(aft)
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
        val { lhs, bef, aft } = It;
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
            stateIdx := StateMap.insert(!stateIdx, J, !stateCounter)
        ) else ();
        E := EdgeSet.add (!E, newEdge);
        computeShiftAndGotoInnerLoopHelper(ItemList, I, T, E, Grm)
    end
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