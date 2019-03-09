use "../type.sml";
use "../grammar.sml";

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
    end

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

fun goto (I : State ref, X : Atom.atom, Grm : Grammar)
=   let 
        val itemList = ItemSet.listItems(!I);
        val J : State ref = ref ItemSet.empty
    in 
        gotoLoopHelper(itemList, J, X);
        closure(J, Grm);
        (!J)
    end