use "../type.sml";
use "../grammar.sml";

fun closureInnerLoopHelper ([], X : Atom.atom, I : ItemSet.set ref) = ()
|   closureInnerLoopHelper (prod::prodList, X : Atom.atom, I : ItemSet.set ref) 
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

fun closureProcessItem (It : Item, I : ItemSet.set ref, Grm : Grammar)
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

fun closureOuterLoopHelper ([] , I : ItemSet.set ref, Grm : Grammar) = ()
|   closureOuterLoopHelper (It::ItemList , I : ItemSet.set ref, Grm : Grammar) 
=   (closureProcessItem(It, I, Grm); closureOuterLoopHelper(ItemList, I, Grm))

fun closure (I : ItemSet.set ref, Grm : Grammar)
=   let 
        val itemList = ItemSet.listItems(!I);
        val initSet = (!I)
    in
        closureOuterLoopHelper(itemList, I, Grm);
        if (ItemSet.equal (initSet , !I)) then (closure (I, Grm); ())
        else ()
    end

(* Test for closure *)
val I : ItemSet.set ref = ref ItemSet.empty ;


val It : Item = {
    lhs = Atom.atom "E'",
    bef = List.map Atom.atom [] ,
    aft = List.map Atom.atom ["E", "$"] 
};

I := ItemSet.add (!I , It);

closure(I, Grm);

printItemSet(!I); 