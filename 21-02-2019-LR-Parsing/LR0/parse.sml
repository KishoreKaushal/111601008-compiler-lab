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
        closureInnerLoopHelper(prodList, I)
    end

fun closureProcessItem (It : Item, I : ItemSet.set ref, Grm : Grammar ref)
=   let
        val { lhs, bef, aft } = It;
        val x = List.hd (aft);
        val {symbols, tokens, rules} = (#symbols (!Grm))
    in
        if (AtomSet.member(symbols, X)) then 
            let 
                val prodList = RHSSet.listItems( AtomMap.lookup (rules, X) 
                                                handle NotFound => RHSSet.empty)
            in 
                closureInnerLoopHelper(prodList, X, I)
            end
    end

fun closureOuterLoopHelper ([] , I : ItemSet.set ref, Grm : Grammar ref) = 
|   closureOuterLoopHelper (It::ItemList , I : ItemSet.set ref, Grm : Grammar ref) 
=   closureProcessItem(It, I, Grm); closureOuterLoopHelper(ItemList, I, Grm)


fun closure (I : ItemSet.set ref, Grm : Grammar ref)
=   let 
        val itemList = ItemSet.listItems(!I);
        val initSet = !I
    in
        closureOuterLoopHelper(itemList, I, Grm),
        if (ItemSet.equal (initSet , !I)) then closure (I, Grm)
    end