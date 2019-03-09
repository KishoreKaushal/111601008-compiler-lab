fun compareAtomList (a , b) 
    = case (a , b) of
        ([], [])    => EQUAL
    |   ([], x::xs) => LESS
    |   (x::xs, []) => GREATER
    |   (x::xs, y::ys)
                    =>  let
                            val temp = Atom.lexCompare(x,y)
                        in
                            case temp of
                                EQUAL   => compareAtomList(xs , ys)
                            |   GREATER => GREATER
                            |   LESS    => LESS
                        end

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
    fun compare (a , b)  = compareAtomList (a,b)
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

(*
    lhs     : the left hand side
    before  : the symbols/tokens before the dot in the rhs in reverse order
    after   : the symbols/tokens after the dot

    for eg., 
    Item [A -> aA . bB] would be represented as:
    
    lhs     = atom "A"
    before  = List.map atom ["A", "a"]
    after   = List.map atom ["b", "B"]

    Note that the before is kept in reverse order. The advantage of this method is that 
    "moving the dot", when computing shift and gotos can be done in one step.    
*)
type Item = { lhs : Atom.atom, before : Atom.atom list, after : Atom.atom list }

structure ITEM_KEY : ORD_KEY = struct
    (* complete this *)
    type ord_key = Item
    fun compare (a : Item, b : Item)
                    =   let
                            val cmp_lhs = Atom.lexCompare((#lhs a), (#lhs b));
                            val cmp_before = compareAtomList((#before a), (#before b));
                            val cmp_after = compareAtomList((#after a), (#after b))
                        in
                            if (cmp_lhs = EQUAL) then (
                                if (cmp_before = EQUAL) then cmp_after
                                else cmp_before
                            ) else cmp_lhs
                        end
end

structure ItemSet = RedBlackSetFn (ITEM_KEY)

fun printAtomListInItem ([]) = ()
|   printAtomListInItem (at::atmList)
=   let val str = Atom.toString (at) in print (str); printAtomListInItem(atmList) end

fun printItem (It : Item)
=   let val { lhs, before, after } = It 
    in  print(Atom.toString(lhs)); print(" -> "); 
        printAtomListInItem(List.rev(before)); 
        print (".");
        printAtomListInItem(after)
    end 

fun printItemList ([]) = ()
|   printItemList (It::itemList)
=   printItem(It); printItemList(itemList)

fun printItemSet(I : ItemSet) 
=   let val itemList = ItemSet.listItems(I) in printItemList(itemList) end 
