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
    bef     : the symbols/tokens before the dot in the rhs in reverse order
    aft     : the symbols/tokens after the dot

    for eg., 
    Item [A -> aA . bB] would be represented as:
    
    lhs     = atom "A"
    bef     = List.map atom ["A", "a"]
    aft     = List.map atom ["b", "B"]

    Note that the before is kept in reverse order. The advantage of this method is that 
    "moving the dot", when computing shift and gotos can be done in one step.    
*)
type Item = { lhs : Atom.atom, bef : Atom.atom list, aft : Atom.atom list }

fun compareItem (a : Item, b : Item)
=   let
        val cmp_lhs = Atom.lexCompare((#lhs a), (#lhs b));
        val cmp_before = compareAtomList((#bef a), (#bef b));
        val cmp_after = compareAtomList((#aft a), (#aft b))
    in
        if (cmp_lhs = EQUAL) then (
            if (cmp_before = EQUAL) then cmp_after
            else cmp_before
        ) else cmp_lhs
    end

structure ITEM_KEY : ORD_KEY = struct
    type ord_key = Item
    fun compare (a : Item, b : Item) = compareItem(a,b)
end

structure ItemSet = RedBlackSetFn (ITEM_KEY)

type State = ItemSet.set

fun compareItemSet(a : State, b : State) = ItemSet.compare(a,b)

structure STATE_KEY : ORD_KEY = struct 
    type ord_key = State
    fun compare (a : State, b : State) = compareItemSet(a,b)
end

structure StateSet = RedBlackSetFn (STATE_KEY)

type Edge = {from : State, to : State, on : Atom.atom}

fun compareEdge (a : Edge, b : Edge)
=   let 
        val cmp_from = compareItemSet((#from a) , (#from b))
        val cmp_to = compareItemSet((#to a) , (#to b))
        val cmp_on = Atom.lexCompare ((#on a), (#on b))
    in 
        if (cmp_from = EQUAL) then (
            if (cmp_to = EQUAL) then cmp_on
            else cmp_to
        ) else cmp_from
    end

structure EDGE_KEY : ORD_KEY = struct
    type ord_key = Edge
    fun compare (a : Edge, b : Edge) = compareEdge (a, b)
end

structure EdgeSet = RedBlackSetFn (EDGE_KEY)

type simpleProd = { left : Atom.atom, right: Atom.atom list }

fun compareSimpleProd (a : simpleProd, b : simpleProd) = (
    let 
        val cmp_left = Atom.compare((#left a), (#left b))
        val cmp_right = compareAtomList((#right a), (#right b))
    in 
        if (cmp_left = EQUAL) then cmp_right
        else cmp_left
    end 
)

structure PROD_KEY : ORD_KEY = struct 
    type ord_key = simpleProd
    fun compare (a : simpleProd, b : simpleProd) = compareSimpleProd(a,b)
end 

type ReduceAction = { state: State, prod: simpleProd }

fun compareRedAction (a : ReduceAction, b: ReduceAction) = (
    let 
        val cmp_state = compareItemSet ((#state a), (#state b)) 
        val cmp_prod = compareSimpleProd((#prod a), (#prod b))
    in 
        if (cmp_state = EQUAL) then cmp_prod
        else cmp_state
    end 
)

structure REDACTION_KEY : ORD_KEY = struct 
    type ord_key = ReduceAction
    fun compare (a : ReduceAction, b: ReduceAction) = compareRedAction(a, b)
end

structure ReduceActionSet = RedBlackSetFn (REDACTION_KEY)

type ReduceActions = ReduceActionSet.set

structure StateMap = RedBlackMapFn(STATE_KEY)

type StateMapToInt = int StateMap.map

structure ProdMap = RedBlackMapFn(PROD_KEY)

type ProdMapToInt = int ProdMap.map;

(* LR0 *)

structure LR0_KEY : ORD_KEY = struct
    (* complete this *)
    type ord_key = (int * Atom.atom)
    fun compare ((a:ord_key , b:ord_key))   = case Int.compare((#1 a), (#1 b)) of 
                                                EQUAL => (Atom.lexCompare((#2 a), (#2 b)) )
                                            |   LESS => LESS 
                                            |   GREATER => GREATER
end

structure Lr0TableMap = RedBlackMapFn (LR0_KEY)

type Lr0TableMapToAtomSet = AtomSet.set Lr0TableMap.map

(**********************HELPER FUNCTIONS BELOW************************)

fun printAtomListInItem ([]) = ()
|   printAtomListInItem (at::atmList)
=   let val str = Atom.toString (at) in print (str^" "); printAtomListInItem(atmList) end

fun printItem (It : Item)
=   let 
        val { lhs, bef, aft } = It
    in  
        print (Atom.toString(lhs)); 
        print (" -> ");
        printAtomListInItem( List.rev (bef)); 
        print (" . ");
        printAtomListInItem(aft)
    end 

fun printItemList ([]) = ()
|   printItemList (It::itemList)
=   (printItem(It); print("\n"); printItemList(itemList))

fun printItemSet (I : ItemSet.set)
=   let val itemList = ItemSet.listItems(I) in printItemList(itemList) end 

fun printSimpleProd (sProd : simpleProd) = (
    let 
        val {right , left} = sProd
    in 
        print (Atom.toString(left));
        print (" -> ");
        printAtomListInItem(right)
    end 
)