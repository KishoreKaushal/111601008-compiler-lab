val sym = ref AtomSet.empty
sym := AtomSet.add (!sym , "X")
sym := AtomSet.add (!sym , "Y")
sym := AtomSet.add (!sym , "Z")

val tok = ref AtomSet.empty
tok := AtomSet.add (!tok , "a")
tok := AtomSet.add (!tok , "c")
tok := AtomSet.add (!tok , "d")

val p = AtomMap.empty
val prod = RHSSet.fromList ([[Atom.atom ("a")]])
val p = AtomMap.insert (p, Atom.atom ("X"), prod)

val rul : Rules ref =

val Grm : Grammar = {
    symbols = !sym,
    tokens = !tok,
    rules = !rul
}
