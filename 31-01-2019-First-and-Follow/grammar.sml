val sym = ref AtomSet.empty ;
sym := AtomSet.add (!sym , Atom.atom "E") ;
sym := AtomSet.add (!sym , Atom.atom "E'") ;
sym := AtomSet.add (!sym , Atom.atom "T") ;
sym := AtomSet.add (!sym , Atom.atom "T'") ;
sym := AtomSet.add (!sym , Atom.atom "F") ;

val tok = ref AtomSet.empty ;
tok := AtomSet.add (!tok , Atom.atom "+") ;
tok := AtomSet.add (!tok , Atom.atom "*") ;
tok := AtomSet.add (!tok , Atom.atom "(") ;
tok := AtomSet.add (!tok , Atom.atom ")") ;
tok := AtomSet.add (!tok , Atom.atom "id") ;

val E_prod : Productions = RHSSet.fromList ([
        [Atom.atom "T" , Atom.atom "E'"]
    ])

val E_prime_prod : Productions = RHSSet.fromList ([
        [],
        [Atom.atom "+", Atom.atom "T", Atom.atom "E'"]
    ])

val T_prod : Productions = RHSSet.fromList ([
        [Atom.atom "F" , Atom.atom "T'"]
    ])

val T_prime_prod : Productions = RHSSet.fromList ([
        [],
        [Atom.atom "*", Atom.atom "F" , Atom.atom "T'"]
    ])

val F_prod : Productions = RHSSet.fromList ([
        [Atom.atom "id"],
        [Atom.atom "(" , Atom.atom "E", Atom.atom ")"]
    ])

val rul : Rules ref = ref AtomMap.empty ;
rul := AtomMap.insert (!rul , Atom.atom "E" , E_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "E'" , E_prime_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "T" , T_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "T'" , T_prime_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "F" , F_prod) ;

val Grm : Grammar = {
    symbols = !sym,
    tokens = !tok,
    rules = !rul
}
