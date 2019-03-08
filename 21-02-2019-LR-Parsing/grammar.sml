val sym = ref AtomSet.empty ;
sym := AtomSet.add (!sym , Atom.atom "E'") ;
sym := AtomSet.add (!sym , Atom.atom "E") ;
sym := AtomSet.add (!sym , Atom.atom "T") ;
sym := AtomSet.add (!sym , Atom.atom "F") ;

val tok = ref AtomSet.empty ;
tok := AtomSet.add (!tok , Atom.atom "+") ;
tok := AtomSet.add (!tok , Atom.atom "*") ;
tok := AtomSet.add (!tok , Atom.atom "(") ;
tok := AtomSet.add (!tok , Atom.atom ")") ;
tok := AtomSet.add (!tok , Atom.atom "id") ;

val E_prime_prod : Productions = RHSSet.fromList ([
        [Atom.atom "E", Atom.atom "$"]
    ])

val E_prod : Productions = RHSSet.fromList ([
        [Atom.atom "E", Atom.atom "T"],
        [Atom.atom "T"]
    ])

val T_prod : Productions = RHSSet.fromList ([
        [Atom.atom "T", Atom.atom "*", Atom.atom "F"],
        [Atom.atom "F"]
    ])

val F_prod : Productions = RHSSet.fromList ([
        [Atom.atom "id"],
        [Atom.atom "(" , Atom.atom "E", Atom.atom ")"]
    ])

val rul : Rules ref = ref AtomMap.empty ;
rul := AtomMap.insert (!rul , Atom.atom "E'" , E_prime_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "E" , E_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "T" , T_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "F" , F_prod) ;

val Grm : Grammar = {
    symbols = !sym,
    tokens = !tok,
    rules = !rul
}


val sym = ref AtomSet.empty ;
sym := AtomSet.add (!sym , Atom.atom "S") ;
sym := AtomSet.add (!sym , Atom.atom "E") ;
sym := AtomSet.add (!sym , Atom.atom "T") ;
sym := AtomSet.add (!sym , Atom.atom "F") ;


val tok = ref AtomSet.empty ;
tok := AtomSet.add (!tok , Atom.atom "$") ;
tok := AtomSet.add (!tok , Atom.atom "+") ;
tok := AtomSet.add (!tok , Atom.atom "-") ;
tok := AtomSet.add (!tok , Atom.atom "*") ;
tok := AtomSet.add (!tok , Atom.atom "/") ;
tok := AtomSet.add (!tok , Atom.atom "id") ;
tok := AtomSet.add (!tok , Atom.atom "num") ;
tok := AtomSet.add (!tok , Atom.atom "(") ;
tok := AtomSet.add (!tok , Atom.atom ")") ;

val S_prod : Productions = RHSSet.fromList ([
        [Atom.atom "E", Atom.atom "$"]
    ])

val E_prod : Productions = RHSSet.fromList ([
        [Atom.atom "E", Atom.atom "+", Atom.atom "T"],
        [Atom.atom "E", Atom.atom "-", Atom.atom "T"],
        [Atom.atom "T"]
    ])

val T_prod : Productions = RHSSet.fromList ([
        [Atom.atom "T", Atom.atom "*", Atom.atom "F"],
        [Atom.atom "T", Atom.atom "/", Atom.atom "F"],
        [Atom.atom "F"]
    ])

val F_prod : Productions = RHSSet.fromList ([
        [Atom.atom "id"],
        [Atom.atom "num"],
        [Atom.atom "(", Atom.atom "E", Atom.atom ")"]
    ])


val rul : Rules ref = ref AtomMap.empty ;
rul := AtomMap.insert (!rul , Atom.atom "S" , S_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "E" , E_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "T" , T_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "F" , F_prod) ;

val Grm : Grammar = {
    symbols = !sym,
    tokens = !tok,
    rules = !rul
} 