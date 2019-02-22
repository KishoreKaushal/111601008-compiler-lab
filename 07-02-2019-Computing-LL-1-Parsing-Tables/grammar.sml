val sym = ref AtomSet.empty ;
sym := AtomSet.add (!sym , Atom.atom "X") ;
sym := AtomSet.add (!sym , Atom.atom "Y") ;
sym := AtomSet.add (!sym , Atom.atom "Z") ;

val tok = ref AtomSet.empty ;
tok := AtomSet.add (!tok , Atom.atom "a") ;
tok := AtomSet.add (!tok , Atom.atom "c") ;
tok := AtomSet.add (!tok , Atom.atom "d") ;

val X_prod : Productions = RHSSet.fromList ([
        [Atom.atom "a"],
        [Atom.atom "Y"]
    ])

val Y_prod : Productions = RHSSet.fromList ([
        [],
        [Atom.atom "c"]
    ])

val Z_prod : Productions = RHSSet.fromList ([
        [Atom.atom "d"],
        [Atom.atom "X" , Atom.atom "Y", Atom.atom "Z"]
    ])

val rul : Rules ref = ref AtomMap.empty ;
rul := AtomMap.insert (!rul , Atom.atom "X" , X_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "Y" , Y_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "Z" , Z_prod) ;

val Grm : Grammar = {
    symbols = !sym,
    tokens = !tok,
    rules = !rul
}


(* val sym = ref AtomSet.empty ;
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
} *)


(* val sym = ref AtomSet.empty ;
sym := AtomSet.add (!sym , Atom.atom "S") ;
sym := AtomSet.add (!sym , Atom.atom "A") ;
sym := AtomSet.add (!sym , Atom.atom "B") ;
sym := AtomSet.add (!sym , Atom.atom "C") ;


val tok = ref AtomSet.empty ;
tok := AtomSet.add (!tok , Atom.atom "d") ;

val S_prod : Productions = RHSSet.fromList ([
        [Atom.atom "A"]
    ])

val A_prod : Productions = RHSSet.fromList ([
        [Atom.atom "B"]
    ])

val B_prod : Productions = RHSSet.fromList ([
        [Atom.atom "C"]
    ])

val C_prod : Productions = RHSSet.fromList ([
        [Atom.atom "d"]
    ])


val rul : Rules ref = ref AtomMap.empty ;
rul := AtomMap.insert (!rul , Atom.atom "S" , S_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "A" , A_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "B" , B_prod) ;
rul := AtomMap.insert (!rul , Atom.atom "C" , C_prod) ;

val Grm : Grammar = {
    symbols = !sym,
    tokens = !tok,
    rules = !rul
} *)
