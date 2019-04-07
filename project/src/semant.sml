structure Target = struct type exp = unit end

structure Semant =
struct

    type venv = Env.enventry Symbol.table
    type tenv = T.ty Symbol.table
    type expty = {exp: Target.exp, ty: T.ty}

end