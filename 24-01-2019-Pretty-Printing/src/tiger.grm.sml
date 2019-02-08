functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "tiger.grm"*)
(*#line 12.1 "tiger.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\017\000\006\000\016\000\007\000\015\000\008\000\014\000\
\\009\000\013\000\000\000\
\\001\000\003\000\009\000\004\000\008\000\005\000\007\000\015\000\006\000\000\000\
\\001\000\005\000\023\000\000\000\
\\001\000\005\000\032\000\000\000\
\\001\000\005\000\033\000\000\000\
\\001\000\005\000\043\000\000\000\
\\001\000\006\000\016\000\007\000\015\000\008\000\014\000\009\000\013\000\
\\014\000\034\000\000\000\
\\001\000\010\000\038\000\000\000\
\\001\000\011\000\012\000\012\000\011\000\013\000\010\000\000\000\
\\001\000\011\000\037\000\000\000\
\\001\000\016\000\031\000\000\000\
\\001\000\017\000\039\000\000\000\
\\046\000\000\000\
\\047\000\003\000\009\000\004\000\008\000\005\000\007\000\015\000\006\000\000\000\
\\048\000\000\000\
\\049\000\000\000\
\\050\000\000\000\
\\051\000\000\000\
\\052\000\006\000\016\000\007\000\015\000\000\000\
\\053\000\006\000\016\000\007\000\015\000\000\000\
\\054\000\000\000\
\\055\000\000\000\
\\056\000\000\000\
\\057\000\018\000\021\000\019\000\020\000\000\000\
\\058\000\000\000\
\\059\000\000\000\
\\060\000\006\000\016\000\007\000\015\000\008\000\014\000\009\000\013\000\000\000\
\\061\000\000\000\
\\062\000\003\000\009\000\004\000\008\000\005\000\007\000\015\000\006\000\000\000\
\\063\000\002\000\040\000\006\000\016\000\007\000\015\000\008\000\014\000\
\\009\000\013\000\000\000\
\\064\000\000\000\
\\065\000\000\000\
\\066\000\000\000\
\\067\000\000\000\
\"
val actionRowNumbers =
"\014\000\009\000\001\000\013\000\
\\024\000\032\000\021\000\016\000\
\\002\000\003\000\002\000\002\000\
\\002\000\002\000\002\000\014\000\
\\024\000\011\000\004\000\005\000\
\\007\000\033\000\022\000\020\000\
\\019\000\018\000\017\000\015\000\
\\025\000\029\000\010\000\008\000\
\\034\000\012\000\030\000\002\000\
\\006\000\023\000\029\000\027\000\
\\026\000\028\000\031\000\000\000"
val gotoT =
"\
\\001\000\043\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\017\000\008\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\020\000\004\000\001\000\000\000\
\\000\000\
\\003\000\022\000\004\000\001\000\000\000\
\\003\000\023\000\004\000\001\000\000\000\
\\003\000\024\000\004\000\001\000\000\000\
\\003\000\025\000\004\000\001\000\000\000\
\\003\000\026\000\004\000\001\000\000\000\
\\002\000\027\000\003\000\002\000\004\000\001\000\000\000\
\\005\000\028\000\008\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\034\000\004\000\001\000\006\000\033\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\039\000\004\000\001\000\000\000\
\\007\000\040\000\000\000\
\\000\000\
\\003\000\034\000\004\000\001\000\006\000\042\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 44
val numrules = 22
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit | ID of unit ->  (string) | STRING of unit ->  (string) | INT of unit ->  (int) | dec of unit ->  (Ast.dec) | ty of unit ->  (Ast.ty) | exps of unit ->  (Ast.exp list) | decs of unit ->  (Ast.dec list) | lvalue of unit ->  (Ast.lvalue) | exp of unit ->  (Ast.exp) | program of unit ->  (Ast.exp list) | init of unit ->  (Ast.exp list)
end
type svalue = MlyValue.svalue
type result = Ast.exp list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 14) => true | (T 15) => true | (T 16) => true | (T 17) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "SEMICOLON"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "ID"
  | (T 5) => "PLUS"
  | (T 6) => "MINUS"
  | (T 7) => "TIMES"
  | (T 8) => "DIVIDE"
  | (T 9) => "EQ"
  | (T 10) => "ASSIGN"
  | (T 11) => "DOT"
  | (T 12) => "LBRACK"
  | (T 13) => "RBRACK"
  | (T 14) => "LET"
  | (T 15) => "IN"
  | (T 16) => "END"
  | (T 17) => "TYPE"
  | (T 18) => "VAR"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.program program1, program1left, program1right)) :: rest671)) => let val  result = MlyValue.init (fn _ => let val  (program as program1) = program1 ()
 in ((*#line 40.22 "tiger.grm"*)program(*#line 231.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, program1left, program1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.program (fn _ => ((*#line 42.18 "tiger.grm"*)[](*#line 237.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.program program1, _, program1right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1) = exp1 ()
 val  (program as program1) = program1 ()
 in ((*#line 43.29 "tiger.grm"*)exp::program(*#line 241.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 1, ( result, exp1left, program1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = INT1 ()
 in ((*#line 45.38 "tiger.grm"*)Ast.INT(INT)(*#line 248.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, INT1left, INT1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 46.38 "tiger.grm"*)Ast.BINOP(exp1,Ast.PLUS,exp2)(*#line 254.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 47.22 "tiger.grm"*)Ast.BINOP(exp1,Ast.MINUS,exp2)(*#line 261.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 48.22 "tiger.grm"*)Ast.BINOP(exp1,Ast.TIMES,exp2)(*#line 268.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 49.23 "tiger.grm"*)Ast.BINOP(exp1,Ast.DIVIDE,exp2)(*#line 275.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (STRING as STRING1) = STRING1 ()
 in ((*#line 50.17 "tiger.grm"*)Ast.STRING(STRING)(*#line 282.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, STRING1left, STRING1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.lvalue lvalue1, lvalue1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (lvalue as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 51.25 "tiger.grm"*)Ast.ASSIGNMENT(lvalue , exp)(*#line 288.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, lvalue1left, exp1right), rest671)
end
|  ( 10, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.exps exps1, _, _)) :: _ :: ( _, ( MlyValue.decs decs1, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (decs as decs1) = decs1 ()
 val  (exps as exps1) = exps1 ()
 in ((*#line 52.27 "tiger.grm"*)Ast.LET(decs , exps)(*#line 295.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, LET1left, END1right), rest671)
end
|  ( 11, ( rest671)) => let val  result = MlyValue.decs (fn _ => ((*#line 54.15 "tiger.grm"*)[](*#line 302.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result = MlyValue.decs (fn _ => let val  (dec as dec1) = dec1 ()
 val  (decs as decs1) = decs1 ()
 in ((*#line 55.18 "tiger.grm"*)dec::decs(*#line 306.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, dec1left, decs1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in ((*#line 57.25 "tiger.grm"*)Ast.TYPE(ID , ty)(*#line 313.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 7, ( result, TYPE1left, ty1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 58.25 "tiger.grm"*)Ast.VARDEC(ID, exp)(*#line 320.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 7, ( result, VAR1left, exp1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.ty (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 60.16 "tiger.grm"*)Ast.TYPEID(ID)(*#line 327.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 16, ( rest671)) => let val  result = MlyValue.exps (fn _ => ((*#line 62.16 "tiger.grm"*)[](*#line 333.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)) => let val  result = MlyValue.exps (fn _ => let val  (exp as exp1) = exp1 ()
 in ((*#line 63.15 "tiger.grm"*)exp(*#line 337.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 5, ( result, exp1left, exp1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exps exps1, _, exps1right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exps (fn _ => let val  (exp as exp1) = exp1 ()
 val  (exps as exps1) = exps1 ()
 in ((*#line 64.27 "tiger.grm"*)exp::exps(*#line 343.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 5, ( result, exp1left, exps1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 66.19 "tiger.grm"*)Ast.LVAL_IDEN(ID)(*#line 350.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( MlyValue.lvalue lvalue1, lvalue1left, _)) :: rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (lvalue as lvalue1) = lvalue1 ()
 val  (ID as ID1) = ID1 ()
 in ((*#line 67.22 "tiger.grm"*)Ast.FIELD (lvalue,ID)(*#line 356.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, lvalue1left, ID1right), rest671)
end
|  ( 21, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.lvalue lvalue1, lvalue1left, _)) :: rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (lvalue as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 68.31 "tiger.grm"*)Ast.ELEMENT (lvalue,exp)(*#line 363.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, lvalue1left, RBRACK1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.init x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.ID (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
end
end
