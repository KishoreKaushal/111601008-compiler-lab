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
\\001\000\002\000\016\000\006\000\015\000\007\000\014\000\008\000\013\000\
\\009\000\012\000\000\000\
\\001\000\003\000\008\000\004\000\007\000\005\000\006\000\000\000\
\\001\000\005\000\018\000\000\000\
\\001\000\006\000\015\000\007\000\014\000\008\000\013\000\009\000\012\000\
\\013\000\025\000\000\000\
\\001\000\010\000\011\000\011\000\010\000\012\000\009\000\000\000\
\\027\000\000\000\
\\028\000\003\000\008\000\004\000\007\000\005\000\006\000\000\000\
\\029\000\000\000\
\\030\000\000\000\
\\031\000\000\000\
\\032\000\000\000\
\\033\000\006\000\015\000\007\000\014\000\000\000\
\\034\000\006\000\015\000\007\000\014\000\000\000\
\\035\000\000\000\
\\036\000\000\000\
\\037\000\000\000\
\\038\000\000\000\
\\039\000\000\000\
\"
val actionRowNumbers =
"\007\000\005\000\001\000\006\000\
\\016\000\014\000\009\000\002\000\
\\003\000\002\000\002\000\002\000\
\\002\000\002\000\007\000\004\000\
\\017\000\015\000\013\000\012\000\
\\011\000\010\000\008\000\018\000\
\\000\000"
val gotoT =
"\
\\001\000\024\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\015\000\004\000\001\000\000\000\
\\000\000\
\\003\000\017\000\004\000\001\000\000\000\
\\003\000\018\000\004\000\001\000\000\000\
\\003\000\019\000\004\000\001\000\000\000\
\\003\000\020\000\004\000\001\000\000\000\
\\003\000\021\000\004\000\001\000\000\000\
\\002\000\022\000\003\000\002\000\004\000\001\000\000\000\
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
\"
val numstates = 25
val numrules = 13
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
datatype svalue = VOID | ntVOID of unit ->  unit | ID of unit ->  (string) | STRING of unit ->  (string) | INT of unit ->  (int) | lvalue of unit ->  (Ast.lvalue) | exp of unit ->  (Ast.exp) | program of unit ->  (Ast.exp list) | init of unit ->  (Ast.exp list)
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
fn _ => false
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
  | (T 9) => "ASSIGN"
  | (T 10) => "DOT"
  | (T 11) => "LBRACK"
  | (T 12) => "RBRACK"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.program program1, program1left, program1right)) :: rest671)) => let val  result = MlyValue.init (fn _ => let val  (program as program1) = program1 ()
 in ((*#line 31.20 "tiger.grm"*)program(*#line 185.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, program1left, program1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.program (fn _ => ((*#line 33.17 "tiger.grm"*)[](*#line 191.1 "tiger.grm.sml"*)
))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.program program1, _, program1right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1) = exp1 ()
 val  (program as program1) = program1 ()
 in ((*#line 34.28 "tiger.grm"*)exp::program(*#line 195.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 1, ( result, exp1left, program1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = INT1 ()
 in ((*#line 36.38 "tiger.grm"*)Ast.INT(INT)(*#line 202.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, INT1left, INT1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 37.38 "tiger.grm"*)Ast.BINOP(exp1,Ast.PLUS,exp2)(*#line 208.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 38.22 "tiger.grm"*)Ast.BINOP(exp1,Ast.MINUS,exp2)(*#line 215.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 39.22 "tiger.grm"*)Ast.BINOP(exp1,Ast.TIMES,exp2)(*#line 222.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ((*#line 40.23 "tiger.grm"*)Ast.BINOP(exp1,Ast.DIVIDE,exp2)(*#line 229.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (STRING as STRING1) = STRING1 ()
 in ((*#line 41.17 "tiger.grm"*)Ast.STRING(STRING)(*#line 236.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, STRING1left, STRING1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( MlyValue.lvalue lvalue1, lvalue1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (lvalue as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 42.25 "tiger.grm"*)Ast.ASSIGNMENT(lvalue , exp)(*#line 242.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, lvalue1left, exp1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 44.19 "tiger.grm"*)Ast.LVAL_IDEN(ID)(*#line 249.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( MlyValue.lvalue lvalue1, lvalue1left, _)) :: rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (lvalue as lvalue1) = lvalue1 ()
 val  (ID as ID1) = ID1 ()
 in ((*#line 45.22 "tiger.grm"*)Ast.FIELD (lvalue,ID)(*#line 255.1 "tiger.grm.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, lvalue1left, ID1right), rest671)
end
|  ( 12, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.lvalue lvalue1, lvalue1left, _)) :: rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (lvalue as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in ((*#line 46.31 "tiger.grm"*)Ast.ELEMENT (lvalue,exp)(*#line 262.1 "tiger.grm.sml"*)
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
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
end
end
