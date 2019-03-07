type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end
fun IntFromString str = let val SOME x = Int.fromString str in x end
fun toStrConst str = String.substring (str , 1 , (String.size str) - 2)

%%
%s STR;
digit = [0-9] ;
eol = ("\n\r"|"\r\n"|"\r"|"\n") ;
whitespace = (" "|\t)  ;
letter = [a-zA-Z]   ;
esc = ("\a"|"\b"|"\f"|"\n"|"\r"|"\t"|"\v");
%%

<INITIAL> {eol} => (lineNum := !lineNum+1; 
                    linePos := yypos :: !linePos;
                    continue());

<INITIAL> {whitespace}+ => (continue());

<INITIAL> ":="        => (Tokens.ASSIGN (yypos, yypos+2));
<INITIAL> "&"         => (Tokens.AND (yypos, yypos+1));
<INITIAL> "|"         => (Tokens.OR (yypos, yypos+1));
<INITIAL> ">="        => (Tokens.GE (yypos, yypos+2));
<INITIAL> ">"         => (Tokens.GT (yypos, yypos+1));
<INITIAL> "<="        => (Tokens.LE (yypos, yypos+2));
<INITIAL> "<"         => (Tokens.LT (yypos, yypos+1));
<INITIAL> "!="        => (Tokens.NEQ (yypos, yypos+2));
<INITIAL> "="         => (Tokens.EQ (yypos, yypos+1));
<INITIAL> "/"         => (Tokens.DIVIDE (yypos, yypos+1));
<INITIAL> "*"         => (Tokens.TIMES (yypos, yypos+1));
<INITIAL> "-"         => (Tokens.MINUS (yypos, yypos+1));
<INITIAL> "+"         => (Tokens.PLUS (yypos, yypos+1));
<INITIAL> "."         => (Tokens.DOT (yypos, yypos+1));
<INITIAL> "}"         => (Tokens.RBRACE (yypos, yypos+1));
<INITIAL> "{"         => (Tokens.LBRACE (yypos, yypos+1));
<INITIAL> "]"         => (Tokens.RBRACK (yypos, yypos+1));
<INITIAL> "["         => (Tokens.LBRACK (yypos, yypos+1));
<INITIAL> ")"         => (Tokens.RPAREN (yypos, yypos+1));
<INITIAL> "("         => (Tokens.LPAREN (yypos, yypos+1));
<INITIAL> ";"         => (Tokens.SEMICOLON (yypos, yypos+1));
<INITIAL> ":"         => (Tokens.COLON (yypos, yypos+1));
<INITIAL> ","	      => (Tokens.COMMA (yypos, yypos+1));
<INITIAL> int         => (Tokens.INT (yypos, yypos+3));
<INITIAL> string      => (Tokens.STRING (yypos, yypos+6));
<INITIAL> {digit}+    => (Tokens.NUMCONST(IntFromString yytext, 
                            yypos, yypos + size yytext));

<INITIAL> \"(\(.|\n)|[^\\"\n])*\" => (Tokens.STRINGCONST(toStrConst yytext,
                                            yypos, yypos + size yytext));

<INITIAL> ({letter}|"_")(({letter}|{digit}|"_")*)
                        =>  (Tokens.ID(yytext, yypos, 
                            yypos + size yytext));

.           => (print("error"); print(yytext); continue());

