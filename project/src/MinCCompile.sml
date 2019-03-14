structure  MinCCompile =
struct

(* This three structure definitions are what the lexer and parser *)

structure MinCLrVals = MinCLrValsFun(structure Token = LrParser.Token) (* Generate the LR values structure *)
structure MinCLex    = MinCLexFun(structure Tokens = MinCLrVals.Tokens)
structure MinCParser = Join( structure ParserData = MinCLrVals.ParserData
			     structure Lex        = MinCLex
			     structure LrParser   = LrParser
			   )

(* Build Lexers *)
fun makeMinCLexer strm = MinCParser.makeLexer (fn n => TextIO.inputN(strm,n))
val makeFileLexer      = makeMinCLexer o TextIO.openIn

(* Parse command line and set a suitable lexer *)
(* If takes the file name as the input and parses that file
 if no file name is mentioned as the input then it reads the input from the terminal
 Otherwise it will give the error*)

val thisLexer = case CommandLine.arguments() of
		    []  => makeMinCLexer TextIO.stdIn
		 |  [x] => makeFileLexer x
		 |  _   => (TextIO.output(TextIO.stdErr, "usage: ec file"); OS.Process.exit OS.Process.failure)



fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr,
					    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

(* The portion of the code that does the actual compiling *)

val (program,_) = MinCParser.parse (0,thisLexer,print_error,())
val executable  = Translate.compile program

end
