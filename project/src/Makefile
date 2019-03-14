SHELL := /bin/bash

all: MinC_ast.sml MinC.grm MinC.lex MinCCompile.sml translate.sml source.mlb
	mllex MinC.lex
	mlyacc MinC.grm
	mlton source.mlb

clean:
	rm  -rf *.grm.desc *.grm.sig *.grm.sml *.lex.sml source

test: source
		bash test.sh