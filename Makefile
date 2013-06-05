# Makefile

.PHONY: all clean install tests jsure

PREFIX?=/usr/local

all: jsure

jsure:
	ocamlbuild -pkg dbm -classic-display jsure.byte

install: all
	install -m 0755 jsure.native $(PREFIX)/bin/jsure

clean:
	rm -rf _build _log jsure.byte

tests: jsure
	@cd tests; ./test.sh

top: 
	/opt/local/bin/ocamlc.opt -c -I /opt/local/lib/ocaml/site-lib/dbm -g -I +aurochs_lib -o pffpsf.cmo pffpsf.ml
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o ansi.cmi ansi.mli
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o ansi.cmo ansi.ml
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o conduit.cmi conduit.mli
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o conduit.cmo conduit.ml
	ocamlfind ocamlc          -c -g -I +aurochs_lib -package extlib -o ast.cmo ast.ml
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o liner.cmo liner.ml
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o source.cmo source.ml
	aurochs -quiet -target ml ecmarex.peg
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o opt.cmo opt.ml
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o pffpsf.cmo pffpsf.ml
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o ecmarex.cmi ecmarex.mli
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o ecmarex.cmo ecmarex.ml
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o excerpt.cmo excerpt.ml
	aurochs -quiet -target ml ecma.peg
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o ecma.cmi ecma.mli
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -custom -o ecma.cmo ecma.ml
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o qwerty.cmo qwerty.ml
	/opt/local/bin/ocamlc.opt -c -g -I +aurochs_lib -o levenshtein.cmo levenshtein.ml
	#ocamlfind ocamlc -package dbm  -c -g -I +aurochs_lib -o cache.cmo cache.ml
	#/opt/local/bin/ocamlc.opt -c -I /opt/local/lib/ocaml/site-lib/dbm -g -I +aurochs_lib -o check.cmo check.ml
	/opt/local/bin/ocamlc.opt -c -I /opt/local/lib/ocaml/site-lib/dbm -g -I +aurochs_lib -o convert.cmo convert.ml
	#/opt/local/bin/ocamlc.opt -c -I /opt/local/lib/ocaml/site-lib/dbm -g -I +aurochs_lib -o eval.cmo eval.ml
	#/opt/local/bin/ocamlc.opt -c -I /opt/local/lib/ocaml/site-lib/dbm -g -I +aurochs_lib -o generate.cmo generate.ml
	/opt/local/bin/ocamlc.opt -c -I /opt/local/lib/ocaml/site-lib/dbm -g -I +aurochs_lib -o minefield.cmo minefield.ml
	#/opt/local/bin/ocamlc.opt -c -I /opt/local/lib/ocaml/site-lib/dbm -g -I +aurochs_lib -o process.cmo process.ml
	/opt/local/bin/ocamlc.opt -c -I /opt/local/lib/ocaml/site-lib/dbm -g -I +aurochs_lib -o version.cmo version.ml
	#/opt/local/bin/ocamlc.opt -c -I /opt/local/lib/ocaml/site-lib/dbm -g -custom -I +aurochs_lib -pp camlp4o -o jsure.cmo jsure.ml
	utop -I /opt/local/lib/ocaml/site-lib/aurochs_lib/ -init top.ml


