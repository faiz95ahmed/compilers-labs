# tools/Makefile

all : nodexp sshconfig pibake

nodexp : nclex.cmo ncparse.cmo ncmain.cmo
	ocamlc ../lib/common.cma $^ -o $@

nclex.ml : nclex.mll
	ocamllex nclex.mll

ncparse.mli ncparse.ml : ncparse.mly
	ocamlyacc -v ncparse.mly

DIR := $(shell pwd)

pibake: pibake.in sshconfig
	sed 's:@DIR@:$(DIR):g' $< >$@
	chmod +x $@

sshconfig: sshconfig.in
	sed 's:@DIR@:$(DIR):g' $< >$@

%.cmi: %.mli
	ocamlc $(MLFLAGS) -c $<

%.cmo: %.ml
	ocamlc $(MLFLAGS) -c $<

MLGEN = nclex.ml ncparse.mli ncparse.ml

ML = $(MLGEN) ncmain.ml

clean : force
	rm -f nodexp *.cmo *.cmi *.output $(MLGEN)
	rm -f sshconfig pibake

depend: $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

force:

MLFLAGS = -I ../lib

###

nclex.cmo : ncparse.cmi
nclex.cmx : ncparse.cmx
ncmain.cmo : ncparse.cmi nclex.cmo
ncmain.cmx : ncparse.cmx nclex.cmx
ncparse.cmo : ncparse.cmi
ncparse.cmx : ncparse.cmi
ncparse.cmi :
