OCAMLC=ocamlfind  c
OCAMLOPT=ocamlfind opt
OCAMLDEP=ocamlfind dep
INCLUDES=
SHAREDLIBS=str.cma unix.cma graph.cma
SHAREDLIBSOPT=str.cmxa unix.cmxa graph.cmxa
OCAMLFLAGS=-g -package ocamlgraph -linkpkg
OCAMLOPTFLAGS=
OUTFILE=murlock

# The list of object files for prog1

PROG1_OBJS_LIBS=log.cmo utils.cmo lang.cmo parser.cmo lexer.cmo proprules.cmo predRules.cmo bigseq.cmo ans_dic.cmo config.cmo unif.cmo prover2.cmo
PROG1_OBJS=$(PROG1_OBJS_LIBS) main.cmo

PROG_OBJS_OPT=log.cmx utils.cmx lang.cmx parser.cmx lexer.cmx proprules.cmx predRules.cmx bigseq.cmx ans_dic.cmo config.cmx unif.cmx prover2.cmx main.cmx

all: core lexyacc depend prog

core: 
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c log.mli
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c log.ml
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c utils.mli
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c utils.ml
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c lang.mli
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c lang.ml
# the separation of `core` part needed cause I have a problems with
# building.  If we make `lexyacc` first then we don't compile parser.cmo
# cause .depend doesn't containg information about it
# Maybe is OK to add new lines to .depend manually?

opt: lexyacc depend prog_opt


depend:
		ocamldep *.ml *.mli > .depend

lexyacc:
		ocamllex lexer.mll
		ocamlyacc parser.mly
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c parser.mli
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c parser.ml
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c lexer.ml

prog:  $(PROG1_OBJS)
		$(OCAMLC) $(OCAMLFLAGS) -g -o $(OUTFILE) $(INCLUDES) $(SHAREDLIBS) $(PROG1_OBJS)
		#$(OCAMLC) $(OCAMLFLAGS) -g $(INCLUDES)  $(SHAREDLIBS) $(PROG1_OBJS_LIBS) testunif.ml -o testunif

prog_opt: $(PROG_OBJS_OPT)
		$(OCAMLOPT) $(OCAMLFAGS) -o $(OUTFILE) $(INCLUDES) $(SHAREDLIBSOPT) $(PROG_OBJS_OPT)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mly.cmo:
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c $<

.ml.cmo:
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c $<

.mli.cmi:
		$(OCAMLC) $(INCLUDES) $(OCAMLFLAGS) -c $<

.ml.cmx:
		$(OCAMLOPT) $(INCLUDES) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
		rm -f *.cm[tdiox] *.cmti *.cmdi *.o *.annot lexer.ml parser.ml parser.mli testunif

include .depend
#asdf
