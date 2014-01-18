# files for solution rising method
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I /usr/lib/ocaml -I ./..
OCAMLFLAGS=-g -annot              
OCAMLOPTFLAGS=              
TARGETS=konevTree.cmo konevProve.cmo

all: $(TARGETS) 
		
depend:
		$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .depend

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

#.mly.cmo:
#		$(OCAMLC)   $(INCLUDES) -c $<
#
.ml.cmo:
		$(OCAMLC)   $(INCLUDES) $(OCAMLFLAGS) -c $<

.mli.cmi:
		$(OCAMLC)   $(INCLUDES) $(OCAMLFLAGS) -c $<

.ml.cmx:
		$(OCAMLOPT) $(INCLUDES) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
		rm -f *.cmi *.cmo *.cmx *.o 
	
include .depend
