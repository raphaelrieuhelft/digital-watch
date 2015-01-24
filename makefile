OCAMLBUILD=ocamlbuild -classic-display \
	-tags annot,debug,thread \
	-libs unix \
	-I compilateur_interpreteur \
	-I simulateur_netlist \
		-I simulateur_netlist/netlist_analyser \
		-I simulateur_netlist/scheduling \
	-use-menhir
TARGET=byte


main:
	$(OCAMLBUILD) main.$(TARGET)

clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~ **/*~

cleanall: realclean

cleansch: 
	rm -f **/*_sch.net