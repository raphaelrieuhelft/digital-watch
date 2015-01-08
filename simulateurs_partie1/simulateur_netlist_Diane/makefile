OCAMLBUILD=ocamlbuild -I src -I src_unmodified
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