# $Id$

VERSION = 0.0.0

FLAGS = -g
PACKS = biniou,yojson,unix

.PHONY: default all opt install
default: META all opt
all: jsondiff.byte
opt: jsondiff

ifndef PREFIX
  PREFIX = $(shell dirname $$(dirname $$(which ocamlfind)))
  export PREFIX
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
  export BINDIR
endif

META: META.in Makefile
	sed -e 's:@@VERSION@@:$(VERSION):' META.in > META

install: META
	test ! -f jsondiff || cp ydump $(BINDIR)/
	test ! -f jsondiff.exe || cp ydump.exe $(BINDIR)/
	ocamlfind install jsondiff META \
          $$(ls jsondiff.mli jsondiff.cmi jsondiff.cmo jsondiff.cmx jsondiff.o)

uninstall:
	test ! -f $(BINDIR)/jsondiff || rm $(BINDIR)/jsondiff
	test ! -f $(BINDIR)/jsondiff.exe || rm $(BINDIR)/jsondiff.exe 
	ocamlfind remove jsondiff

jsondiff.cmo: jsondiff.cmi
jsondiff.cmi: jsondiff.ml
	ocamlfind ocamlc -c $(FLAGS) -package $(PACKS) jsondiff.ml

jsondiff.cmx: jsondiff.cmi jsondiff.ml
	ocamlfind ocamlopt -c $(FLAGS) -package $(PACKS) jsondiff.ml

jsondiff.byte: jsondiff.cmo
	ocamlfind ocamlc -o jsondiff.byte -package $(PACKS) -linkpkg \
	       	jsondiff.cmo 

jsondiff: jsondiff.cmx
	ocamlfind ocamlopt -o jsondiff -package $(PACKS) -linkpkg \
		jsondiff.cmx 

.PHONY: clean

clean:
	rm -f *.o *.a *.cm* *~ *.annot jsondiff jsondiff.exe \
		META jsondiff.byte
