BIN=xtee
DIST=dist/build/$(BIN)/$(BIN)

all : $(BIN)

doc : $(BIN).pdf

install : $(DIST)
	cabal install

$(BIN) : $(DIST)
	cp $< $@

dist/setup-config : $(BIN).cabal
	cabal configure

$(DIST) : dist/setup-config $(BIN).lhs
	cabal build
	@touch $@ # cabal doesn't always update the build (if it doesn't need to)

$(BIN).pdf : $(BIN).tex
	pdflatex $<

$(BIN).tex : $(BIN).lhs
	lhs2TeX $< > $@

clean :
	cabal clean
	rm -f $(BIN).{aux,log,out,ptb,tex}
