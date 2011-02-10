PROGRAM = xtee
DIST := $(PROGRAM)-$(shell ./$(PROGRAM) --version | cut -d ' ' -f 3)

all : $(PROGRAM)

$(PROGRAM) : $(PROGRAM).lhs
	runhaskell Setup.lhs configure
	runhaskell Setup.lhs build

doc : $(PROGRAM).pdf

$(PROGRAM).pdf : $(PROGRAM).tex
	pdflatex $<

$(PROGRAM).tex : $(PROGRAM).lhs
	lhs2TeX $(PROGRAM).lhs > $@

clean :
	cabal clean
	rm -f $(PROGRAM).{aux,log,out,ptb,tex}

dist :
	darcs dist

sloc : $(PROGRAM).lhs
	lhs2TeX --code $< | grep --invert-match '^ *$$' | wc --lines

# dist:
# #	darcs dist would be nice, but we want to add xtee.pdf
# 	mkdir $(DIST)
# 	for f in `darcs query manifest`; do cp $$f $(DIST)/; done
# #       append the documentation
# 	cp xtee.pdf $(DIST)/
# 	tar czvf $(DIST).tar.gz $(DIST)/
# 	rm -rf $(DIST)/
