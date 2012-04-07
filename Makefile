bins := xtee map chop
dists := $(foreach bin,$(bins),dist/build/$(bin)/$(bin))

all : $(bins)

doc : xtee.pdf

install : $(dists)
	cabal install

$(bins) : $(dists)
	cp $^ .

dist/setup-config : jcoreutils.cabal
	cabal configure

$(dists) : dist/setup-config xtee.lhs map.hs chop.hs
	cabal build
	@touch $@ # cabal doesn't always update the build (if it doesn't need to)

xtee.pdf : xtee.tex
	pdflatex $<

xtee.tex : xtee.lhs
	lhs2TeX $< > $@

clean :
	cabal clean
	rm -f xtee.{aux,log,out,ptb,tex}
