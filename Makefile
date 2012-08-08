bins := xtee map chop trans
dists := $(foreach bin,$(bins),dist/build/$(bin)/$(bin))

all : $(bins)

install : $(dists)
	cabal install

$(bins) : $(dists)
	cp $^ .

dist/setup-config : jcoreutils.cabal
	cabal configure

$(dists) : dist/setup-config xtee.hs map.hs chop.hs trans.hs
	cabal build
	@touch $@ # cabal doesn't always update the build (if it doesn't need to)

clean :
	cabal clean
