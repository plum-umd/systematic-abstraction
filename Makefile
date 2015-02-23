GHC=ghc

BINS=reduction cek cek-map cesk ceskp ceskpt ceskpt2 aceskpt ceskptl aceskptl cesks ceskst cesksl acesksl anf-concrete 0aceskp

all: $(BINS)

anf-concrete: anf-concrete.hs ANFSyntax.hs
	ghc --make anf-concrete.hs

acesksl: acesksl.hs
	ghc -XTypeOperators --make acesksl.hs

cesksl: cesksl.hs
	ghc -XTypeOperators --make cesksl.hs

ceskst: ceskst.hs
	ghc -XTypeOperators --make ceskst.hs


0aceskp: 0aceskp.hs
	ghc -XTypeOperators -XTypeSynonymInstances --make 0aceskp.hs

aceskptl: aceskptl.hs
	ghc -XTypeOperators -XTypeSynonymInstances --make aceskptl.hs

ceskptl: ceskptl.hs
	ghc -XTypeOperators --make ceskptl.hs

ceskpt2: ceskpt2.hs
	ghc -XTypeOperators --make ceskpt2.hs

aceskpt: aceskpt.hs
	ghc -XTypeOperators -XTypeSynonymInstances --make aceskpt.hs

ceskpt: ceskpt.hs
	ghc -XTypeOperators --make ceskpt.hs

ceskp: ceskp.hs
	ghc -XTypeOperators --make ceskp.hs

cesks: cesks.hs
	ghc -XTypeOperators --make cesks.hs

cesk: cesk.hs
	ghc -XTypeOperators --make cesk.hs

cek-map: cek-map.hs
	ghc -XTypeOperators --make cek-map.hs

cek: cek.hs
	ghc --make cek.hs

reduction: reduction.hs
	ghc -XViewPatterns --make reduction.hs
clean:
	cleanup
	rm -vf *.{hi,o} $(BINS)


