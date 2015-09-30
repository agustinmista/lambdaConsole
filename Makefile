all:
	ghc -o build/Main -outputdir build/ src/*.hs
	cp src/Prelude.lam build/
