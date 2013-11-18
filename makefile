test:
	ghc ./Frp.hs -e 'runTests'

run: 
	ghc --make ./Main.hs && ./Main

install:
	cabal install ChasingBottoms
