test:
	ghc ./frp.hs -e 'runTests'

run: 
	runhaskell ./frp.hs

install:
	cabal install ChasingBottoms
