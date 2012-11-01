
# Run all the quickcheck tests.
.PHONY : test
test   : 
	ghc -fforce-recomp test/props/Test.hs -e main -XMagicHash -XBangPatterns
		

# Build the fusion eyeball tests.
#   Look at test/fusion/Test.prep to see if it worked.
.PHONY : fusion
fusion :
	touch test/fusion/Test.hs 
	ghc --make test/fusion/Test.hs \
		-XScopedTypeVariables -XMagicHash -XBangPatterns  \
		-O2 -fno-liberate-case -funfolding-use-threshold1000 \
		-fllvm -optlo-O3 \
		-ddump-prep -dsuppress-all -dppr-case-as-let -dppr-cols200 \
		-keep-s-files \
		> test/fusion/Test.prep
