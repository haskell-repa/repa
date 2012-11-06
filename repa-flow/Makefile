
# Run all the quickcheck tests.
.PHONY : test-seq
test-seq : 
	@echo "-- Sequential ----------------------------------------------------"
	ghc -fforce-recomp -fobject-code test/props/TestSeq.hs -e main \
 	    -XScopedTypeVariables -XMagicHash -XBangPatterns -XUnboxedTuples \
 	    -XExistentialQuantification
	@echo


.PHONY : test-par
test-par : 
	@echo "-- Parallel ------------------------------------------------------"
	ghc -fforce-recomp -fobject-code test/props/TestPar.hs -e main \
	    -XScopedTypeVariables -XMagicHash -XBangPatterns -XUnboxedTuples \
	    -XExistentialQuantification -XTypeFamilies
	@echo

.PHONY : test
test : test-seq test-par


# Build the fusion eyeball tests.
#   Look at test/fusion/Test.prep to see if it worked.
.PHONY : fusion-seq
fusion-seq :
	touch test/fusion/TestSeq.hs 
	ghc --make test/fusion/TestSeq.hs \
		-XScopedTypeVariables -XMagicHash -XBangPatterns -XUnboxedTuples \
		-XExistentialQuantification \
		-O2 \
		-fno-liberate-case -funfolding-use-threshold1000 \
		-fsimpl-tick-factor=200 \
		-fllvm -optlo-O3 \
		-ddump-prep -dsuppress-all -dppr-case-as-let -dppr-cols200 \
		-keep-s-files \
		> test/fusion/TestSeq.prep

.PHONY : fusion-par
fusion-par :
	touch test/fusion/TestPar.hs 
	ghc --make test/fusion/TestPar.hs \
		-XScopedTypeVariables -XMagicHash -XBangPatterns -XUnboxedTuples \
		-XExistentialQuantification -XTypeFamilies \
		-O2 \
		-fno-liberate-case -funfolding-use-threshold1000 \
		-fsimpl-tick-factor=200 \
		-fllvm -optlo-O3 \
		-ddump-prep -dsuppress-all -dppr-case-as-let -dppr-cols200 \
		-keep-s-files \
		> test/fusion/TestPar.prep
