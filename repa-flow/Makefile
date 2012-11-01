
# Run all the quickcheck tests.
.PHONY : test
test   : 
	ghc test/props/Test.hs -e main -XMagicHash -XBangPatterns
		

# Build the fusion eyeball tests.
#   Look at test/fusion/Test.prep to see if it worked.
.PHONY : fusion
fusion :
	ghc -O2 -c test/fusion/Test.hs \
		-funfolding-use-threshold1000 \
		-ddump-prep -dsuppress-all -dppr-case-as-let -dppr-cols200 \
		> test/fusion/Test.prep
