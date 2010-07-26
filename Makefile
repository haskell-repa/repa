

all :
	cd repa 		; pwd ; runghc Setup.hs clean ; runghc Setup.hs configure --user ; runghc Setup.hs build ; runghc Setup.hs install
	cd repa-bytestring 	; pwd ; runghc Setup.hs clean ; runghc Setup.hs configure --user ; runghc Setup.hs build ; runghc Setup.hs install
	cd repa-io 		; pwd ; runghc Setup.hs clean ; runghc Setup.hs configure --user ; runghc Setup.hs build ; runghc Setup.hs install
	cd repa-algorithms	; pwd ; runghc Setup.hs clean ; runghc Setup.hs configure --user ; runghc Setup.hs build ; runghc Setup.hs install
	cd repa-examples	; pwd ; runghc Setup.hs clean ; runghc Setup.hs configure --user ; runghc Setup.hs build ; runghc Setup.hs install
	