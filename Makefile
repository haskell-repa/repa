
.PHONY : all
all : 
	@make clean
	@make repa
	@make repa-bytestring
	@make repa-io
	@make repa-algorithms
	@make repa-examples
	

.PHONY : clean
clean :
	@echo "-- Cleaning up -------------------------------------------------"
	rm -Rf repa/dist
	rm -Rf repa-bytestring/dist
	rm -Rf repa-io/dist
	rm -Rf repa-algorithms/dist
	rm -Rf repa-examples/dist
	@echo

.PHONY : bench
bench :
	@echo "-- Running benchmarks ------------------------------------------"
	@repa-examples/dist/build/repa-benchmark/repa-benchmark



# == Packages =====================================================================================
.PHONY : repa
repa :
	@echo "-- Building repa -----------------------------------------------"
	@cd $@ ; \
		runghc Setup.hs clean ; \
		runghc Setup.hs configure --user ; \
		runghc Setup.hs build ; \
		runghc Setup.hs install
	@echo


.PHONY : repa-bytestring
repa-bytestring :
	@echo "-- Building repa-bytestring ------------------------------------"
	@cd $@ ; \
		runghc Setup.hs clean ; \
		runghc Setup.hs configure --user ; \
		runghc Setup.hs build ; \
		runghc Setup.hs install
	@echo


.PHONY : repa-io
repa-io :
	@echo "-- Building repa-io --------------------------------------------"
	@cd $@ ; \
		runghc Setup.hs clean ; \
		runghc Setup.hs configure --user ; \
		runghc Setup.hs build ; \
		runghc Setup.hs install
	@echo


.PHONY : repa-algorithms
repa-algorithms :
	@echo "-- Building repa-algorithms ------------------------------------"
	@cd $@ ; \
	runghc Setup.hs clean ; \
		runghc Setup.hs configure --user ; \
		runghc Setup.hs build ; \
		runghc Setup.hs install
	@echo


.PHONY : repa-examples
repa-examples :
	@echo "-- Building repa-examples --------------------------------------"
	@cd $@ ; \
		runghc Setup.hs clean ; \
		runghc Setup.hs configure --user ; \
		runghc Setup.hs build ; \
		runghc Setup.hs install
	@echo 


