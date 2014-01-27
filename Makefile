
# Build the standard library.
.PHONY : all
all : 
	@make clean
	@make repa
	@make repa-io
	@make repa-algorithms
	@make repa-examples


# Build the repa plugin.
.PHONY : plugin
plugin : 
	@make clean
	@make repa
	@make repa-io
	@make repa-series
	@make repa-plugin
	

# == Clean ====================================================================
.PHONY : clean
clean :
	@echo "-- Cleaning up -------------------------------------------------"
	rm -Rf repa/dist
	rm -Rf repa-algorithms/dist
	rm -Rf repa-bulk/dist
	rm -Rf repa-examples/dist
	rm -Rf repa-io/dist
	rm -Rf repa-plugin/dist
	rm -Rf repa-series/dist
	@echo


# == Packages =================================================================
.PHONY : repa
repa :
	@echo "-- Building repa -----------------------------------------------"
	@cd $@ ; \
		cabal clean ; \
		cabal install --enable-shared --user --reinstall --force
	@echo


.PHONY : repa-io
repa-io :
	@echo "-- Building repa-io --------------------------------------------"
	@cd $@ ; \
		cabal clean ; \
		cabal install --enable-shared --user --reinstall --force
	@echo


.PHONY : repa-algorithms
repa-algorithms :
	@echo "-- Building repa-algorithms ------------------------------------"
	@cd $@ ; \
		cabal clean ; \
		cabal install --enable-shared --user --reinstall --force
	@echo


.PHONY : repa-examples
repa-examples :
	@echo "-- Building repa-examples --------------------------------------"
	@cd $@ ; \
		cabal clean ; \
		cabal install --enable-shared --user --reinstall
	@echo 


.PHONY : repa-series
repa-series :
	@echo "-- Building repa-series ----------------------------------------"
	@cd $@ ; \
		cabal clean ; \
		cabal install --enable-shared --user --reinstall --force
	@echo 


.PHONY : repa-plugin
repa-plugin :
	@echo "-- Building repa-plugin ----------------------------------------"
	@cd $@ ; \
		cabal clean ; \
		cabal install --enable-shared --user --reinstall --force
	@echo 


