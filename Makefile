
PACKAGES = \
	repa \
	repa-eval \
	repa-io \
	repa-algorithms \
	repa-scalar \
	repa-stream \
	repa-convert \
	repa-array \
	repa-flow \
	repa-store \
	repa-query


# Build the standard library.
.PHONY : all
all : 
	@make packages
	@make tools
	@make examples


# Build and install all the Cabal packages.
.PHONY : packages
packages :
	@for package in ${PACKAGES}; do \
		if [ `ghc-pkg list $${package} --simple-output | wc -l` -gt 0 ]; then \
			echo "* Skipping $${package}; already installed"; \
			continue; \
		fi; \
		cd $${package}; \
		echo "* Building $${package}"; \
			cabal install --enable-documentation --force-reinstalls;\
		if [ $$? -ne 0 ]; then \
			echo "* Error in $${package}!"; \
			break; \
		fi; \
		cd ..; \
		echo; \
	done;


# Build the tools.
.PHONY : tools
tools :
	@cd repa-tools; cabal install


# Build the examples
.PHONY : examples
examples :
	@cd repa-examples; cabal install


# Unregister all the Cabal packages.
.PHONY : unregister
unregister : 
	@echo "* Unregistering packages"
	@for package in ${PACKAGES}; do \
		if [ `ghc-pkg list $${package} --simple-output | wc -l` -gt 0 ]; then \
			echo "* Unregistering $${package}"; \
			ghc-pkg unregister $${package} --force 2> /dev/null; \
			if [ $$? -ne 0 ]; then  \
				continue; \
			fi; \
		fi; \
	done;


# Unregister all the packages, then reinstall them.
.PHONY : reinstall
reinstall :
	@make unregister
	@make packages


# Clean out all the junk.
.PHONY : clean
clean :
	@for package in ${PACKAGES} repa-examples repa-tools; do \
		rm -Rf $${package}/dist; \
	done;
	@find . -name '.DS_Store' -or -name '._.DS_Store' | xargs rm -f

