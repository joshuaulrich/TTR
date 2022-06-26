#https://stackoverflow.com/questions/34603415/makefile-automatic-target-generation
#https://www.gnu.org/software/make/manual/make.html#Static-Pattern
#https://stackoverflow.com/questions/2826029/passing-additional-variables-from-command-line-to-make
#https://stackoverflow.com/questions/2214575/passing-arguments-to-make-run

R_HOME = /usr

PKG_PATH = ${PWD}
TOP = ${PWD}/..
PKG_DESC = ${PKG_PATH}/DESCRIPTION
PKG_NAME = $(shell sed -ne "s/^Package: //p" ${PKG_DESC} | tr -d '\n')
PKG_VER = $(shell sed -ne "s/^Version: \(.*\)/\1/p" ${PKG_DESC} | tr -d '\n')
PKG_TARGZ = $(PKG_NAME)_$(PKG_VER).tar.gz


PKG_BUILD_OPTS ?= --no-build-vignettes
R_LIB ?= $(shell Rscript -e 'cat(.libPaths()[1L])')

PKG_INST_FILE = $(R_LIB)/${PKG_NAME}/DESCRIPTION

PKG_R_FILES := $(wildcard ${PKG_PATH}/R/*.R)
PKG_SRC_FILES := $(wildcard ${PKG_PATH}/src/*)
PKG_ALL_FILES := ${PKG_PATH}/.Rbuildignore ${PKG_PATH}/DESCRIPTION \
  ${PKG_PATH}/NAMESPACE $(PKG_R_FILES) $(PKG_SRC_FILES)

UNIT_TEST_SUITE = ${PKG_PATH}/tests/doRUnit.R
UNIT_TEST_FILES = $(wildcard ${PKG_PATH}/inst/unitTests/runit*.R)

BENCHMARK_FILE = ${PKG_PATH}/inst/benchmarks/benchmark.subset.R

.PHONY: build install check tests test

all: check #benchmark

#man/*.Rd depend on R/*.R files
print:
	@echo 'path: $(PKG_PATH) \
	inst_file: $(PKG_INST_FILE) \
	tar.gz: $(PKG_TARGZ)'

# Build package
build: $(PKG_TARGZ)
$(PKG_TARGZ): $(PKG_ALL_FILES) $(UNIT_TEST_FILES) $(UNIT_TEST_SUITE)
	@${R_HOME}/bin/R CMD build ${PKG_BUILD_OPTS} ${PKG_PATH} --no-build-vignettes

# Install package
install: build $(PKG_INST_FILE)
$(PKG_INST_FILE): $(PKG_TARGZ)
	@${R_HOME}/bin/R CMD INSTALL ${PKG_TARGZ} --no-byte-compile

# Run R CMD check
check: docs install
	@_R_CHECK_CRAN_INCOMING_=false \
	${R_HOME}/bin/R CMD check ${PKG_TARGZ} --as-cran

docs: ${PKG_R_FILES}
	@${R_HOME}/bin/Rscript -e "roxygen2::roxygenize(roclets='rd')"

# Build for CRAN
#build-cran: $(PKG_TARGZ)
#$(PKG_TARGZ): $(PKG_ALL_FILES) $(UNIT_TEST_FILES) $(UNIT_TEST_SUITE)
#	@${R_HOME}/bin/R CMD build ${PKG_BUILD_OPTS} ${PKG_PATH}
#
## Install package for CRAN
#install-cran: build-cran $(PKG_INST_FILE)
#$(PKG_INST_FILE): $(PKG_TARGZ)
#	@${R_HOME}/bin/R CMD INSTALL ${PKG_TARGZ}
#
## Run R CMD check for CRAN
#check-cran: install-cran
#	${R_HOME}/bin/R CMD check ${PKG_TARGZ}

# Run unit test suite
tests: install ${UNIT_TEST_FILES}
	@${R_HOME}/bin/Rscript ${UNIT_TEST_SUITE}

# Run one test file
TEST_FILE = $(wildcard ${PKG_PATH}/inst/unitTests/*${file}*)
TEST_CMD = 'suppressMessages({require(${PKG_NAME}); require(RUnit)}); \
            out <- runTestFile("${TEST_FILE}", verbose = TRUE); \
            printTextProtocol(out)'

test: install
ifndef file
	$(error "file not defined")
endif
	@${R_HOME}/bin/Rscript -e ${TEST_CMD}

# Run individual unit test file
#%::
#	@THING=$(wildcard unitTests/runit*$@*);\
#	echo $(foreach f, $(THING), $(echo $(f)))
#	@echo $(value ut)

#% :: unitTests/runit%.R
#	@echo $<

#runit%.R: runit%.R
#	@echo "yay!"#${R_HOME}/bin/Rscript -e 'require(RUnit); runTestFile("unitTests/$@")'"

#	@echo "$(filter $(wildcard unitTests/runit*$@*), $(UNIT_TEST_FILES))"

#$(filter $(wildcard unitTests/runit*$@*), $(UNIT_TEST_FILES))"

#.PHONY: runit
#runit: ;
#	@echo "$(filter $(wildcard unitTests/runit*$@*), $(UNIT_TEST_FILES))"

#$(filter unitTests/runit.%.R, $(UNIT_TEST_FILES)): runit.%.R
#	echo "yay!"
#	@${R_HOME}/bin/Rscript -e 'require(RUnit); runTestFile("unitTests/$@")'

# Run benchmarks
#benchmark:
#	@${R_HOME}/bin/Rscript ${BENCHMARK_FILE}
