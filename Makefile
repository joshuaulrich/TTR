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
PKG_RD_FILES := $(wildcard ${PKG_PATH}/man/*.Rd)
PKG_SRC_FILES := $(wildcard ${PKG_PATH}/src/*)
PKG_ALL_FILES := ${PKG_PATH}/DESCRIPTION ${PKG_PATH}/NAMESPACE \
  $(PKG_R_FILES) $(PKG_RD_FILES) $(PKG_SRC_FILES) ${PKG_PATH}/.Rbuildignore

HTML_FILES := $(patsubst %.Rmd, %.html, $(wildcard *.Rmd)) \
              $(patsubst %.md, %.html, $(wildcard *.md))

UNIT_TEST_SUITE = ${PKG_PATH}/tests/doRUnit.R
UNIT_TEST_FILES = $(wildcard ${PKG_PATH}/inst/unitTests/runit*.R)

BENCHMARK_FILE = ${PKG_PATH}/inst/benchmarks/benchmark.subset.R

.PHONY: docs build install check tests test clean

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
check: docs build
	@_R_CHECK_CRAN_INCOMING_=false \
	${R_HOME}/bin/R CMD check ${PKG_TARGZ} --as-cran

docs: ${PKG_R_FILES}
	@${R_HOME}/bin/Rscript -e "roxygen2::roxygenize(roclets='rd')" \
	  && sed -i '/^RoxygenNote/d' ${PKG_PATH}/DESCRIPTION \
	  && /bin/rm ${PKG_PATH}/src/*.o \
	  && /bin/rm ${PKG_PATH}/src/*.so

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

clean:
	$(RM) $(HTML_FILES) ${PKG_PATH}/src/*.o ${PKG_PATH}/src/*.so
