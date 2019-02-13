.PHONY: all test clean

version := $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile = xsd-$(version).tgz

SWIPL := swipl
CLI := ./cli.exe

all: install

check: test.validate

install: cli

cli:
	@$(SWIPL) -g main -o $(CLI) -c cli.pl && chmod +x $(CLI)

test: cli test.cli test.validate

test.validate:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

test.cli:
	@$(CLI) ./test/schema/simpleType_int.xsd ./test/cli/simpleType_int.xml

# Run `make test.xsd` to check whether the files
#   in test/schema are valid XSD 1.1 files.
# You have to provide the location of Xerces2-J
#   as JAXP_PATH classpath for Java. 
test.xsd:
	find test/schema -type f -exec java -cp $(JAXP_PATH) jaxp.SourceValidator -xsd11 -a {} \;

clean: clean.cli

clean.cli:
	rm -f $(CLI)

package: test
	tar cvzf $(packfile) prolog test pack.pl README.md LICENSE cli.pl FEATURES.md

release: test
	hub release create -m v$(version) v$(version)
