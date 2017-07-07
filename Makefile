version := $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile = xsd-$(version).tgz

SWIPL := swipl
CLI := ./cli.exe

all: install

install: cli

cli:
	@$(SWIPL) -g main -o $(CLI) -c cli.pl && chmod +x $(CLI)

test: cli test.cli test.validate

test.validate:
	@$(SWIPL) -q -g 'main,halt(0)' -t 'halt(1)' -s test/test.pl

test.cli:
	@$(CLI) ./test/schema/simpleType_int.xsd ./test/cli/simpleType_int.xml

clean: clean.cli

clean.cli:
	rm -f $(CLI)

package: test
	tar cvzf $(packfile) prolog test pack.pl README.md LICENSE cli.pl FEATURES.md

release: test
	hub release create -m v$(version) v$(version)
