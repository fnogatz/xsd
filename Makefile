SWIPL := swipl
CLI := ./cli.exe

all: clean install test

install: cli

cli:
	@$(SWIPL) -g main -o $(CLI) -c cli.pl && chmod +x $(CLI)

test: cli test.cli test.validate

test.validate:
	@$(SWIPL) -q -g main -t halt -s test/test.pl

test.cli:
	@$(CLI) ./test/schema/simpleType_int.xsd ./test/cli/simpleType_int.xml

clean: clean.cli

clean.cli:
	rm -f $(CLI)
