.PHONY: all test clean

SWIPL ?= swipl

version := $(shell $(SWIPL) -q -s pack -g 'version(V),writeln(V)' -t 'halt(1)')
pack_dir := $(shell $(SWIPL) -q -g "absolute_file_name(pack('.'),D),writeln(D)" -t 'halt(1)')
packfile = xsd-$(version).tgz

CLI := ./cli.exe

all: install

version:
	@echo $(version)

version.swi:
	@$(shell swipl --version)

check: test.validate

link:
	echo $(SWIPL)
	echo $(pack_dir)
	$(SWIPL) -g "absolute_file_name(pack('.'),D),writeln(D)" -t 'halt(1)'
	$(SWIPL) --version
	ln -s $(shell pwd) $(pack_dir)/xsd

install: install.packs cli

install.packs: install.packs.regex install.packs.tap

install.packs.regex:
	@$(SWIPL) -g 'pack_install(regex,[interactive(false)]),halt(0)' -t 'halt(1)'

install.packs.tap:
	@$(SWIPL) -g 'pack_install(tap,[interactive(false)]),halt(0)' -t 'halt(1)'

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
