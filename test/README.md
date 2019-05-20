# TAP Input/Output Test Suite

Definition of input/output tests for the XML Validator following the [Test Anything Protocol](http://testanything.org/) (TAP).

## Run Tests

The defined tests can be run using the following command:

```shell
swipl -q -g main -t halt -s test.pl
```

This produces a TAP compatible output like the following:

```
TAP version 13
1..4
ok 1 - [schema:element_string] Simple xs:string
ok 2 - [schema:element_string] Empty xs:string
ok 3 - [schema:element_integer] Simple xs:integer
ok 4 - [xpath:constructor] boolean(true)
```

The test output lines have the following format: `(ok|not okay) <test number> - [<type>:<file name>] <test name> (fail)?`.

* `(ok|not okay)` in association with `(fail)?` indicates the success of the test case.
* `<test number>` is an ascending number identifying the test case.
* `<type>` is either `schema` or `xpath`.
    * If it is `schema`, the test validates an XML instance against a specific XML schema. The schema is located inside the `schema` directory as `<file name>.xsd` and the corresponding instances are embedded in `<file name>.pl` inside the `validation` directory.
    * If it is `xpath`, the test is a simple `<INPUT> ==> <OUTPUT>` test for the `xpath_expr(<INPUT>, <OUTPUT>)` predicate. The test itself is defined in the file `<file name>.pl` inside the `xpath` directory.
* `<test name>` is a verbal description of the test case.

## Define Tests

### Schema validation tests
The goal of this test type is to assert the correctness of the `xsd_validate(SCHEMA, INSTANCE)` predicate inside `xsd.pl` and therefore to validate a given XML instance against an XML schema document.

The test runner looks for pairs of `*.xsd` files in the `schema` directory and corresponding `*.pl` files inside the `validation` directory with the same file name. After that, the pairs are validated and the test results are printed out.

The XML schema is provided as a regular `*.xsd` file.

However, the XML instance is embedded inside prolog code by using the `{|xml|| ... |}` quasi quotation:
```prolog
`<test name>`(fail)?:
{|xml||
  <!-- some XML instance, i.e. XML document -->
|}.
```

Please note that the `<test name>` must be given as an atom and be unique within a certain validation file.

To define failing tests, i.e. to define XML documents which should be recognized as no valid instances of the given XML Schema, simply extend the `<test name>` by `(fail)`, e.g. `'Not a valid integer'(fail)`.

### XPath Unit Tests
The goal of this test type is to assert the correctness of the `xpath_expr(IN, OUT)` predicate defined inside `xpath.pl` and therefore to validate a given XPath expression.

The test runner looks for test definitions of the form `<IN> ==> <OUT>` in an arbitrary `*.pl` file inside the `xpath` directory. After that, `xpath_expr(<IN>, <OUT>)` is called and the result is printed out.

If `<OUT>` is `false`, it is a negative test, which asserts, that `<IN>` is not a valid XPath expression:
e.g. `decimal('test') ==> false.`.

Otherwise, `<IN>` has to be evaluated to `<OUT>` inside `xpath_expr`: e.g. `decimal('+1.234') ==> data('decimal', [1.234]).`.

## Meta-validate Tested XSD Files

As the used XSD files in the `test/schema` subfolder are created manually, it might be useful to check if they really satisfy the XML Schema 1.1 Meta-Schema, i.e., that they are valid XSD 1.1 documents. This test can be run via `make test.xsd`, which expects the [Apache Xerces2-J](http://xerces.apache.org/xerces2-j/samples-jaxp.html) samples directory set as `JAXP_PATH` environment varaible.

Windows users, you can meta-validate the XSD files using the following command:
```
for /r test/schema %f in (*.xsd) do java jaxp.SourceValidator -xsd11 -a %f
```
The (system) environment variable `PATH` has to contain an entry to the binary folder of the JDK, `JAVA_HOME` has to point to the JDK directory and `CLASSPATH` has to contain entries pointing to all `*.jar` files inside the extracted Xerces2-J archieve in order to function properly.
