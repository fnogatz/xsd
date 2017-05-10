# TAP Input/Output Test Suite

Definition of input/output tests for the XML Validator following the [Test Anything Protocol](http://testanything.org/) (TAP).

## Run Tests

The defined tests can be run using the following command:

```shell
swipl -q -t main -s test.pl
```

This produces a TAP compatible output like the following:

```
TAP version 13
1..4
ok 1 - [element_string] Simple xs:string
ok 2 - [element_string] Empty xs:string
ok 3 - [element_integer] Simple xs:integer
ok 4 - [element_integer] Not a number
```

The identifier given in the square brackets references the tested XML Schema (located in the `schema` directory), the following name identifies the called test.

## Define Tests

Each file in the `schema` directory will be used as a XSD instance. The test runner `test.pl` will look for a corresponding Prolog file in the `validation` directory. There, new tests can be specified in the following form using the `{|xml|| ... |}` quasi quotation:

```prolog
Testname:
{|xml||
  <!-- Some XSD instance, i.e. XML document -->
|}.
```

Please note that the `Testname` must be given as atom and must be unique within a certain validation file.

To define failing tests, i.e. to define XML documents which should be recognized as no valid instances of the given XML Schema, simply extend the `Testname` by `(fail)`, e.g. `'Not a valid integer'(fail)`.
