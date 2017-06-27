# XML Validator in SWI-Prolog

Validate an XML Document against an XML Schema in SWI-Prolog. 

## Installation

All you need is [SWI-Prolog](http://www.swi-prolog.org/). See there for installation instructions.

### Pre-Compilation

It is possible to create a pre-compiled file which increases the tool's performance significantly. The command line interface is compiled using swipl's [`-c` option](http://www.swi-prolog.org/pldoc/doc_for?object=section%282,%272.10%27,swi%28%27/doc/Manual/compilation.html%27%29%29):

```shell
swipl -g main -o cli.exe -c cli.pl
```

The `.exe` suffix is chosen for compatibility with Windows systems.

## Usage as CLI

xml-validate provides a command line interface. You can directly execute it via

```shell
swipl -g main cli.pl -- schema.xsd instance.xml 
```

Call with `--help` instead of the filenames to get more options.

After the pre-compilation step mentioned before, the created executable can be called via:

./cli.exe schema.xsd instance.xml

## Usage with SWI-Prolog

The module `xml_validate` exports the following predicates:

*   `flatten_xml(+Input, ?Identifier)`

    Loads an XML File from source `Input` into the prolog database. 
    An `Identifier` can be freely chosen, otherwise it will be generated.

*   `validate(+Document, +Schema)`

    Validates an XML Document with Identifier `Document` against an XML Schema `Schema`. 

*   `remove_file(+Identifier)`

    Deletes all nodes corresponding to `Identifier` from the prolog database. 

### Example call

```prolog
?- flatten_xml('path/to/document.xml', xml),
   flatten_xml('path/to/schema.xsd', xsd),
   validate(xml, xsd).
```

## XML File representation

Once an XML File is loaded using `flatten_xml/2`, it will be represented by the following predicates:

*   `node(File_ID, Namespace, Node, ID)`

    Each node in the document `File_ID` is represented by a node/4 fact with the unique identifier `ID`. Also, the namespace and the name of the element are stated. 

*   `node_attribute(File_ID, ID, Attribute, Value)`

    For every attribute a `node_attribute/4` fact is generated. It stores the `Attribute` and `Value` of an attribute associated to node `ID` inside document `File_ID`. 

*   `text_node(File_ID, ID, Node)`

    Text inside the document `File_ID` is represented as `text_node/3` facts. These have unique identifiers like regular document nodes and store the text `Node`.

## Supported Features

Please see `FEATURES.md` for full list of currently supported components of the XML Schema Definition Language. 

## Environment

Developed and tested in SWI-Prolog, version 7.4.1, 64 bit.
