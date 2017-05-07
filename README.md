# XML Validator in SWI-Prolog

Validate a XML Document against XML Schema in SWI-Prolog. 

## Usage

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