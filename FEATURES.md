# Supported features 

## Schema Declaration

```
< schema >
    Content: ( annotation ?, ( complexType | simpleType | element )*)
</ schema >
```

## Element Declaration

```
< element
maxOccurs = ( nonNegativeInteger | unbounded ) : 1
minOccurs = nonNegativeInteger : 1
name = NCName
ref = QName
type = QName >
    Content: ( annotation ?, ( complexType | simpleType )?)
</ element >
```

## Complex Type Definition

```
< complexType
name = NCName >
    Content: ( annotation ?, ( all | choice | sequence )?, attribute *)
</ complexType >
```

Complex type inheritance is not supported yet.

## Model Group Definition

```
< sequence
maxOccurs = ( nonNegativeInteger | unbounded ) : 1
minOccurs = nonNegativeInteger : 1>
    Content: ( annotation ?, ( element | choice | sequence )*)
</ sequence >

< all
maxOccurs = 1 : 1
minOccurs = (0 |1) : 1>
    Content: ( annotation ?, element *)
</ all >

< choice
maxOccurs = ( nonNegativeInteger | unbounded ) : 1
minOccurs = nonNegativeInteger : 1>
    Content: ( annotation ?, ( element | choice | sequence )*)
</ choice >
```

## Attribute Declaration

```
< attribute
fixed = string
name = NCName
ref = QName
type = QName
use = ( optional | prohibited | required ) : optional >
    Content: ( annotation ?, simpleType ?)
</ attribute >
```

## Simple Type Definitions

```
< simpleType
name = NCName >
    Content: ( annotation ?, ( restiction | list | union ))
</ simpleType >

< restriction
base = QName >
    Content: ( annotation ?, ( minExclusive | minInclusive | maxExclusive | maxInclusive | enumeration | pattern | length | minLength | maxLength )*)
</ restriction >

< list
itemType = QName >
    Content: ( annotation ?, ( simpleType ?))
</ list >

< union
memberTypes = List of QName >
    Content: ( annotation ?, ( simpleType *))
</ union >
```

## Constraining Facets

Almost all facets of the XML 1.1 specification are supported.

However, 'Assert's and 'Assertion's are currently being worked on.
The 'whiteSpace'-facet cannot be used due to the parser eliminating all redundant whitespaces automatically.
The 'explicitTimezone'-facet is not implemented yet.

All other facets are supported:

```
< enumeration
value = anySimpleType >
    Content: ( annotation ?)
</ enumeration >

< fractionDigits
value = anySimpleType >
    Content: ( annotation ?)
</ fractionDigits >

< length
value = anySimpleType >
    Content: ( annotation ?)
</ length >

< maxExclusive
value = anySimpleType >
    Content: ( annotation ?)
</ maxExclusive >

< maxInclusive
value = anySimpleType >
    Content: ( annotation ?)
</ maxInclusive >

< maxLength
value = anySimpleType >
    Content: ( annotation ?)
</ maxLength >

< minExclusive
value = anySimpleType >
    Content: ( annotation ?)
</ minExclusive >

< minInclusive
value = anySimpleType >
    Content: ( annotation ?)
</ minInclusive >

< minLength
value = anySimpleType >
    Content: ( annotation ?)
</ minLength >

< pattern
value = anySimpleType >
    Content: ( annotation ?)
</ pattern >

< totalDigits
value = anySimpleType >
    Content: ( annotation ?)
</ totalDigits >
```

## Annotations

```
< annotation >
    Content: (( appinfo | documentation )*)
</ annotation >

< appinfo
source = anyURI >
    Content: ({ any })*
</ appinfo >

< documentation
source = anyURI >
    Content: ({ any })*
</ documentation >
```

## Primitive Datatypes

All data types of the XML 1.1 specification are supported:

```
anyAtomicType
anySimpleType
anyType
anyURI
base64Binary
boolean
byte
date
dateTime
dayTimeDuration
decimal
double
duration
ENTITY
ENTITIES
float
gDay
gMonth
gMonthDay
gYear
gYearMonth
hexBinary
ID
IDREF
IDREFS
int
integer
language
long
Name
NCName
negativeInteger
NMTOKEN
NMTOKENS
nonNegativeInteger
nonPositiveInteger
normalizedString
NOTATION
positiveInteger
QName
short
string
time
token
unsignedByte
unsignedInt
unsignedLong
unsignedShort
untyped
untypedAtomic
yearMonthDuration
```