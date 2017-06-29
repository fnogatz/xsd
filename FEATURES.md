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
name = NCName
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
    Content: ( annotation ?, ( minExclusive | minInclusive | maxExclusive | maxInclusive | enumeration )*)
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

```
< enumeration
value = anySimpleType >
    Content: ( annotation ?)
</ enumeration >

< minExclusive
value = anySimpleType >
    Content: ( annotation ?)
</ minExclusive >

< minInclusive
value = anySimpleType >
    Content: ( annotation ?)
</ minInclusive >

< maxExclusive
value = anySimpleType >
    Content: ( annotation ?)
</ maxExclusive >

< maxInclusive
value = anySimpleType >
    Content: ( annotation ?)
</ maxInclusive >
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

```
anyAtomicType
anySimpleType
boolean
byte
date
datetime
decimal
double
float
int
integer
long
negativeInteger
nonNegativeInteger
nonPositiveInteger
positiveInteger
short
string
time
unsignedByte
unsignedInt
unsignedLong
unsignedShort
NMTOKEN
```
