'xs:NCName with only letters':
{|xml||
	<NCName>myElement</NCName>
|}.

'xs:NCName beginning with an underscore':
{|xml||
	<NCName>_myElement</NCName>
|}.

'xs:NCName containing a number':
{|xml||
	<NCName>myElement3</NCName>
|}.

'xs:NCName with a hyphen in-between':
{|xml||
	<NCName>my-element</NCName>
|}.

'xs:NCName with a period in-between':
{|xml||
	<NCName>my.element</NCName>
|}.

'xs:NCName with a colon in-between'(fail):
{|xml||
	<NCName>pre:myelement3</NCName>
|}.

'xs:NCName beginning with a colon'(fail):
{|xml||
	<NCName>:myElement</NCName>
|}.

'xs:NCName beginning with a number'(fail):
{|xml||
	<NCName>3rdElement</NCName>
|}.

'xs:NCName beginning with a hyphen'(fail):
{|xml||
	<NCName>-myelement</NCName>
|}.

'xs:NCName beginning with a period'(fail):
{|xml||
	<NCName>.myelement</NCName>
|}.

'xs:NCName with an empty value'(fail):
{|xml||
	<NCName></NCName>
|}.