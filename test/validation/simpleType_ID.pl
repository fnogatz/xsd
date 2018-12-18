'xs:ID with only letters':
{|xml||
	<ID>myElement</ID>
|}.

'xs:ID beginning with an underscore':
{|xml||
	<ID>_myElement</ID>
|}.

'xs:ID containing a number':
{|xml||
	<ID>myElement3</ID>
|}.

'xs:ID with a hyphen in-between':
{|xml||
	<ID>my-element</ID>
|}.

'xs:ID with a period in-between':
{|xml||
	<ID>my.element</ID>
|}.

'xs:ID with a colon in-between'(fail):
{|xml||
	<ID>pre:myelement3</ID>
|}.

'xs:ID beginning with a colon'(fail):
{|xml||
	<ID>:myElement</ID>
|}.

'xs:ID beginning with a number'(fail):
{|xml||
	<ID>3rdElement</ID>
|}.

'xs:ID beginning with a hyphen'(fail):
{|xml||
	<ID>-myelement</ID>
|}.

'xs:ID beginning with a period'(fail):
{|xml||
	<ID>.myelement</ID>
|}.

'xs:ID with an empty value'(fail):
{|xml||
	<ID></ID>
|}.