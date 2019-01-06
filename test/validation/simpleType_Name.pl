'xs:Name with only letters':
{|xml||
	<Name>myElement</Name>
|}.

'xs:Name beginning with an underscore':
{|xml||
	<Name>_myElement</Name>
|}.

'xs:Name beginning with a colon':
{|xml||
	<Name>:myElement</Name>
|}.

'xs:Name containing a number':
{|xml||
	<Name>myElement3</Name>
|}.

'xs:Name with a hyphen in-between':
{|xml||
	<Name>my-element</Name>
|}.

'xs:Name with a period in-between':
{|xml||
	<Name>my.element</Name>
|}.

'xs:Name with a colon in-between':
{|xml||
	<Name>pre:myelement3</Name>
|}.

'xs:Name beginning with a number'(fail):
{|xml||
	<Name>3rdElement</Name>
|}.

'xs:Name beginning with a hyphen'(fail):
{|xml||
	<Name>-myelement</Name>
|}.

'xs:Name beginning with a period'(fail):
{|xml||
	<Name>.myelement</Name>
|}.

'xs:Name with an empty value'(fail):
{|xml||
	<Name></Name>
|}.