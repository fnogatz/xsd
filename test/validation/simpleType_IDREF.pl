'xs:IDREF with only letters':
{|xml||
	<IDREF>myElement</IDREF>
|}.

'xs:IDREF beginning with an underscore':
{|xml||
	<IDREF>_myElement</IDREF>
|}.

'xs:IDREF containing a number':
{|xml||
	<IDREF>myElement3</IDREF>
|}.

'xs:IDREF with a hyphen in-between':
{|xml||
	<IDREF>my-element</IDREF>
|}.

'xs:IDREF with a period in-between':
{|xml||
	<IDREF>my.element</IDREF>
|}.

'xs:IDREF with a colon in-between'(fail):
{|xml||
	<IDREF>pre:myelement3</IDREF>
|}.

'xs:IDREF beginning with a colon'(fail):
{|xml||
	<IDREF>:myElement</IDREF>
|}.

'xs:IDREF beginning with a number'(fail):
{|xml||
	<IDREF>3rdElement</IDREF>
|}.

'xs:IDREF beginning with a hyphen'(fail):
{|xml||
	<IDREF>-myelement</IDREF>
|}.

'xs:IDREF beginning with a period'(fail):
{|xml||
	<IDREF>.myelement</IDREF>
|}.

'xs:IDREF with an empty value'(fail):
{|xml||
	<IDREF></IDREF>
|}.