'xs:ENTITY with only letters':
{|xml||
	<ENTITY>myElement</ENTITY>
|}.

'xs:ENTITY beginning with an underscore':
{|xml||
	<ENTITY>_myElement</ENTITY>
|}.

'xs:ENTITY containing a number':
{|xml||
	<ENTITY>myElement3</ENTITY>
|}.

'xs:ENTITY with a hyphen in-between':
{|xml||
	<ENTITY>my-element</ENTITY>
|}.

'xs:ENTITY with a period in-between':
{|xml||
	<ENTITY>my.element</ENTITY>
|}.

'xs:ENTITY with a colon in-between'(fail):
{|xml||
	<ENTITY>pre:myelement3</ENTITY>
|}.

'xs:ENTITY beginning with a colon'(fail):
{|xml||
	<ENTITY>:myElement</ENTITY>
|}.

'xs:ENTITY beginning with a number'(fail):
{|xml||
	<ENTITY>3rdElement</ENTITY>
|}.

'xs:ENTITY beginning with a hyphen'(fail):
{|xml||
	<ENTITY>-myelement</ENTITY>
|}.

'xs:ENTITY beginning with a period'(fail):
{|xml||
	<ENTITY>.myelement</ENTITY>
|}.

'xs:ENTITY with an empty value'(fail):
{|xml||
	<ENTITY></ENTITY>
|}.