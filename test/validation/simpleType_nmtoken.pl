'xs:NMTOKEN with letters':
{|xml||
	<NMTOKEN>ABCD</NMTOKEN>
|}.

'xs:NMTOKEN with numbers':
{|xml||
	<NMTOKEN>123_456</NMTOKEN>
|}.

'xs:NMTOKEN with leading spaces that get removed while parsing':
{|xml||
	<NMTOKEN>  starts_with_spaces</NMTOKEN>
|}.

'xs:NMTOKEN with trailing spaces that get removed while parsing':
{|xml||
	<NMTOKEN>ends_with_spaces  </NMTOKEN>
|}.

'xs:NMTOKEN with spaces in-between'(fail):
{|xml||
	<NMTOKEN>     </NMTOKEN>
|}.

'xs:NMTOKEN with empty value'(fail):
{|xml||
	<NMTOKEN></NMTOKEN>
|}.