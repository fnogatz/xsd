'xs:NMTOKENS with one NMTOKEN':
{|xml||
	<NMTOKENS>ABCD</NMTOKENS>
|}.

'xs:NMTOKENS with two NMTOKENs':
{|xml||
	<NMTOKENS>ABCD 123_456</NMTOKENS>
|}.

'xs:NMTOKENS with two NMTOKENs and multiple spaces in-between':
{|xml||
	<NMTOKENS>ABCD  starts_with_spaces</NMTOKENS>
|}.

'xs:NMTOKENS with no NMTOKEN'(fail):
{|xml||
	<NMTOKENS></NMTOKENS>
|}.
