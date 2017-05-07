'xs:nonNegativeInteger':
{|xml||
	<num>1234</num>
|}.

'Out of bounds'(fail):
{|xml||
	<num>-32769</num>
|}.
