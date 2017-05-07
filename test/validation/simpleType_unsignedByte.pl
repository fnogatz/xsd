'xs:unsignedByte':
{|xml||
	<num>123</num>
|}.

'Out of bounds'(fail):
{|xml||
	<num>256</num>
|}.
