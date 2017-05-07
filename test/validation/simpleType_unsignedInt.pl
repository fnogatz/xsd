'xs:unsignedInt':
{|xml||
	<num>1234</num>
|}.

'Out of bounds'(fail):
{|xml||
	<num>18446744073709551615</num>
|}.
