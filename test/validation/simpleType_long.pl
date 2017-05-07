'xs:long':
{|xml||
	<num>-21474836480</num>
|}.

'Float is no xs:long'(fail):
{|xml||
	<num>1.23</num>
|}.

'Out of bounds'(fail):
{|xml||
	<num>-9223372036854775809</num>
|}.
