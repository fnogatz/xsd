'Reference element':
{|xml||
	<root>
		<some>str</some>
	</root>
|}.

'Reference element missing'(fail):
{|xml||
	<root>
	</root>
|}.

'Reference element invalid #1'(fail):
{|xml||
	<root>
		<some random="too much" />
	</root>
|}.

'Reference element invalid #2'(fail):
{|xml||
	<root>
		<other />
	</root>
|}.
