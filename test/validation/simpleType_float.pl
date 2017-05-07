'Float':
{|xml||
	<float>-213.4e12</float>
|}.

'Infinity':
{|xml||
	<float>-INF</float>
|}.

'NaN':
{|xml||
	<float>NaN</float>
|}.

'Empty is no xs:float'(fail):
{|xml||
	<float></float>
|}.