'Empty sequence (minOccurs=0)':
{|xml||
	<list></list>
|}.

'Single Element':
{|xml||
	<list>
		<node />
	</list>
|}.

'number of nodes in range #1':
{|xml||
	<list>
		<node />
		<node />
	</list>
|}.

'number of nodes in range #2':
{|xml||
	<list>
		<node />
		<node />
		<node />
		<node />
		<node />
	</list>
|}.

'number of nodes > maxOccurs'(fail):
{|xml||
	<list>
		<node />
		<node />
		<node />
		<node />
		<node />
		<node />
	</list>
|}.