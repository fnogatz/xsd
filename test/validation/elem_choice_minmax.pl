'Empty choice (minOccurs=0)':
{|xml||
	<choice>
	</choice>
|}.

'Element multiple times (maxOccurs > 1)':
{|xml||
	<choice>
		<elem2 />
		<elem2 />
	</choice>
|}.

'number of nodes > maxOccurs'(fail):
{|xml||
	<choice>
		<elem2 />
		<elem2 />
		<elem2 />
	</choice>
|}.

