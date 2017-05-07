'Valid choice #1':
{|xml||
	<choice>
		<elem1 />
	</choice>
|}.

'Valid choice #2':
{|xml||
	<choice>
		<elem2 />
	</choice>
|}.

'Valid choice #3':
{|xml||
	<choice>
		<elem3 />
	</choice>
|}.

'Multiple elements'(fail):
{|xml||
	<choice>
		<elem2 />
		<elem3 />
	</choice>
|}.

'Unspecified element'(fail):
{|xml||
	<choice>
		<other />
	</choice>
|}.
