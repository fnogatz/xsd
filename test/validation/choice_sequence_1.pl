'Should work #1':
{|xml||
	<choice>
		<e1 />
	</choice>
|}.

'Should work #2':
{|xml||
	<choice>
		<e2 />
	</choice>
|}.

'Should work #3':
{|xml||
	<choice>
		<e3 />
	</choice>
|}.

'Should work #4':
{|xml||
	<choice>
		<e4 />
	</choice>
|}.

'Should work #5':
{|xml||
	<choice>
		<e2 />
		<e3 />
	</choice>
|}.

'Should work #6':
{|xml||
	<choice>
	</choice>
|}.

'Too many elements #1'(fail):
{|xml||
	<choice>
		<e1 />
		<e2 />
	</choice>
|}.

'Too many elements #2'(fail):
{|xml||
	<choice>
		<e3 />
		<e4 />
	</choice>
|}.