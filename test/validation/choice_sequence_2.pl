'Should work #1':
{|xml||
	<choice>
		<e1 />
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
		<e2 />
		<e2 />
		<e3 />
		<e3 />
	</choice>
|}.

'Should work #4':
{|xml||
	<choice>
		<e2 />
	</choice>
|}.

'Should work #5':
{|xml||
	<choice>
		<e4 />
	</choice>
|}.

'Should work #6':
{|xml||
	<choice>
	</choice>
|}.

'number of nodes < minOccurs'(fail):
{|xml||
	<choice>
		<e1 />
	</choice>
|}.

'number of nodes > maxOccurs'(fail):
{|xml||
	<choice>
		<e4 />
		<e4 />
	</choice>
|}.

'Invalid choice #1'(fail):
{|xml||
	<choice>
		<e1 />
		<e1 />
		<e3 />
	</choice>
|}.

'Invalid choice #2'(fail):
{|xml||
	<choice>
		<e3 />
		<e3 />
	</choice>
|}.

'Invalid choice #3'(fail):
{|xml||
	<choice>
		<e3 />
	</choice>
|}.