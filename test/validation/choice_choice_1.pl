'Valid choice #1':
{|xml||
	<choice>
		<e1 />
		<e1 />
	</choice>
|}.

'Valid choice #2':
{|xml||
	<choice>
		<e2 />
	</choice>
|}.

'Valid choice #3':
{|xml||
	<choice>
		<e3 />
		<e3 />
	</choice>
|}.

'Valid choice #4':
{|xml||
	<choice>
	</choice>
|}.

'Valid choice #5':
{|xml||
	<choice>
		<e4 />
	</choice>
|}.

'Invalid choice #1'(fail):
{|xml||
	<choice>
		<e2 />
		<e3 />
	</choice>
|}.

'Invalid choice #'(fail):
{|xml||
	<choice>
		<e2 />
		<e4 />
	</choice>
|}.