'Basic choice #1':
{|xml||
	<basic />
|}.

'Basic choice #2':
{|xml||
	<basic>
		<e2 />
	</basic>
|}.

'Basic choice #3':
{|xml||
	<basic>
		<e3 />
		<e3 />
		<e3 />
	</basic>
|}.

'Basic choice #4':
{|xml||
	<basic>
		<e2 />
		<e3 />
		<e1 />
	</basic>
|}.

'Invalid basic choice #1'(fail):
{|xml||
	<basic>
		<e1 />
		<e2 />
		<e3 />
		<e4 />
	</basic>
|}.

'Invalid basic choice #2'(fail):
{|xml||
	<basic>
		<e1 />
		<e2 />
		<e3 />
		<e1 />
	</basic>
|}.

'Choice unbounded #1':
{|xml||
	<unbounded>
		<e3 />
		<e3 />
		<e3 />
		<e3 />
	</unbounded>
|}.

'Choice unbounded #2':
{|xml||
	<unbounded>
		<e1 />
		<e3 />
		<e1 />
		<e2 />
		<e3 />
		<e3 />
	</unbounded>
|}.

'Choice minOccurs #3'(fail):
{|xml||
	<unbounded>
		<e3 />
	</unbounded>
|}.

'Valid choice #1':
{|xml||
	<choice />
|}.

'Valid choice #2':
{|xml||
	<choice>
		<e1 /><e1 />
		<e4 />
	</choice>
|}.

'Valid choice #3':
{|xml||
	<choice>
		<e5 /><e6 />
		<e2 /><e2 />
		<e1 /><e1 />
	</choice>
|}.

'Valid choice #4':
{|xml||
	<choice>
		<e3 /><e3 />
		<e4 />
		<e1 /><e1 />
	</choice>
|}.

'Valid choice #5':
{|xml||
	<choice>
		<e1 /><e1 />
		<e1 /><e1 />
		<e4 />
	</choice>
|}.

'Valid choice #6':
{|xml||
	<choice>
		<e1 /><e1 />
		<e2 />
		<e3 />
	</choice>
|}.

'Valid choice #7':
{|xml||
	<choice>
		<e5 />
		<e6 />
		<e5 />
		<e6 />
		<e5 />
		<e6 />
	</choice>
|}.

'Invalid choice #1'(fail):
{|xml||
	<choice>
		<e4 />
		<e6 />
		<e5 />
		<e4 />
	</choice>
|}.

'Invalid choice #2'(fail):
{|xml||
	<choice>
		<e4 />
		<e4 />
		<e4 />
		<e4 />
	</choice>
|}.

'Invalid choice #3'(fail):
{|xml||
	<choice>
		<e5 />
		<e6 />
		<e5 />
		<e6 />
		<e5 />
		<e6 />
		<e5 />
		<e6 />
	</choice>
|}.
