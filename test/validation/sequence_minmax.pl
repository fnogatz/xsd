% basic 
'Basic sequence #1':
{|xml||
	<basic />
|}.

'Basic sequence #2':
{|xml||
	<basic>
		<e1 />
		<e2 />
		<e3 />
	</basic>
|}.

'Basic sequence #3':
{|xml||
	<basic>
		<e1 />
		<e2 />
		<e3 />
		
		<e1 />
		<e2 />
		<e3 />
	</basic>
|}.

'Invalid basic sequence #1'(fail):
{|xml||
	<basic>
		<e1 />
		<e2 />
		<e3 />

		<e2 />
		<e3 />
	</basic>
|}.

'Invalid basic sequence #2'(fail):
{|xml||
	<basic>
		<e1 />
		<e2 />
		<e3 />
		
		<e1 />
		<e2 />
		<e3 />
		
		<e1 />
		<e2 />
		<e3 />
	</basic>
|}.

'Invalid basic sequence #3'(fail):
{|xml||
	<basic>
		<e1 />
		<e2 />
		<e3 />
		
		<e1 />
			<e3 />
			<e2 />
			
		<e1 />
		<e2 />
		<e3 />
	</basic>
|}.

% unbounded
'Unbounded sequence #1':
{|xml||
	<unbounded>
		<e1 />
		<e2 />
		<e3 />
		
		<e1 />
		<e2 />
		<e3 />
		
		<e1 />
		<e2 />
		<e3 />
		
		<e1 />
		<e2 />
		<e3 />
	</unbounded>
|}.

'MinOccurs sequence #1'(fail):
{|xml||
	<unbounded>
		<e1 />
		<e2 />
		<e3 />
	</unbounded>
|}.

'MinOccurs sequence #2':
{|xml||
	<min />
|}.

% seq
'Valid sequence #1':
{|xml||
	<seq>
		<e1 />
		<e2 />
		<e4 />
		<e5 />
		<e6 />
	</seq>
|}.

'Valid sequence #2':
{|xml||
	<seq>
		<e2 />
		<e4 />
		<e5 />
		<e6 />
		
		<e1 /><e1 />
		<e3 />
		<e4 />
		<e5 />
	</seq>
|}.

'Valid sequence #3':
{|xml||
	<seq>
		<e1 />
		
		<e1 /><e1 />
		<e3 /><e3 />
		<e4 />
		<e6 />
	</seq>
|}.

'Valid sequence #4':
{|xml||
	<seq>
		<e4 />
		
		<e1 />
	</seq>
|}.

'Invalid sequence #1'(fail):
{|xml||
	<seq>
		<e1 />
		<e1 />
		
		<e1 />
		<e3 />
		
		<e2 />
	</seq>
|}.

'Invalid sequence #2'(fail):
{|xml||
	<seq>
		<e4 />
		
		<e4 />
		
		<e4 />
	</seq>
|}.

