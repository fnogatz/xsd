'Should work #1':
{|xml||
	<sequence>
		<e1 />
	</sequence>
|}.

'Should work #2':
{|xml||
	<sequence>
		<e1 />
		<e2 />
	</sequence>
|}.

'Should work #3':
{|xml||
	<sequence>
		<e1 />
		<e3 />
		<e3 />
	</sequence>
|}.

'Should work #4':
{|xml||
	<sequence>
		<e1 />
		<e4 />
	</sequence>
|}.

'Should work #5':
{|xml||
	<sequence>
		<e1 />
		<e3 />
		<e3 />
		<e4 />
	</sequence>
|}.

'Should work #6':
{|xml||
	<sequence>
		<e1 />
		<e2 />
		<e4 />
		<e4 />
	</sequence>
|}.

'Invalid sequence #1'(fail):
{|xml||
	<sequence>
		<e1 />
		<e2 />
		<e3 />
		<e3 />
		<e4 />
	</sequence>
|}.

'Invalid sequence #2'(fail):
{|xml||
	<sequence>
		<e2 />
		<e4 />
	</sequence>
|}.