'Valid sequence #1':
{|xml||
	<elem1>
		<node />
	</elem1>
|}.

'Valid sequence #2':
{|xml||
	<elem1>
		<node />
		<node />
		<node />
	</elem1>
|}.

'Valid sequence #3':
{|xml||
	<elem2>
		<node1 />
		<node3 />
	</elem2>
|}.

'Valid sequence #4':
{|xml||
	<elem2>
		<node2 />
	</elem2>
|}.

'Valid sequence #5':
{|xml||
	<elem2 />
|}.

'Valid sequence #6':
{|xml||
	<elem2>
		<node1 />
		<node2 />
		<node3 />
	</elem2>
|}.

'Valid sequence #7':
{|xml||
	<elem3>
		<node1 /><node1 /><node1 />
		<node2 /><node2 /><node2 />
		<node3 /><node3 /><node3 />
	</elem3>
|}.

'Invalid sequence #'(fail):
{|xml||
	<elem3>
		<node2 /><node2 /><node2 />
		<node3 />
	</elem3>
|}.

