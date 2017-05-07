'Should work #1':
{|xml||
	<all>
		<e1 />
		<e4 />
	</all>
|}.

'Should work #2':
{|xml||
	<all>
		<e4 />
		<e1 />
		<e2 />
	</all>
|}.

'Should work #3':
{|xml||
	<all>
		<e2 />
		<e4 />
		<e1 />
	</all>
|}.

'Should work #4':
{|xml||
	<all>
		<e1 />
		<e2 />
		<e3 />
		<e4 />
	</all>
|}.

'Should work #5':
{|xml||
	<all>
		<e2 />
		<e4 />
		<e3 />
		<e1 />
	</all>
|}.

'Missing elements #1'(fail):
{|xml||
	<all>
	</all>
|}.

'Missing elements #2'(fail):
{|xml||
	<all>
		<e1 />
	</all>
|}.

'Out of bounds (min/maxOccurs) #1'(fail):
{|xml||
	<all>
		<e1 />
		<e2 />
		<e3 />
		<e4 />
		<e0 />
	</all>
|}.

'Out of bounds (min/maxOccurs) #2'(fail):
{|xml||
	<all>
		<e0 />
		<e1 />
		<e2 />
		<e3 />
		<e4 />
	</all>
|}.

'Out of bounds (min/maxOccurs) #3'(fail):
{|xml||
	<all>
		<e1 />
		<e4 />
		<e1 />
	</all>
|}.

'Out of bounds (min/maxOccurs) #4'(fail):
{|xml||
	<all>
		<e0 />
	</all>
|}.