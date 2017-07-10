'Valid all #1':
{|xml||
	<all>
		<e3 />
		<e2 />
		<e0 />
	</all>
|}.

'Valid all #2':
{|xml||
	<all>
		<e0 />
		<e2 />
		<e3 />
	</all>
|}.

'Valid empty all':
{|xml||
	<all />
|}.

'Missing elements #1'(fail):
{|xml||
	<all>
		<e0 />
	</all>
|}.

'Missing elements #2'(fail):
{|xml||
	<all>
		<e0 />
		<e3 />
	</all>
|}.


'Too many elements #1'(fail):
{|xml||
	<all>
		<e2 />
		<e0 />
		<e3 />
		<unknown />
	</all>
|}.

'Too many elements #2'(fail):
{|xml||
	<all>
		<e2 />
		<e0 />
		<e3 />
		<e2 />
	</all>
|}.

'Too many elements #3'(fail):
{|xml||
	<all>
		<e0 />
		<e1 />
		<e2 />
		<e3 />
	</all>
|}.


