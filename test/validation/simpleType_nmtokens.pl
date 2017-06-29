'NMTOKENS #1':
{|xml||
	<simple>a</simple>
|}.

'NMTOKENS #2':
{|xml||
	<simple>0-9</simple>
|}.

'NMTOKENS #3':
{|xml||
	<simple>1950-10-04</simple>
|}.

'NMTOKENS #4':
{|xml||
	<simple>a 1 b</simple>
|}.

'NMTOKENS must not be empty'(fail):
{|xml||
	<simple></simple>
|}.
