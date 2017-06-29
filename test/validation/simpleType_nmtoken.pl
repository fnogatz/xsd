'NMTOKEN #1':
{|xml||
	<simple>a</simple>
|}.

'NMTOKEN #2':
{|xml||
	<simple>0-9</simple>
|}.

'NMTOKEN #3':
{|xml||
	<simple>1950-10-04</simple>
|}.

'NMTOKEN #4':
{|xml||
	<simple>US</simple>
|}.

'NMTOKEN must not be empty'(fail):
{|xml||
	<simple></simple>
|}.

'NMTOKEN must not contain dot'(fail):
{|xml||
	<simple>a.2</simple>
|}.
