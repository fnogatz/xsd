'Date #1':
{|xml||
	<simple>1999-10-20</simple>
|}.

'Date with zero timezone':
{|xml||
	<simple>1999-10-20+00:00</simple>
|}.

'Full date'(fail):
{|xml||
	<simple>1999-1-1</simple>
|}.

'Date with negative timezone':
{|xml||
	<simple>1999-10-20-03:45</simple>
|}.

'Date with positive timezone':
{|xml||
	<simple>1999-10-20+03:45</simple>
|}.

'Date with 0-leading 4+ digit year not allowed'(fail):
{|xml||
	<simple>01999-10-20</simple>
|}.

'Date with non-0-leading 4+ digit year allowed':
{|xml||
	<simple>11999-10-20</simple>
|}.

'Invalid month'(fail):
{|xml||
	<simple>1999-13-20</simple>
|}.

'Invalid day'(fail):
{|xml||
	<simple>1999-10-32</simple>
|}.

'Timezone not higher than 14:00 #1'(fail):
{|xml||
	<simple>1999-10-20+15:00</simple>
|}.

'Timezone not higher than 14:00 #2':
{|xml||
	<simple>1999-10-20+14:00</simple>
|}.

'Timezone not higher than 14:00 #3'(fail):
{|xml||
	<simple>1999-10-20+14:01</simple>
|}.
