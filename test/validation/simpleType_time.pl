'Time #1':
{|xml||
	<simple>13:20:00</simple>
|}.

'Time #2':
{|xml||
	<simple>00:00:00</simple>
|}.

'Time with fractional seconds #1':
{|xml||
	<simple>12:34:56.789</simple>
|}.

'Time with fractional seconds #2':
{|xml||
	<simple>12:34:56.789000</simple>
|}.

'Time with zeroes as fractional seconds':
{|xml||
	<simple>12:34:56.000</simple>
|}.

'At most 24:00':
{|xml||
	<simple>24:00:00</simple>
|}.

'Not higher than 24:00 #1'(fail):
{|xml||
	<simple>24:01:00</simple>
|}.

'Not higher than 24:00 #2'(fail):
{|xml||
	<simple>24:00:01</simple>
|}.

'Not higher than 24:00 #3'(fail):
{|xml||
	<simple>24:00:00.001</simple>
|}.

'Not higher than 24:00 #4'(fail):
{|xml||
	<simple>25:00:00</simple>
|}.

'Time with positive timezone':
{|xml||
	<simple>13:20:00+12:34</simple>
|}.

'Time with negative timezone':
{|xml||
	<simple>13:20:00-12:34</simple>
|}.

'Time with timezone of at most 14:00':
{|xml||
	<simple>13:20:00-14:00</simple>
|}.

'Time with timezone of not higher than 14:00'(fail):
{|xml||
	<simple>13:20:00-14:01</simple>
|}.
