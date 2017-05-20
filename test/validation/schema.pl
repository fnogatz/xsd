'Schema with multiple root elements #1':
{|xml||
	<num>123</num>
|}.

'Schema with multiple root elements #2':
{|xml||
	<text />
|}.

'No such root element'(fail):
{|xml||
	<other />
|}.

'Support xmlns:xsi':
{|xml||
	<num xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">123</num>
|}.