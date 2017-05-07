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