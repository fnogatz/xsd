'global simpleType #1':
{|xml||
	<att my="12" />
|}.

'global simpleType #2':
{|xml||
	<att />
|}.

'global simpleType invalid'(fail):
{|xml||
	<att my="a" />
|}.

'nested simpleType #1':
{|xml||
	<att two="12" />
|}.

'nested simpleType invalid'(fail):
{|xml||
	<att two="a" />
|}.





