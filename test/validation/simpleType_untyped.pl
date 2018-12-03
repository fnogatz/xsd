'Arbitrary xs:untyped':
{|xml||
	<untyped>ß汉98af é▓fioe</untyped>
|}.

'Empty xs:untyped':
{|xml||
	<untyped />
|}.

'xs:untyped may not contain a complex type'(fail):
{|xml||
	<untyped><complex /></untyped>
|}.