'Simple xs:string':
{|xml||
	<text>Test String</text>
|}.

'Empty xs:string':
{|xml||
	<text></text>
|}.

'Empty xs:string with short tag':
{|xml||
	<text />
|}.

'ComplexType is no xs:string'(fail):
{|xml||
	<text><string /></text>
|}.
