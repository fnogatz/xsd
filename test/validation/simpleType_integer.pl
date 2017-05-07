'Simple xs:integer':
{|xml||
	<num>123</num>
|}.

'Signed xs:integer':
{|xml||
	<num>+123</num>
|}.

'Negative xs:integer':
{|xml||
	<num>-123</num>
|}.

'String is no xs:integer'(fail):
{|xml||
	<num>abc</num>
|}.

'Float is no xs:integer'(fail):
{|xml||
	<num>1.23</num>
|}.

'ComplexType is no xs:integer'(fail):
{|xml||
	<num><int /></num>
|}.
