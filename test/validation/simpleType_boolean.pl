'xs:boolean as number':
{|xml||
	<bool>0</bool>
|}.

'xs:boolean as keyword':
{|xml||
	<bool>true</bool>
|}.

'Empty xs:boolean'(fail):
{|xml||
	<bool></bool>
|}.

'Integer is no xs:boolean'(fail):
{|xml||
	<bool>123</bool>
|}.