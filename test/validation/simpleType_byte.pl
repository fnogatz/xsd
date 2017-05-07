'Lower bound':
{|xml||
	<byte>-128</byte>
|}.

'Beyond lower bound'(fail):
{|xml||
	<byte>-129</byte>
|}.

'Zero':
{|xml||
	<byte>+0</byte>
|}.

'Upper bound':
{|xml||
	<byte>127</byte>
|}.

'Beyond upper bound'(fail):
{|xml||
	<byte>128</byte>
|}.

'Float is no xs:byte'(fail):
{|xml||
	<byte>-64.2</byte>
|}.

'Empty is no xs:byte'(fail):
{|xml||
	<byte></byte>
|}.