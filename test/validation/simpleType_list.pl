'list of xs:integer #1':
{|xml||
	<s0>1 -5 0 12</s0>
|}.

'list of xs:integer #2':
{|xml||
	<s0> 1 -5   0 12 </s0>
|}.

'list of xs:integer #3':
{|xml||
	<s0></s0>
|}.

'list of xs:integer #4':
{|xml||
	<s0> </s0>
|}.

'list of xs:integer #5'(fail):
{|xml||
	<s0>1 zwei 3</s0>
|}.

'list of simpleType #1':
{|xml||
	<s1>f e d</s1>
|}.

'list of simpleType #2'(fail):
{|xml||
	<s1>d e f g</s1>
|}.

'list of simpleType #3':
{|xml||
	<s2>a b b b</s2>
|}.

'list of simpleType #4'(fail):
{|xml||
	<s2>abc</s2>
|}.
