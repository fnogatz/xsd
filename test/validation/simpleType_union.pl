'memberTypes #1':
{|xml||
	<s0>-5</s0>
|}.

'memberTypes #2':
{|xml||
	<s0>b</s0>
|}.

'memberTypes #3'(fail):
{|xml||
	<s0>...</s0>
|}.

'memberTypes #4'(fail):
{|xml||
	<s0></s0>
|}.

'nested #1':
{|xml||
	<s1>a</s1>
|}.

'nested #2':
{|xml||
	<s1>e</s1>
|}.

'nested #3'(fail):
{|xml||
	<s1></s1>
|}.

'nested #4'(fail):
{|xml||
	<s1>12</s1>
|}.