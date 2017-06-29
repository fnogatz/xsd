'Fixed attribute value':
{|xml||
	<att str="hello" />
|}.

'Fixed attribute value not respected'(fail):
{|xml||
	<att str="some" />
|}.
