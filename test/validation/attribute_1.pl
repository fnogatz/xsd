'Should work #1':
{|xml||
	<att num="123" str="randString" />
|}.

'Should work #2':
{|xml||
	<att str="randString" num="123" />
|}.

'Should work #3':
{|xml||
	<att str="randString" />
|}.

'Should work #4':
{|xml||
	<att />
|}.

'Invalid Type'(fail):
{|xml||
	<att num="asd" />
|}.

'Unspecified Attribute'(fail):
{|xml||
	<att xx="true" />
|}.