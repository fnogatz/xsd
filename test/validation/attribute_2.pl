'Should work #1':
{|xml||
	<att num="123" str="randString" id="5" />
|}.

'Should work #2':
{|xml||
	<att str="randString" num="123" id="5" />
|}.

'Should work #3':
{|xml||
	<att str="randString" id="5" />
|}.

'Should work #4':
{|xml||
	<att id="5" />
|}.

'Invalid Type'(fail):
{|xml||
	<att num="asd" id="5" />
|}.

'Required missing'(fail):
{|xml||
	<att num="asd" />
|}.

'Unspecified attribute'(fail):
{|xml||
	<att xx="true" id="5" />
|}.

'Prohibited attribute'(fail):
{|xml||
	<att id="5" never="aloha" />
|}.