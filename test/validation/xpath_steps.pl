/* steps */
/*
'steps - child - arbitrary - true':
{|xml||
	<step_child_arbitrary>
		<element>1</element>
	</step_child_arbitrary>
|}.

'steps - child - arbitrary - false'(fail):
{|xml||
	<step_child_arbitrary>
	</step_child_arbitrary>
|}.
*/

'steps - child - specific - true':
{|xml||
	<step_child_specific>
		<element1>
			<element2>1</element2>
		</element1>
	</step_child_specific>
|}.

'steps - child - specific - false(inner)'(fail):
{|xml||
	<step_child_specific>
		<element1></element1>
	</step_child_specific>
|}.

'steps - child - specific - false(outer)'(fail):
{|xml||
	<step_child_specific>
	</step_child_specific>
|}.