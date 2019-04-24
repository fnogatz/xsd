/* steps */

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

'steps - slash - specific - true':
{|xml||
	<step_slash_specific>
		<element1>
			<element2>1</element2>
		</element1>
	</step_slash_specific>
|}.

'steps - slash - specific - false(inner)'(fail):
{|xml||
	<step_slash_specific>
		<element1></element1>
	</step_slash_specific>
|}.

'steps - slash - specific - false(outer)'(fail):
{|xml||
	<step_slash_specific>
	</step_slash_specific>
|}.


/* predicates */

'predicate - bool - true':
{|xml||
	<predicate_bool_true>
		<element>1</element>
	</predicate_bool_true>
|}.

'predicate - bool - false'(fail):
{|xml||
	<predicate_bool_false>
		<element>1</element>
	</predicate_bool_false>
|}.

'predicate - other - true':
{|xml||
	<predicate_other>
		<element>1</element>
	</predicate_other>
|}.


/* attribute */

'attribute presence required and given':
{|xml||
	<attribute_presence lang="en-US"/>
|}.

'attribute presence required and not given'(fail):
{|xml||
	<attribute_presence/>
|}.

'attribute value required and given':
{|xml||
	<attribute_comparison id="0"/>
|}.

'attribute value required and not given'(fail):
{|xml||
	<attribute_comparison id="1"/>
|}.

/*
NOT IMPLEMENTED YET
'attribute - child - bool - true':
{|xml||
	<attribute_child_bool>
		<element lang='de-DE'></element>
	</attribute_child_bool>
|}.

'attribute - child - bool - false'(fail):
{|xml||
	<attribute_child_bool>
		<element lang='de-CH'></element>
	</attribute_child_bool>
|}.

'attribute - child - other - true':
{|xml||
	<attribute_child_other>
		<element lang='de-DE'></element>
	</attribute_child_other>
|}.

'attribute - child - other - false'(fail):
{|xml||
	<attribute_child_other>
		<element></element>
	</attribute_child_other>
|}.
*/