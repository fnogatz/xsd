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