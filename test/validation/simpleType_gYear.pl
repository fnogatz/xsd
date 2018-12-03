'xs:gYear with year only':
{|xml||
	<gYear>2004</gYear>
|}.

'xs:gYear with year and timezone offset':
{|xml||
	<gYear>2004-05:00</gYear>
|}.

'xs:gYear with a long year':
{|xml||
	<gYear>12004</gYear>
|}.

'xs:gYear with a short (not truncated) year':
{|xml||
	<gYear>0922</gYear>
|}.

'xs:gYear with a negative year':
{|xml||
	<gYear>-0045</gYear>
|}.

'xs:gYear with truncated century'(fail):
{|xml||
	<gYear>99</gYear>
|}.

'xs:gYear with illegally truncated leading zero'(fail):
{|xml||
	<gYear>992</gYear>
|}.

'xs:gYear with empty value'(fail):
{|xml||
	<gYear></gYear>
|}.