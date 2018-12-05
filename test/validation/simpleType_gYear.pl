'xs:gYear with year only':
{|xml||
	<gYear>2004</gYear>
|}.

'xs:gYear with year and zero time zone offset':
{|xml||
	<gYear>2004Z</gYear>
|}.

'xs:gYear with year and non-zero time zone offset':
{|xml||
	<gYear>2004-05:25</gYear>
|}.

'xs:gYear with a long positive year':
{|xml||
	<gYear>12004</gYear>
|}.

'xs:gYear with a short positive year':
{|xml||
	<gYear>0922</gYear>
|}.

'xs:gYear with a long negative year':
{|xml||
	<gYear>-12045</gYear>
|}.

'xs:gYear with a short negative year':
{|xml||
	<gYear>-0045</gYear>
|}.

'xs:gYear with too many leading hyphens'(fail):
{|xml||
	<gYear>-99</gYear>
|}.

'xs:gYear with truncated century'(fail):
{|xml||
	<gYear>99</gYear>
|}.

'xs:gYear with illegally truncated leading zero'(fail):
{|xml||
	<gYear>992</gYear>
|}.

'xs:gYear with too large time zone offset'(fail):
{|xml||
	<gYear>2004-15:00</gYear>
|}.

'xs:gYear with invalid time zone offset'(fail):
{|xml||
	<gYear>2004-13:60</gYear>
|}.

'xs:gYear with empty value'(fail):
{|xml||
	<gYear></gYear>
|}.