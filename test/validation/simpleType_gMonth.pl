'xs:gMonth with month only':
{|xml||
	<gMonth>--04</gMonth>
|}.

'xs:gMonth with month and zero time zone offset':
{|xml||
	<gMonth>--04Z</gMonth>
|}.

'xs:gMonth with month and non-zero time zone offset':
{|xml||
	<gMonth>--04-05:00</gMonth>
|}.

'xs:gMonth with year specified'(fail):
{|xml||
	<gMonth>2004-04</gMonth>
|}.

'xs:gMonth without one leading hyphen'(fail):
{|xml||
	<gMonth>-04</gMonth>
|}.

'xs:gMonth without all leading hyphens'(fail):
{|xml||
	<gMonth>04</gMonth>
|}.

'xs:gMonth with truncated month value'(fail):
{|xml||
	<gMonth>--4</gMonth>
|}.

'xs:gMonth with too small value'(fail):
{|xml||
	<gMonth>--00</gMonth>
|}.

'xs:gMonth with too large value'(fail):
{|xml||
	<gMonth>--13</gMonth>
|}.

'xs:gYear with too large time zone offset'(fail):
{|xml||
	<gYear>--04-15:00</gYear>
|}.

'xs:gYear with invalid time zone offset'(fail):
{|xml||
	<gYear>--04-09:60</gYear>
|}.

'xs:gMonth with empty value'(fail):
{|xml||
	<gMonth></gMonth>
|}.