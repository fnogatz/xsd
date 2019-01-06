'xs:gMonthDay with month and day only':
{|xml||
	<gMonthDay>--04-12</gMonthDay>
|}.

'xs:gMonthDay with month and day and time zone':
{|xml||
	<gMonthDay>--04-12Z</gMonthDay>
|}.

'xs:gMonthDay without one leading hyphen'(fail):
{|xml||
	<gMonthDay>-04-12</gMonthDay>
|}.

'xs:gMonthDay without all leading hyphens'(fail):
{|xml||
	<gMonthDay>04-12</gMonthDay>
|}.

'xs:gMonthDay with truncated month'(fail):
{|xml||
	<gMonthDay>--4-06</gMonthDay>
|}.

'xs:gMonthDay with truncated day'(fail):
{|xml||
	<gMonthDay>--04-6</gMonthDay>
|}.

'xs:gMonthDay with both truncated month and day'(fail):
{|xml||
	<gMonthDay>--4-6</gMonthDay>
|}.

'xs:gMonthDay with too small month'(fail):
{|xml||
	<gMonthDay>--00-31</gMonthDay>
|}.

'xs:gMonthDay with too large month'(fail):
{|xml||
	<gMonthDay>--13-31</gMonthDay>
|}.

'xs:gMonthDay with too small day'(fail):
{|xml||
	<gMonthDay>--05-00</gMonthDay>
|}.

'xs:gMonthDay with too large day'(fail):
{|xml||
	<gMonthDay>--05-32</gMonthDay>
|}.

'xs:gMonthDay with invalid month-day-combination'(fail):
{|xml||
	<gMonthDay>--04-31</gMonthDay>
|}.

'xs:gMonthDay with empty value'(fail):
{|xml||
	<gMonthDay></gMonthDay>
|}.