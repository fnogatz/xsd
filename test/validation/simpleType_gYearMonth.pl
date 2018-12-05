'xs:gYearMonth with year and month':
{|xml||
	<gYearMonth>2004-04</gYearMonth>
|}.

'xs:gYearMonth with large year and month':
{|xml||
	<gYearMonth>14024-04</gYearMonth>
|}.

'xs:gYearMonth with year and month and zero time zone offset':
{|xml||
	<gYearMonth>2004-04Z</gYearMonth>
|}.

'xs:gYearMonth with year and month and non-zero time zone offset':
{|xml||
	<gYearMonth>2004-04-05:00</gYearMonth>
|}.

'xs:gYearMonth with too many leading hyphens'(fail):
{|xml||
	<gYearMonth>-99-04</gYearMonth>
|}.

'xs:gYearMonth with truncated century'(fail):
{|xml||
	<gYearMonth>99-04</gYearMonth>
|}.

'xs:gYearMonth with truncated month'(fail):
{|xml||
	<gYearMonth>2004-4</gYearMonth>
|}.

'xs:gYearMonth without month'(fail):
{|xml||
	<gYearMonth>2004</gYearMonth>
|}.

'xs:gYearMonth with too small month'(fail):
{|xml||
	<gYearMonth>2004-00</gYearMonth>
|}.

'xs:gYearMonth with too large month'(fail):
{|xml||
	<gYearMonth>2004-13</gYearMonth>
|}.

'xs:gYearMonth with empty value'(fail):
{|xml||
	<gYearMonth></gYearMonth>
|}.