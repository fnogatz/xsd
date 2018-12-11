'xs:yearMonthDuration with year and month property':
{|xml||
	<yearMonthDuration>P2Y6M</yearMonthDuration>
|}.

'xs:yearMonthDuration with only year property':
{|xml||
	<yearMonthDuration>P23Y</yearMonthDuration>
|}.

'xs:yearMonthDuration with only month property':
{|xml||
	<yearMonthDuration>P20M</yearMonthDuration>
|}.

'xs:yearMonthDuration with redundant year property':
{|xml||
	<yearMonthDuration>P0Y20M</yearMonthDuration>
|}.

'xs:yearMonthDuration with all properties'(fail):
{|xml||
	<yearMonthDuration>P2Y6M5DT12H35M30S</yearMonthDuration>
|}.

'xs:yearMonthDuration with day property'(fail):
{|xml||
	<yearMonthDuration>P2Y6M5D</yearMonthDuration>
|}.

'xs:yearMonthDuration with hour property'(fail):
{|xml||
	<yearMonthDuration>P2Y6MT5H</yearMonthDuration>
|}.

'xs:yearMonthDuration with minute property'(fail):
{|xml||
	<yearMonthDuration>P2Y6MT5M</yearMonthDuration>
|}.

'xs:yearMonthDuration with second property'(fail):
{|xml||
	<yearMonthDuration>P2Y6MT5S</yearMonthDuration>
|}.