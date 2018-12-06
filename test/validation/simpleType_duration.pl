'xs:duration with all properties':
{|xml||
	<duration>P2Y6M5DT12H35M30S</duration>
|}.

'xs:duration with day and hour properties':
{|xml||
	<duration>P1DT2H</duration>
|}.

'xs:duration with redundant zero properties':
{|xml||
	<duration>P0Y20M0D</duration>
|}.

'xs:duration with month property':
{|xml||
	<duration>P20M</duration>
|}.

'xs:duration with minute property':
{|xml||
	<duration>PT20M</duration>
|}.

'xs:duration with effectively no duration':
{|xml||
	<duration>P0Y</duration>
|}.

'xs:duration with negative duration':
{|xml||
	<duration>-P60D</duration>
|}.

'xs:duration with milliseconds':
{|xml||
	<duration>PT1M30.5S</duration>
|}.

'xs:duration with invalid minus sign position'(fail):
{|xml||
	<duration>P-20M</duration>
|}.

'xs:duration with T, but without any time property'(fail):
{|xml||
	<duration>P20MT</duration>
|}.

'xs:duration with M, but without month property'(fail):
{|xml||
	<duration>P1YM5D</duration>
|}.

'xs:duration with fractional year property'(fail):
{|xml||
	<duration>P15.5Y</duration>
|}.

'xs:duration without T between day and time properties'(fail):
{|xml||
	<duration>P1D2H</duration>
|}.

'xs:duration without P'(fail):
{|xml||
	<duration>1Y2M</duration>
|}.

'xs:duration with invalid property order'(fail):
{|xml||
	<duration>P2M1Y</duration>
|}.

'xs:duration without any property'(fail):
{|xml||
	<duration>P</duration>
|}.

'xs:duration without digit after the decimal point of seconds property'(fail):
{|xml||
	<duration>PT15.S</duration>
|}.

'xs:duration with empty value'(fail):
{|xml||
	<duration></duration>
|}.