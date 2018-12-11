'xs:dayTimeDuration with day, hour, minute and second properties':
{|xml||
	<dayTimeDuration>P5DT12H35M30S</dayTimeDuration>
|}.

'xs:dayTimeDuration with only day property':
{|xml||
	<dayTimeDuration>P7D</dayTimeDuration>
|}.

'xs:dayTimeDuration with only hour property':
{|xml||
	<dayTimeDuration>PT1H</dayTimeDuration>
|}.

'xs:dayTimeDuration with only minute property':
{|xml||
	<dayTimeDuration>PT120M</dayTimeDuration>
|}.

'xs:dayTimeDuration with only second property':
{|xml||
	<dayTimeDuration>PT12S</dayTimeDuration>
|}.

'xs:dayTimeDuration with all properties'(fail):
{|xml||
	<dayTimeDuration>P2Y6M5DT12H35M30S</dayTimeDuration>
|}.

'xs:dayTimeDuration with year property'(fail):
{|xml||
	<dayTimeDuration>P2Y5DT12H35M30S</dayTimeDuration>
|}.

'xs:dayTimeDuration with month property'(fail):
{|xml||
	<dayTimeDuration>P2M5DT12H35M30S</dayTimeDuration>
|}.