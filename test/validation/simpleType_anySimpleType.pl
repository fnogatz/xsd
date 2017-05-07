'Simple xs:anySimpleType':
{|xml||
	<any>Test String</any>
|}.

'Empty xs:anySimpleType':
{|xml||
	<any />
|}.

'Complex content is no xs:anySimpleType'(fail):
{|xml||
	<any><complex /></any>
|}.