'Simple xs:anyAtomicType':
{|xml||
	<any>Test String</any>
|}.

'Empty xs:anyAtomicType':
{|xml||
	<any />
|}.

'Complex content is no xs:anyAtomicType'(fail):
{|xml||
	<any><complex /></any>
|}.