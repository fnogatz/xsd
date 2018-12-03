'Arbitrary xs:anyType':
{|xml||
	<anyType>ß汉98af é▓fioe</anyType>
|}.

'Empty xs:anyType':
{|xml||
	<anyType />
|}.

'xs:anyType may not contain a complex type'(fail):
{|xml||
	<anyType><complex /></anyType>
|}.