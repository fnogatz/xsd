'Arbitrary xs:anySimpleType':
{|xml||
	<anySimpleType>ß汉98af é▓fioe</anySimpleType>
|}.

'Empty xs:anySimpleType':
{|xml||
	<anySimpleType />
|}.

'xs:anySimpleType may not contain a complex type'(fail):
{|xml||
	<anySimpleType><complex /></anySimpleType>
|}.