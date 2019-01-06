'xs:QName with valid prefix and local part':
{|xml||
	<QName>pre:myElement</QName>
|}.

'xs:QName with valid prefix and invalid local part'(fail):
{|xml||
	<QName>pre:-myelement</QName>
|}.

'xs:QName with invalid prefix and valid local part'(fail):
{|xml||
	<QName>.pre:myelement</QName>
|}.

'xs:QName with invalid prefix and invalid local part'(fail):
{|xml||
	<QName>.pre:.myelement</QName>
|}.

'xs:QName with valid local part':
{|xml||
	<QName>myElement</QName>
|}.

'xs:QName with invalid local part or missing prefix'(fail):
{|xml||
	<QName>:myElement</QName>
|}.

'xs:QName with invalid prefix or local part or too many parts'(fail):
{|xml||
	<QName>pre:pre:myElement</QName>
|}.

'xs:QName with empty value'(fail):
{|xml||
	<QName></QName>
|}.