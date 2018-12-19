'xs:NOTATION with valid prefix and local part':
{|xml||
	<NOTATION>pre:myElement</NOTATION>
|}.

'xs:NOTATION with valid prefix and invalid local part'(fail):
{|xml||
	<NOTATION>pre:-myelement</NOTATION>
|}.

'xs:NOTATION with invalid prefix and valid local part'(fail):
{|xml||
	<NOTATION>.pre:myelement</NOTATION>
|}.

'xs:NOTATION with invalid prefix and invalid local part'(fail):
{|xml||
	<NOTATION>.pre:.myelement</NOTATION>
|}.

'xs:NOTATION with valid local part':
{|xml||
	<NOTATION>myElement</NOTATION>
|}.

'xs:NOTATION with invalid local part or missing prefix'(fail):
{|xml||
	<NOTATION>:myElement</NOTATION>
|}.

'xs:NOTATION with invalid prefix or local part or too many parts'(fail):
{|xml||
	<NOTATION>pre:pre:myElement</NOTATION>
|}.

'xs:NOTATION with empty value'(fail):
{|xml||
	<NOTATION></NOTATION>
|}.