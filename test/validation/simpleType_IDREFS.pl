'xs:IDREFS with one IDREF':
{|xml||
	<IDREFS>myElement</IDREFS>
|}.

'xs:IDREFS with two IDREFs':
{|xml||
	<IDREFS>myElement _myElement</IDREFS>
|}.

'xs:IDREFS with an invalid IDREF'(fail):
{|xml||
	<IDREFS>myElement3 3rdElement</IDREFS>
|}.

'xs:IDREFS with no IDREF'(fail):
{|xml||
	<IDREFS></IDREFS>
|}.