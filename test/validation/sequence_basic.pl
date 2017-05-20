'Basic sequence':
{|xml||
	<kundendaten>
		<kdnr>123</kdnr>
		<anrede>Herr</anrede>
		<vorname>Peter</vorname>
		<nachname>Schmidt</nachname>
	</kundendaten>
|}.

'Empty sequence'(fail):
{|xml||
	<kundendaten>
	</kundendaten>
|}.

'Missing elements in sequence'(fail):
{|xml||
	<kundendaten>
		<kdnr>123</kdnr>
		<vorname>Peter</vorname>
		<nachname>Schmidt</nachname>
	</kundendaten>
|}.

'Wrong order in sequence'(fail):
{|xml||
	<kundendaten>
		<kdnr>123</kdnr>
		<anrede>Herr</anrede>
		<nachname>Schmidt</nachname>
		<vorname>Peter</vorname>
	</kundendaten>
|}.

'Unspecified elements in sequence'(fail):
{|xml||
	<kundendaten>
		<kdnr>123</kdnr>
		<anrede>Herr</anrede>
		<vorname>Peter</vorname>
		<nachname>Schmidt</nachname>
		<telefon>0302010</telefon>
	</kundendaten>
|}.

'Out of bounds (maxOccurs)'(fail):
{|xml||
	<kundendaten>
		<kdnr>123</kdnr>
		<anrede>Herr</anrede>
		<vorname>Peter</vorname>
		<vorname>Maximilian</vorname>
		<nachname>Schmidt</nachname>
	</kundendaten>
|}.