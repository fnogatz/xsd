'Should work':
{|xml||
	<?xml version="1.0" encoding="UTF-8"?>
	<kundenliste>
		<kunde>
			<kundennr>1001</kundennr>
			<anrede>Frau</anrede>
			<vorname>Maria</vorname>
			<nachname>Slonka</nachname>
			<email/>
		</kunde>
		<kunde>
			<kundennr>1002</kundennr>
			<anrede>Herr</anrede>
			<vorname>Gerd</vorname>
			<nachname>Wieden</nachname>
			<email/>
		</kunde>
	</kundenliste>
|}.

'Should work #2':
{|xml||
	<?xml version="1.0" encoding="UTF-8"?>
	<kundenliste>
		<kunde>
			<kundennr>1001</kundennr>
			<anrede>Frau</anrede>
			<vorname>Maria</vorname>
			<nachname>Slonka</nachname>
			<email/>
		</kunde>
		<kunde>
			<kundennr>1002</kundennr>
			<anrede>Herr</anrede>
			<vorname>Gerd</vorname>
			<nachname>Wieden</nachname>
			<email/>
		</kunde>
		<kunde>
			<kundennr>1002</kundennr>
			<anrede>Herr</anrede>
			<vorname>Gerd</vorname>
			<nachname>Wieden</nachname>
			<email/>
		</kunde>
	</kundenliste>
|}.

'Should work #3':
{|xml||
	<?xml version="1.0" encoding="UTF-8"?>
	<kundenliste>
	</kundenliste>
|}.
