'Valid sequence #1':
{|xml||
	<kdliste>
		<kunde>
			<kdnr>123</kdnr>
			<anrede>Herr</anrede>
			<vorname>Peter</vorname>
			<nachname>Schmidt</nachname>
			<notiz>Isst gerne Lakritz</notiz>
			<stammkunde>true</stammkunde>
		</kunde>
		<kunde>
			<kdnr>124</kdnr>
			<vorname>Ursula</vorname>
			<nachname>Schmied</nachname>
		</kunde>
	</kdliste>
|}.

'Valid sequence #2':
{|xml||
	<kdliste>
		<kunde>
			<kdnr>123</kdnr>
			<anrede>Herr</anrede>
			<vorname>Peter</vorname>
			<nachname>Schmidt</nachname>
			<notiz>Isst gerne Lakritz</notiz>
			<stammkunde>true</stammkunde>
		</kunde>
		<kunde>
			<kdnr>124</kdnr>
			<vorname>Ursula</vorname>
			<nachname>Schmied</nachname>
		</kunde>
		<kunde>
			<kdnr>125</kdnr>
			<vorname>Patrick</vorname>
			<nachname>Schmitt</nachname>
			<stammkunde>0</stammkunde>
		</kunde>
		<kunde>
			<kdnr>126</kdnr>
			<anrede>Herr</anrede>
			<vorname>Max</vorname>
			<nachname>Muster</nachname>
			<notiz>Notizenschreiber</notiz>
			<notiz>Wirklich oft und gerne</notiz>
			<notiz>Viele Hinweise nötig</notiz>
			<stammkunde>1</stammkunde>
		</kunde>
	</kdliste>
|}.

'Out of bounds (minOccurs)'(fail):
{|xml||
	<kdliste>
		<kunde>
			<kdnr>123</kdnr>
			<anrede>Herr</anrede>
			<vorname>Peter</vorname>
			<nachname>Schmidt</nachname>
			<notiz>Isst gerne Lakritz</notiz>
			<stammkunde>true</stammkunde>
		</kunde>
	</kdliste>
|}.

'Out of bounds (maxOccurs)'(fail):
{|xml||
	<kdliste>
		<kunde>
			<kdnr>123</kdnr>
			<anrede>Herr</anrede>
			<vorname>Peter</vorname>
			<nachname>Schmidt</nachname>
			<notiz>Isst gerne Lakritz</notiz>
			<stammkunde>true</stammkunde>
		</kunde>
		<kunde>
			<kdnr>124</kdnr>
			<vorname>Ursula</vorname>
			<nachname>Schmied</nachname>
		</kunde>
		<kunde>
			<kdnr>125</kdnr>
			<vorname>Patrick</vorname>
			<nachname>Schmitt</nachname>
			<stammkunde>0</stammkunde>
		</kunde>
		<kunde>
			<kdnr>126</kdnr>
			<anrede>Herr</anrede>
			<vorname>Max</vorname>
			<nachname>Muster</nachname>
			<notiz>Notizenschreiber</notiz>
			<notiz>Wirklich oft und gerne</notiz>
			<notiz>Viele Hinweise nötig</notiz>
			<stammkunde>1</stammkunde>
		</kunde>
		<kunde>
			<kdnr>127</kdnr>
			<vorname>Hans</vorname>
			<nachname>Wurst</nachname>
			<notiz>Leider nicht.</notiz>
		</kunde>
	</kdliste>
|}.