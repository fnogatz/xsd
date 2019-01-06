'xs:language without regional part':
{|xml||
	<language>en</language>
|}.

'xs:language with regional part':
{|xml||
	<language>en-US</language>
|}.

'xs:language with IANA registered language':
{|xml||
	<language>i-navajo</language>
|}.

'xs:language with private, unregistered language':
{|xml||
	<language>x-Newspeak</language>
|}.

'xs:language with multiple parts':
{|xml||
	<language>any-value-with-short-parts</language>
|}.

'xs:language with too long part'(fail):
{|xml||
	<language>longerThan8</language>
|}.

'xs:language with empty value'(fail):
{|xml||
	<language></language>
|}.