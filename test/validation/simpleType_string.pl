'xs:string in English language':
{|xml||
	<string>This is a string!</string>
|}.

'xs:string in French language':
{|xml||
	<string>Édition française.</string>
|}.

'xs:string in Chinese language':
{|xml||
	<string>Wǒ de péngyǒu jiào Li.</string>
|}.

'xs:string with spaces':
{|xml||
	<string>   Separated by 3 spaces.</string>
|}.

'xs:string with line break':
{|xml||
	<string>This
	is on two lines.</string>
|}.

'xs:string with tabs':
{|xml||
	<string>This		is on two lines.</string>
|}.

'xs:string with numeric value':
{|xml||
	<string>12.5</string>
|}.

'xs:string with empty value (two tags)':
{|xml||
	<string></string>
|}.

'xs:string with empty value (single tag)':
{|xml||
	<string/>
|}.

'xs:string with escaped special character \'<\'':
{|xml||
	<string>3 &lt; 4</string>
|}.

'xs:string with escaped special character \'>\'':
{|xml||
	<string>4 &gt; 3</string>
|}.

'xs:string with escaped special character \'\"\'':
{|xml||
	<string>&quot;What?&quot;</string>
|}.

'xs:string with escaped special character \'\'\'':
{|xml||
	<string>&apos;What?&apos;</string>
|}.

/*
% These test cases should be executed, but we cannot differentiate between a decoded escaped character ("&lt;" -->) "<" 
% and a not decoded unescaped character "<" when the type validation takes place.
% 
% Prolog's built-in function 'load_structure', with which the xml file is loaded, does not seem to care for unescaped characters and simply passes them through.

'xs:string with not escaped special character \'<\''(fail):
{|xml||
	<string>3 < 4</string>
|}.

'xs:string with not escaped special character \'>\''(fail):
{|xml||
	<string>4 > 3</string>
|}.

'xs:string with not escaped special character \'\"\''(fail):
{|xml||
	<string>"What?"</string>
|}.

'xs:string with not escaped special character \'\'\''(fail):
{|xml||
	<string>'What?'</string>
|}.
*/