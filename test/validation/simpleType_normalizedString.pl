'xs:normalizedString with spaces':
{|xml||
	<normalizedString>   Separated by 3 spaces.</normalizedString>
|}.

'xs:normalizedString with empty value':
{|xml||
	<normalizedString></normalizedString>
|}.

% the following test cases would theoretically fail, but the whitespaces get replaced while parsing

'xs:normalizedString with line break':
{|xml||
	<normalizedString>This
	is on two lines.</normalizedString>
|}.

'xs:normalizedString with tab':
{|xml||
	<normalizedString>This		is on two lines.</normalizedString>
|}.