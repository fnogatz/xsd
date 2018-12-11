'xs:token with single spaces':
{|xml||
	<token>This is a sentence.</token>
|}.

'xs:token with empty value':
{|xml||
	<token></token>
|}.

% the following test cases would theoretically fail, but the whitespaces get replaced while parsing

'xs:token with leading space':
{|xml||
	<token> This is a string!</token>
|}.

'xs:token with trailing space':
{|xml||
	<token>This is a string! </token>
|}.

'xs:token with multiple spaces in-between':
{|xml||
	<token> This is  a string!</token>
|}.