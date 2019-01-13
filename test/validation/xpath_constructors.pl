/* string */
'string with single quotes':
{|xml||
	<string_single_quotes></string_single_quotes>
|}.
'string without quotes'(fail):
{|xml||
	<string_without_quotes></string_without_quotes>
|}.

/* boolean */
'boolean (1)':
{|xml||
	<boolean_1></boolean_1>
|}.
'boolean (true)':
{|xml||
	<boolean_true></boolean_true>
|}.
'boolean (arbitrary)':
{|xml||
	<boolean_arbitrary></boolean_arbitrary>
|}.
'boolean (0)'(fail):
{|xml||
	<boolean_0></boolean_0>
|}.
'boolean (false)'(fail):
{|xml||
	<boolean_false></boolean_false>
|}.