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

/* decimal */
'decimal (positive with long notation)':
{|xml||
	<decimal_long_positive></decimal_long_positive>
|}.
'decimal (negative with short notation)':
{|xml||
	<decimal_short_negative></decimal_short_negative>
|}.
'decimal (invalid)'(fail):
{|xml||
	<decimal_invalid></decimal_invalid>
|}.

/* float, double */

/* duration */
'duration (signless, full)':
{|xml||
	<duration_full></duration_full>
|}.
'duration (positive, months only)':
{|xml||
	<duration_months_only></duration_months_only>
|}.
'duration (negative, minutes and seconds only)':
{|xml||
	<duration_minutes_seconds_only></duration_minutes_seconds_only>
|}.
'duration (neutral)':
{|xml||
	<duration_neutral></duration_neutral>
|}.
'duration (unnormalized)':
{|xml||
	<duration_unnormalized></duration_unnormalized>
|}.