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

/* float */
'float (common)':
{|xml||
	<float_common></float_common>
|}.
'float (negative zero)':
{|xml||
	<float_negative_zero></float_negative_zero>
|}.
'float (scientific - short)':
{|xml||
	<float_scientific_short></float_scientific_short>
|}.
'float (scientific - long)':
{|xml||
	<float_scientific_long></float_scientific_long>
|}.
'float (infinity)':
{|xml||
	<float_infinity></float_infinity>
|}.
'float (NaN)':
{|xml||
	<float_NaN></float_NaN>
|}.
'float (invalid)'(fail):
{|xml||
	<float_invalid></float_invalid>
|}.

/* double */
'double (common)':
{|xml||
	<double_common></double_common>
|}.
'double (negative zero)':
{|xml||
	<double_negative_zero></double_negative_zero>
|}.
'double (scientific - short)':
{|xml||
	<double_scientific_short></double_scientific_short>
|}.
'double (scientific - long)':
{|xml||
	<double_scientific_long></double_scientific_long>
|}.
'double (infinity)':
{|xml||
	<double_infinity></double_infinity>
|}.
'double (NaN)':
{|xml||
	<double_NaN></double_NaN>
|}.
'double (invalid)'(fail):
{|xml||
	<double_invalid></double_invalid>
|}.

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

/* dateTime */
'dateTime (BC, no TC)':
{|xml||
	<dateTime_BC_no_TC></dateTime_BC_no_TC>
|}.
'dateTime (BC, UTC TC)':
{|xml||
	<dateTime_BC_UTC_TC></dateTime_BC_UTC_TC>
|}.
'dateTime (BC, negative TC)':
{|xml||
	<dateTime_BC_negative_TC></dateTime_BC_negative_TC>
|}.
'dateTime (BC, positive TC)':
{|xml||
	<dateTime_BC_positive_TC></dateTime_BC_positive_TC>
|}.
'dateTime (AD, no TC)':
{|xml||
	<dateTime_AD_no_TC></dateTime_AD_no_TC>
|}.
'dateTime (AD, UTC TC)':
{|xml||
	<dateTime_AD_UTC_TC></dateTime_AD_UTC_TC>
|}.
'dateTime (AD, negative TC)':
{|xml||
	<dateTime_AD_negative_TC></dateTime_AD_negative_TC>
|}.
'dateTime (AD, positive TC)':
{|xml||
	<dateTime_AD_positive_TC></dateTime_AD_positive_TC>
|}.
'dateTime (merge, no TC)':
{|xml||
	<dateTime_merge_no_TC></dateTime_merge_no_TC>
|}.
'dateTime (merge, same TC)':
{|xml||
	<dateTime_merge_same_TC></dateTime_merge_same_TC>
|}.
'dateTime (merge, only date has TC)':
{|xml||
	<dateTime_merge_date_TC></dateTime_merge_date_TC>
|}.
'dateTime (merge, only time has TC)':
{|xml||
	<dateTime_merge_time_TC></dateTime_merge_time_TC>
|}.
'dateTime (merge, different TC)'(fail):
{|xml||
	<dateTime_merge_different_TC></dateTime_merge_different_TC>
|}.

/* time */
'time (no TC)':
{|xml||
	<time_no_TC></time_no_TC>
|}.
'time (UTC TC)':
{|xml||
	<time_UTC_TC></time_UTC_TC>
|}.
'time (negative TC)':
{|xml||
	<time_negative_TC></time_negative_TC>
|}.
'time (positive TC)':
{|xml||
	<time_positive_TC></time_positive_TC>
|}.

/* date */
'date (BC, no TC)':
{|xml||
	<date_BC_no_TC></date_BC_no_TC>
|}.
'date (BC, UTC TC)':
{|xml||
	<date_BC_UTC_TC></date_BC_UTC_TC>
|}.
'date (BC, negative TC)':
{|xml||
	<date_BC_negative_TC></date_BC_negative_TC>
|}.
'date (BC, positive TC)':
{|xml||
	<date_BC_positive_TC></date_BC_positive_TC>
|}.
'date (AD, no TC)':
{|xml||
	<date_AD_no_TC></date_AD_no_TC>
|}.
'date (AD, UTC TC)':
{|xml||
	<date_AD_UTC_TC></date_AD_UTC_TC>
|}.
'date (AD, negative TC)':
{|xml||
	<date_AD_negative_TC></date_AD_negative_TC>
|}.
'date (AD, positive TC)':
{|xml||
	<date_AD_positive_TC></date_AD_positive_TC>
|}.