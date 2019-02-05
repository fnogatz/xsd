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

/* gYearMonth */
'gYearMonth (BC, no TC)':
{|xml||
	<gYearMonth_BC_no_TC></gYearMonth_BC_no_TC>
|}.
'gYearMonth (BC, UTC TC)':
{|xml||
	<gYearMonth_BC_UTC_TC></gYearMonth_BC_UTC_TC>
|}.
'gYearMonth (BC, negative TC)':
{|xml||
	<gYearMonth_BC_negative_TC></gYearMonth_BC_negative_TC>
|}.
'gYearMonth (BC, positive TC)':
{|xml||
	<gYearMonth_BC_positive_TC></gYearMonth_BC_positive_TC>
|}.
'gYearMonth (AD, no TC)':
{|xml||
	<gYearMonth_AD_no_TC></gYearMonth_AD_no_TC>
|}.
'gYearMonth (AD, UTC TC)':
{|xml||
	<gYearMonth_AD_UTC_TC></gYearMonth_AD_UTC_TC>
|}.
'gYearMonth (AD, negative TC)':
{|xml||
	<gYearMonth_AD_negative_TC></gYearMonth_AD_negative_TC>
|}.
'gYearMonth (AD, positive TC)':
{|xml||
	<gYearMonth_AD_positive_TC></gYearMonth_AD_positive_TC>
|}.

/* gYear */
'gYear (BC, no TC)':
{|xml||
	<gYear_BC_no_TC></gYear_BC_no_TC>
|}.
'gYear (BC, UTC TC)':
{|xml||
	<gYear_BC_UTC_TC></gYear_BC_UTC_TC>
|}.
'gYear (BC, negative TC)':
{|xml||
	<gYear_BC_negative_TC></gYear_BC_negative_TC>
|}.
'gYear (BC, positive TC)':
{|xml||
	<gYear_BC_positive_TC></gYear_BC_positive_TC>
|}.
'gYear (AD, no TC)':
{|xml||
	<gYear_AD_no_TC></gYear_AD_no_TC>
|}.
'gYear (AD, UTC TC)':
{|xml||
	<gYear_AD_UTC_TC></gYear_AD_UTC_TC>
|}.
'gYear (AD, negative TC)':
{|xml||
	<gYear_AD_negative_TC></gYear_AD_negative_TC>
|}.
'gYear (AD, positive TC)':
{|xml||
	<gYear_AD_positive_TC></gYear_AD_positive_TC>
|}.

/* gMonthDay */
'gMonthDay (no TC)':
{|xml||
	<gMonthDay_no_TC></gMonthDay_no_TC>
|}.
'gMonthDay (UTC TC)':
{|xml||
	<gMonthDay_UTC_TC></gMonthDay_UTC_TC>
|}.
'gMonthDay (negative TC)':
{|xml||
	<gMonthDay_negative_TC></gMonthDay_negative_TC>
|}.
'gMonthDay (positive TC)':
{|xml||
	<gMonthDay_positive_TC></gMonthDay_positive_TC>
|}.

/* gDay */
'gDay (no TC)':
{|xml||
	<gDay_no_TC></gDay_no_TC>
|}.
'gDay (UTC TC)':
{|xml||
	<gDay_UTC_TC></gDay_UTC_TC>
|}.
'gDay (negative TC)':
{|xml||
	<gDay_negative_TC></gDay_negative_TC>
|}.
'gDay (positive TC)':
{|xml||
	<gDay_positive_TC></gDay_positive_TC>
|}.

/* gMonth */
'gMonth (no TC)':
{|xml||
	<gMonth_no_TC></gMonth_no_TC>
|}.
'gMonth (UTC TC)':
{|xml||
	<gMonth_UTC_TC></gMonth_UTC_TC>
|}.
'gMonth (negative TC)':
{|xml||
	<gMonth_negative_TC></gMonth_negative_TC>
|}.
'gMonth (positive TC)':
{|xml||
	<gMonth_positive_TC></gMonth_positive_TC>
|}.

/* hexBinary */
'hexBinary (uppercase)':
{|xml||
	<hexBinary_uc></hexBinary_uc>
|}.
'hexBinary (lowercase)':
{|xml||
	<hexBinary_lc></hexBinary_lc>
|}.
'hexBinary (empty)':
{|xml||
	<hexBinary_empty></hexBinary_empty>
|}.

/* base64Binary */
'base64Binary (uppercase and whitespaces)':
{|xml||
	<base64Binary_uc_ws></base64Binary_uc_ws>
|}.
'base64Binary (lowercase and padding)':
{|xml||
	<base64Binary_lc_pad></base64Binary_lc_pad>
|}.
'base64Binary (empty)':
{|xml||
	<base64Binary_empty></base64Binary_empty>
|}.

/* anyURI */
'anyURI':
{|xml||
	<anyURI></anyURI>
|}.

/* QName */

/* normalizedString */
'normalizedString (multiple spaces)':
{|xml||
	<normalizedString_spaces></normalizedString_spaces>
|}.
'normalizedString (line break)':
{|xml||
	<normalizedString_linebreak></normalizedString_linebreak>
|}.

/* token */
'token':
{|xml||
	<token></token>
|}.

/* language */
'language':
{|xml||
	<language></language>
|}.

/* integer */
'integer (leading zeroes)':
{|xml||
	<integer_leading_zeroes></integer_leading_zeroes>
|}.
'integer (positive sign)':
{|xml||
	<integer_positive></integer_positive>
|}.
'integer (negative sign)':
{|xml||
	<integer_negative></integer_negative>
|}.

/* nonPositiveInteger */
'nonPositiveInteger':
{|xml||
	<nonPositiveInteger></nonPositiveInteger>
|}.

/* negativeInteger */
'negativeInteger':
{|xml||
	<negativeInteger></negativeInteger>
|}.

/* long */
'long':
{|xml||
	<long></long>
|}.

/* int */
'int':
{|xml||
	<int></int>
|}.

/* short */
'short':
{|xml||
	<short></short>
|}.

/* byte */
'byte':
{|xml||
	<byte></byte>
|}.

/* nonNegativeInteger */
'nonNegativeInteger':
{|xml||
	<nonNegativeInteger></nonNegativeInteger>
|}.

/* unsignedLong */
'unsignedLong':
{|xml||
	<unsignedLong></unsignedLong>
|}.

/* unsignedInt */
'unsignedInt':
{|xml||
	<unsignedInt></unsignedInt>
|}.

/* unsignedShort */
'unsignedShort':
{|xml||
	<unsignedShort></unsignedShort>
|}.

/* unsignedByte */
'unsignedByte':
{|xml||
	<unsignedByte></unsignedByte>
|}.

/* positiveInteger */
'positiveInteger':
{|xml||
	<positiveInteger></positiveInteger>
|}.

/* positiveInteger */
/* positiveInteger */

/* untypedAtomic */
'untypedAtomic':
{|xml||
	<untypedAtomic></untypedAtomic>
|}.