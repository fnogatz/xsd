:- module(xpath, 
	[
		assert/3,
		assertion/4
	]).

:- use_module(library(regex)).
:- use_module(library(xsd/simpletype)).
:- use_module(library(xsd/xsd_messages)).

:- op(1, fx, user:($)).
:- op(700, xfx, user:(eq)).

:- set_prolog_flag(allow_variable_name_as_functor, true).

assert(D_File, D_ID, XPathExpr) :-
	warning('Testing for ~w with File ~w and ID ~w!', [XPathExpr, D_File, D_ID]).
assertion(D_File, D_ID, D_Text, XPathString) :-
	nb_setval(context_file, D_File),
	nb_setval(context_id, D_ID),
	nb_setval(context_value, D_Text),

	term_string(XPathExpr, XPathString),
	!,
	xpath_expr(XPathExpr, Result),
	(
		\+compound(Result);
		(
			=(Result, data(_, Value)),
			\=(Value, [false])
		)
	).
	% warning('Result: ~w', [Result]).


/* ### Special Cases ### */

/* --- atomic values are valid xpath expressions --- */
xpath_expr(Result, Result) :-
	% simple check for now - needs to be replaced by constructors later on
	\+compound(Result).


/* ### Special Functions ### */

/* --- $value --- */
xpath_expr($value, Result) :-
	nb_current(context_value, Value),
	term_string(Result, Value).


/* ### Operators ### */

/* --- eq --- */
xpath_expr(Value1 eq Value2, data('boolean', [ResultValue])) :-
	xpath_expr(Value1, Result1),
	xpath_expr(Value2, Result2),
	
	% just a simple numeric comparison for now (needs to be replaced)
	Result1 =:= Result2 ->
		ResultValue = true;
		ResultValue = false.


/* ### Functions ### */

/* ~~~ Constructors ~~~ */

/* --- string --- */
xpath_expr(string(Value), data('string', [ResultValue])) :-
	validate_xsd_simpleType('string', Value),
	term_string(Value, ResultValue).
/* --- boolean --- */
xpath_expr(boolean(Value), data('boolean', [ResultValue])) :-
	member(Value, ['false', '0']) ->
		ResultValue = false;
		ResultValue = true.
/* --- decimal --- */
xpath_expr(decimal(Value), data('decimal', [ResultValue])) :-
	validate_xsd_simpleType('decimal', Value),
	( % add leading 0 in front of decimal point, as prolog cannot handle decimals like ".32"
		Value =~ '^(\\+|-)?\\..*$' ->
		(
			atomic_list_concat(TMP, '.', Value),
			atomic_list_concat(TMP, '0.', ProcValue)	
		);
		ProcValue = Value
	),
	term_string(ResultValue, ProcValue).
/* --- float --- */
xpath_expr(float(Value), data('float', [ResultValue])) :-
	validate_xsd_simpleType('float', Value),
	parse_float(Value, ResultValue).
/* --- double --- */
xpath_expr(double(Value), data('double', [ResultValue])) :-
	validate_xsd_simpleType('double', Value),
	% double values are internally handled as float values
	parse_float(Value, ResultValue).
/* --- duration --- */
xpath_expr(duration(Value), Result) :-
	validate_xsd_simpleType('duration', Value),
	split_string(Value, 'P', '', PSplit),
	(
		PSplit = [Sign, PSplitR], Sign = '-';
		PSplit = [_, PSplitR], Sign = '+'
	),
	split_string(PSplitR, 'Y', '', YSplit),
	(
		YSplit = [YSplitR], Years = 0;
		YSplit = [YearsTMP, YSplitR], number_string(Years, YearsTMP)
	),
	split_string(YSplitR, 'M', '', MoSplit),
	(
		MoSplit = [MoSplitR], Months = 0;
		MoSplit = [MonthsTMP, MoSplitR], \+sub_string(MonthsTMP, _, _, _, 'T'), number_string(Months, MonthsTMP);
		MoSplit = [MoSplitR0, MoSplitR1], sub_string(MoSplitR0, _, _, _, 'T'), string_concat(MoSplitR0, 'M', TMP), string_concat(TMP, MoSplitR1, MoSplitR), Months = 0;
		MoSplit = [MonthsTMP, MoSplitR0, MoSplitR1], string_concat(MoSplitR0, 'M', TMP), string_concat(TMP, MoSplitR1, MoSplitR), number_string(Months, MonthsTMP)
	),
	split_string(MoSplitR, 'D', '', DSplit),
	(
		DSplit = [DSplitR], Days = 0;
		DSplit = [DaysTMP, DSplitR], number_string(Days, DaysTMP)
	),
	split_string(DSplitR, 'T', '', TSplit),
	(
		TSplit = [TSplitR];
		TSplit = [_, TSplitR]	
	),
	split_string(TSplitR, 'H', '', HSplit),
	(
		HSplit = [HSplitR], Hours = 0;
		HSplit = [HoursTMP, HSplitR], number_string(Hours, HoursTMP)	
	),
	split_string(HSplitR, 'M', '', MiSplit),
	(
		MiSplit = [MiSplitR], Minutes = 0;
		MiSplit = [MinutesTMP, MiSplitR], number_string(Minutes, MinutesTMP)
	),
	split_string(MiSplitR, 'S', '', SSplit),
	(
		SSplit = [_], Seconds = 0;
		SSplit = [SecondsTMP, _], number_string(Seconds, SecondsTMP)
	),
	normalize_duration(
		data('duration', [Sign, Years, Months, Days, Hours, Minutes, Seconds]),
		Result
	).
/* --- dateTime --- */
xpath_expr(dateTime(Value), data('dateTime', [Year, Month, Day, Hour, Minute, Second, TimeZoneOffset])) :-
	validate_xsd_simpleType('dateTime', Value),
	split_string(Value, 'T', '', TSplit),
	TSplit = [Date, Time],
	xpath_expr(date(Date), data('date', [Year, Month, Day, _, _, _, _])),
	xpath_expr(time(Time), data('time', [_, _, _, Hour, Minute, Second, TimeZoneOffset])).
xpath_expr(dateTime(Date,Time), data('dateTime', [Year, Month, Day, Hour, Minute, Second, TimeZoneOffset])) :-
	validate_xsd_simpleType('date', Date),
	validate_xsd_simpleType('time', Time),
	xpath_expr(date(Date), data('date', [Year, Month, Day, _, _, _, TimeZoneOffsetDate])),
	xpath_expr(time(Time), data('time', [_, _, _, Hour, Minute, Second, TimeZoneOffsetTime])),
	(
		% both date and time have the same or no TC
		TimeZoneOffsetDate = TimeZoneOffsetTime, TimeZoneOffset = TimeZoneOffsetDate;
		% only date has TC
		TimeZoneOffsetDate \= 0, TimeZoneOffsetTime = 0, TimeZoneOffset = TimeZoneOffsetDate;
		% only time has TC
		TimeZoneOffsetDate = 0, TimeZoneOffsetTime \= 0, TimeZoneOffset = TimeZoneOffsetTime
	).
/* --- time --- */
xpath_expr(time(Value), data('time', [0, 0, 0, Hour, Minute, Second, TimeZoneOffset])) :-
	validate_xsd_simpleType('time', Value),
	(
		% negative TC
		split_string(Value, '-', '', MinusSplit),
		MinusSplit = [TimeTMP, TimeZoneTMP],
		TimeZoneSign = '-'
		;
		% positive TC
		split_string(Value, '+', '', PlusSplit),
		PlusSplit = [TimeTMP, TimeZoneTMP],
		TimeZoneSign = '+'
		;
		% UTC TC
		split_string(Value, 'Z', '', ZSplit),
		ZSplit = [TimeTMP, _],
		TimeZoneSign = '+',
		TimeZoneTMP = '00:00'
		;
		% no TC
		split_string(Value, 'Z+-', '', AllSplit),
		AllSplit = [TimeTMP],
		TimeZoneSign = '+',
		TimeZoneTMP = '00:00'
	),
	split_string(TimeTMP, ':', '', TimeSplit),
	TimeSplit = [HourTMP, MinuteTMP, SecondTMP],
	split_string(TimeZoneTMP, ':', '', TimeZoneSplit),
	TimeZoneSplit = [TimeZoneHourTMP, TimeZoneMinuteTMP],
	number_string(Hour, HourTMP),
	number_string(Minute, MinuteTMP),
	number_string(Second, SecondTMP),
	number_string(TimeZoneHour, TimeZoneHourTMP),
	number_string(TimeZoneMinute, TimeZoneMinuteTMP),
	timezone_offset(TimeZoneSign, TimeZoneHour, TimeZoneMinute, TimeZoneOffset).
/* --- date --- */
xpath_expr(date(Value), data('date', [Year, Month, Day, 0, 0, 0, TimeZoneOffset])) :-
	validate_xsd_simpleType('date', Value),
	split_string(Value, '-', '', MinusSplit),
	(
		% BC, negative TZ
		MinusSplit = [_, YearString, MonthTMP, DayTMP, TimeZoneTMP], string_concat('-', YearString, YearTMP), TimeZoneSign = '-';
		% BC, UTC, positive, no TZ
		MinusSplit = [_, YearString, MonthTMP, DayTimeZoneTMP], string_concat('-', YearString, YearTMP), TimeZoneSign = '+',
		timezone_split(DayTMP, TimeZoneTMP, DayTimeZoneTMP);
		% AD, negative TZ
		MinusSplit = [YearTMP, MonthTMP, DayTMP, TimeZoneTMP], TimeZoneSign = '-';
		% AD, UTC, positive, no TZ
		MinusSplit = [YearTMP, MonthTMP, DayTimeZoneTMP], TimeZoneSign = '+',
		timezone_split(DayTMP, TimeZoneTMP, DayTimeZoneTMP)
	),
	split_string(TimeZoneTMP, ':', '', ColonSplit),
	ColonSplit = [TimeZoneHourTMP, TimeZoneMinuteTMP],
	number_string(Year, YearTMP),
	number_string(Month, MonthTMP),
	number_string(Day, DayTMP),
	number_string(TimeZoneHour, TimeZoneHourTMP),
	number_string(TimeZoneMinute, TimeZoneMinuteTMP),
	timezone_offset(TimeZoneSign, TimeZoneHour, TimeZoneMinute, TimeZoneOffset).
/* --- gYearMonth --- */
xpath_expr(gYearMonth(Value), data('gYearMonth', [Year, Month, 0, 0, 0, 0, TimeZoneOffset])) :-
	validate_xsd_simpleType('gYearMonth', Value),
	split_string(Value, '-', '', MinusSplit),
	(
		% BC, negative TZ
		MinusSplit = [_, YearString, MonthTMP, TimeZoneTMP], string_concat('-', YearString, YearTMP), TimeZoneSign = '-';
		% BC, UTC, positive, no TZ
		MinusSplit = [_, YearString, MonthTimeZoneTMP], string_concat('-', YearString, YearTMP), TimeZoneSign = '+',
		timezone_split(MonthTMP, TimeZoneTMP, MonthTimeZoneTMP);
		% AD, negative TZ
		MinusSplit = [YearTMP, MonthTMP, TimeZoneTMP], TimeZoneSign = '-';
		% AD, UTC, positive, no TZ
		MinusSplit = [YearTMP, MonthTimeZoneTMP], TimeZoneSign = '+',
		timezone_split(MonthTMP, TimeZoneTMP, MonthTimeZoneTMP)
	),
	split_string(TimeZoneTMP, ':', '', ColonSplit),
	ColonSplit = [TimeZoneHourTMP, TimeZoneMinuteTMP],
	number_string(Year, YearTMP),
	number_string(Month, MonthTMP),
	number_string(TimeZoneHour, TimeZoneHourTMP),
	number_string(TimeZoneMinute, TimeZoneMinuteTMP),
	timezone_offset(TimeZoneSign, TimeZoneHour, TimeZoneMinute, TimeZoneOffset).
/* --- gYear --- */
xpath_expr(gYear(Value), data('gYear', [Year, 0, 0, 0, 0, 0, TimeZoneOffset])) :-
	validate_xsd_simpleType('gYear', Value),
	split_string(Value, '-', '', MinusSplit),
	(
		% BC, negative TZ
		MinusSplit = [_, YearString, TimeZoneTMP], string_concat('-', YearString, YearTMP), TimeZoneSign = '-';
		% BC, UTC, positive, no TZ
		MinusSplit = [_, YearTimeZoneTMP], TimeZoneSign = '+',
		timezone_split(YearString, TimeZoneTMP, YearTimeZoneTMP), string_concat('-', YearString, YearTMP);
		% AD, negative TZ
		MinusSplit = [YearTMP, TimeZoneTMP], TimeZoneSign = '-';
		% AD, UTC, positive, no TZ
		MinusSplit = [YearTimeZoneTMP], TimeZoneSign = '+',
		timezone_split(YearTMP, TimeZoneTMP, YearTimeZoneTMP)
	),
	split_string(TimeZoneTMP, ':', '', ColonSplit),
	ColonSplit = [TimeZoneHourTMP, TimeZoneMinuteTMP],
	number_string(Year, YearTMP),
	number_string(TimeZoneHour, TimeZoneHourTMP),
	number_string(TimeZoneMinute, TimeZoneMinuteTMP),
	timezone_offset(TimeZoneSign, TimeZoneHour, TimeZoneMinute, TimeZoneOffset).
/* --- gMonthDay --- */
xpath_expr(gMonthDay(Value), data('gMonthDay', [0, Month, Day, 0, 0, 0, TimeZoneOffset])) :-
	validate_xsd_simpleType('gMonthDay', Value),
	split_string(Value, '-', '', MinusSplit),
	(
		% negative TZ
		MinusSplit = [_, _, MonthTMP, DayTMP, TimeZoneTMP], TimeZoneSign = '-';
		% UTC, positive, no TZ
		MinusSplit = [_, _, MonthTMP, DayTimeZoneTMP], TimeZoneSign = '+',
		timezone_split(DayTMP, TimeZoneTMP, DayTimeZoneTMP)
	),
	split_string(TimeZoneTMP, ':', '', ColonSplit),
	ColonSplit = [TimeZoneHourTMP, TimeZoneMinuteTMP],
	number_string(Month, MonthTMP),
	number_string(Day, DayTMP),
	number_string(TimeZoneHour, TimeZoneHourTMP),
	number_string(TimeZoneMinute, TimeZoneMinuteTMP),
	timezone_offset(TimeZoneSign, TimeZoneHour, TimeZoneMinute, TimeZoneOffset).
/* --- gDay --- */
xpath_expr(gDay(Value), data('gDay', [0, 0, Day, 0, 0, 0, TimeZoneOffset])) :-
	validate_xsd_simpleType('gDay', Value),
	split_string(Value, '-', '', MinusSplit),
	(
		% negative TZ
		MinusSplit = [_, _, _, DayTMP, TimeZoneTMP], TimeZoneSign = '-';
		% UTC, positive, no TZ
		MinusSplit = [_, _, _, DayTimeZoneTMP], TimeZoneSign = '+',
		timezone_split(DayTMP, TimeZoneTMP, DayTimeZoneTMP)
	),
	split_string(TimeZoneTMP, ':', '', ColonSplit),
	ColonSplit = [TimeZoneHourTMP, TimeZoneMinuteTMP],
	number_string(Day, DayTMP),
	number_string(TimeZoneHour, TimeZoneHourTMP),
	number_string(TimeZoneMinute, TimeZoneMinuteTMP),
	timezone_offset(TimeZoneSign, TimeZoneHour, TimeZoneMinute, TimeZoneOffset).
/* --- gMonth --- */
xpath_expr(gMonth(Value), data('gMonth', [0, Month, 0, 0, 0, 0, TimeZoneOffset])) :-
	validate_xsd_simpleType('gMonth', Value),
	split_string(Value, '-', '', MinusSplit),
	(
		% negative TZ
		MinusSplit = [_, _, MonthTMP, TimeZoneTMP], TimeZoneSign = '-';
		% UTC, positive, no TZ
		MinusSplit = [_, _, MonthTimeZoneTMP], TimeZoneSign = '+',
		timezone_split(MonthTMP, TimeZoneTMP, MonthTimeZoneTMP)
	),
	split_string(TimeZoneTMP, ':', '', ColonSplit),
	ColonSplit = [TimeZoneHourTMP, TimeZoneMinuteTMP],
	number_string(Month, MonthTMP),
	number_string(TimeZoneHour, TimeZoneHourTMP),
	number_string(TimeZoneMinute, TimeZoneMinuteTMP),
	timezone_offset(TimeZoneSign, TimeZoneHour, TimeZoneMinute, TimeZoneOffset).
/* --- hexBinary --- */
xpath_expr(hexBinary(Value), data('hexBinary', [LowerCaseValue])) :-
	validate_xsd_simpleType('hexBinary', Value),
	atom_string(Value, ValueString),
	string_upper(ValueString, LowerCaseValue).
/* --- base64Binary --- */
xpath_expr(base64Binary(Value), data('base64Binary', [ValueString])) :-
	validate_xsd_simpleType('base64Binary', Value),
	string_upper(Value, LowerCaseValue),
	atomic_list_concat(TMP, ' ', LowerCaseValue),
	atomic_list_concat(TMP, '', SanitizedValue),
	atom_string(SanitizedValue, ValueString).
/* --- anyURI --- */
xpath_expr(anyURI(Value), data('anyURI', [ValueString])) :-
	validate_xsd_simpleType('anyURI', Value),
	atom_string(Value, ValueString).
/* --- QName --- */
xpath_expr(QName(Value), data('QName', [ValueString])) :-
	validate_xsd_simpleType('QName', Value),
	atom_string(Value, ValueString).
/* --- normalizedString --- */
xpath_expr(normalizedString(Value), data('normalizedString', [ValueString])) :-
	validate_xsd_simpleType('normalizedString', Value),
	atom_string(Value, ValueString).
/* --- token --- */
xpath_expr(token(Value), data('token', [ValueString])) :-
	validate_xsd_simpleType('token', Value),
	atom_string(Value, ValueString).
/* --- language --- */
xpath_expr(language(Value), data('language', [ValueString])) :-
	validate_xsd_simpleType('language', Value),
	atom_string(Value, ValueString).
/* --- NMTOKEN --- */
xpath_expr(NMTOKEN(Value), data('NMTOKEN', [NormalizedValueString])) :-
	atom_string(Value, ValueString),
	normalize_space(atom(NormalizedValue), ValueString),
	atom_string(NormalizedValue, NormalizedValueString),
	validate_xsd_simpleType('NMTOKEN', NormalizedValueString).
/* --- NCName --- */
xpath_expr(NCName(Value), data('NCName', [ValueString])) :-
	validate_xsd_simpleType('NCName', Value),
	atom_string(Value, ValueString).
/* --- Name --- */
xpath_expr(Name(Value), data('Name', [ValueString])) :-
	validate_xsd_simpleType('Name', Value),
	atom_string(Value, ValueString).
/* --- ID --- */
xpath_expr(ID(Value), data('ID', [ValueString])) :-
	validate_xsd_simpleType('ID', Value),
	atom_string(Value, ValueString).
/* --- IDREF --- */
xpath_expr(IDREF(Value), data('IDREF', [ValueString])) :-
	validate_xsd_simpleType('IDREF', Value),
	atom_string(Value, ValueString).
/* --- ENTITY --- */
xpath_expr(ENTITY(Value), data('ENTITY', [ValueString])) :-
	validate_xsd_simpleType('ENTITY', Value),
	atom_string(Value, ValueString).
/* --- integer --- */
xpath_expr(integer(Value), data('integer', [NumberValue])) :-
	validate_xsd_simpleType('integer', Value),
	atom_string(Value, ValueString),
	number_string(NumberValue, ValueString).
/* --- nonPositiveInteger --- */
xpath_expr(nonPositiveInteger(Value), data('nonPositiveInteger', [NumberValue])) :-
	validate_xsd_simpleType('nonPositiveInteger', Value),
	atom_string(Value, ValueString),
	number_string(NumberValue, ValueString).
/* --- negativeInteger --- */
xpath_expr(negativeInteger(Value), data('negativeInteger', [NumberValue])) :-
	validate_xsd_simpleType('negativeInteger', Value),
	atom_string(Value, ValueString),
	number_string(NumberValue, ValueString).
/* --- long --- */
xpath_expr(long(Value), data('long', [NumberValue])) :-
	validate_xsd_simpleType('long', Value),
	atom_string(Value, ValueString),
	number_string(NumberValue, ValueString).
/* --- int --- */
xpath_expr(int(Value), data('int', [NumberValue])) :-
	validate_xsd_simpleType('int', Value),
	atom_string(Value, ValueString),
	number_string(NumberValue, ValueString).
/* --- short --- */
xpath_expr(short(Value), data('short', [NumberValue])) :-
	validate_xsd_simpleType('short', Value),
	atom_string(Value, ValueString),
	number_string(NumberValue, ValueString).
/* --- byte --- */
xpath_expr(byte(Value), data('byte', [NumberValue])) :-
	validate_xsd_simpleType('byte', Value),
	atom_string(Value, ValueString),
	number_string(NumberValue, ValueString).
/* --- nonNegativeInteger --- */
xpath_expr(nonNegativeInteger(Value), data('nonNegativeInteger', [NumberValue])) :-
	validate_xsd_simpleType('nonNegativeInteger', Value),
	atom_string(Value, ValueString),
	number_string(NumberValue, ValueString).
/* --- unsignedLong --- */
xpath_expr(unsignedLong(Value), data('unsignedLong', [NumberValue])) :-
	validate_xsd_simpleType('unsignedLong', Value),
	atom_string(Value, ValueString),
	number_string(NumberValue, ValueString).
/* --- unsignedInt --- */
xpath_expr(unsignedInt(Value), data('unsignedInt', [NumberValue])) :-
	validate_xsd_simpleType('unsignedInt', Value),
	atom_string(Value, ValueString),
	number_string(NumberValue, ValueString).
/* --- unsignedShort --- */
xpath_expr(unsignedShort(Value), data('unsignedShort', [NumberValue])) :-
	validate_xsd_simpleType('unsignedShort', Value),
	atom_string(Value, ValueString),
	number_string(NumberValue, ValueString).
/* --- unsignedByte --- */
xpath_expr(unsignedByte(Value), data('unsignedByte', [NumberValue])) :-
	validate_xsd_simpleType('unsignedByte', Value),
	atom_string(Value, ValueString),
	number_string(NumberValue, ValueString).
/* --- positiveInteger --- */
xpath_expr(positiveInteger(Value), data('positiveInteger', [NumberValue])) :-
	validate_xsd_simpleType('positiveInteger', Value),
	atom_string(Value, ValueString),
	number_string(NumberValue, ValueString).
/* --- yearMonthDuration --- */
/* --- dayTimeDuration --- */
/* --- untypedAtomic --- */
xpath_expr(untypedAtomic(Value), data('untypedAtomic', [ValueString])) :-
	validate_xsd_simpleType('untypedAtomic', Value),
	atom_string(Value, ValueString).


/* ~~~ Parsing ~~~ */

parse_float(Value, nan) :-
	Value =~ '^\\+?NaN$'.
parse_float(Value, -nan) :-
	Value =~ '^-NaN$'.
parse_float(Value, inf) :-
	Value =~ '^\\+?INF$'.
parse_float(Value, -inf) :-
	Value =~ '^-INF$'.
parse_float(Value, ResultValue) :-
	term_string(ValueTerm, Value), ResultValue is float(ValueTerm).


/* ~~~ Normalization ~~~ */

normalize_duration(
	data('duration', [USign, UYears, UMonths, UDays, UHours, UMinutes, USeconds]),
	data('duration', [NSign, NYears, NMonths, NDays, NHours, NMinutes, NSeconds])) :-
	% 0 =< Seconds < 60
	% seconds are (in constrast to the other values) given as float
	number_string(USeconds, SUSeconds),
	split_string(SUSeconds, '.', '', LUSeconds),
	(
		LUSeconds = [SIntegerSeconds], number_string(IntegerSeconds, SIntegerSeconds), NSeconds is IntegerSeconds mod 60;
		LUSeconds = [SIntegerSeconds,SFractionalSeconds], number_string(IntegerSeconds, SIntegerSeconds),
			TSeconds is IntegerSeconds mod 60, number_string(TSeconds, STSeconds),
			atomic_list_concat([STSeconds,SFractionalSeconds], '.', ANSeconds), atom_string(ANSeconds, SNSeconds), number_string(NSeconds, SNSeconds)
	),
	MinutesDiv is IntegerSeconds div 60,
	% 0 =< Minutes < 60
	MinutesTMP is UMinutes + MinutesDiv,
	HoursDiv is MinutesTMP div 60, NMinutes is MinutesTMP mod 60,
	% 0 =< Hours < 24
	HoursTMP is UHours + HoursDiv,
	DaysDiv is HoursTMP div 24, NHours is HoursTMP mod 24,
	% 0 =< Days < 31
	DaysTMP is UDays + DaysDiv,
	MonthsDiv is DaysTMP div 31, NDays is DaysTMP mod 31,
	% 0 =< Months < 12
	MonthsTMP is UMonths + MonthsDiv,
	YearsDiv is MonthsTMP div 12, NMonths is MonthsTMP mod 12,
	% Years have no restrictions
	YearsTMP is UYears + YearsDiv,
	(
		% negative year values lead to a flipping of the sign
		YearsTMP < 0 ->
			(
				USign = '+' ->
					NSign = '-';
					NSign = '+'	
			);
			(
				% durations of 0 are always positive
				UYears = 0, UMonths = 0, UDays = 0, UHours = 0, UMinutes = 0, USeconds = 0 ->
					NSign = '+';
					NSign = USign
			)
	),
	NYears is abs(YearsTMP).


/* ~~~ Helping Functions ~~~ */
/* --- convert time zone parts to compact time zone offset --- */
timezone_offset('-', TimeZoneHour, TimeZoneMinute, TimeZoneOffset) :-
	TimeZoneOffset is (-1 * (60 * TimeZoneHour + TimeZoneMinute)).
timezone_offset('+', TimeZoneHour, TimeZoneMinute, TimeZoneOffset) :-
	TimeZoneOffset is (60 * TimeZoneHour + TimeZoneMinute).
/* --- splits RestTimeZoneTMP into RestTMP (e.g. MonthTMP - '02') and TimeZoneTMP (e.g. '-13:30') --- */
timezone_split(RestTMP, TimeZoneTMP, RestTimeZoneTMP) :-
	% UTC TC
	split_string(RestTimeZoneTMP, 'Z', '', ZSplit), ZSplit = [RestTMP, _], TimeZoneTMP = '00:00'.
timezone_split(RestTMP, TimeZoneTMP, RestTimeZoneTMP) :-
	% positive TC
	split_string(RestTimeZoneTMP, '+', '', PlusSplit), PlusSplit = [RestTMP, TimeZoneTMP].
timezone_split(RestTMP, TimeZoneTMP, RestTimeZoneTMP) :- 
	% no TZ
	\+sub_string(RestTimeZoneTMP, _, _, _, 'Z'), \+sub_string(RestTimeZoneTMP, _, _, _, '+'), 
	RestTMP = RestTimeZoneTMP, TimeZoneTMP = '00:00'.