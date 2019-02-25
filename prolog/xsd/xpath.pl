:- module(xpath, 
	[
		assert/5
	]).

:- use_module(library(regex)).
:- use_module(library(xsd/flatten)).
:- use_module(library(xsd/simpletype)).
:- use_module(library(xsd/xsd_helper)).
:- use_module(library(xsd/xsd_messages)).

:- op(1, fx, user:($)).
:- op(200, fy, user:(@)).
:- op(400, yfx, user:(mod)).
:- op(500, yfx, user:(+)).
:- op(700, xfx, user:(eq)).

:- set_prolog_flag(allow_variable_name_as_functor, true).


% this is the exported predicate, which is used in validate.pl
assert(D_File, D_ID, D_Text, XPathString, Documentation) :-
	save_context(D_File, D_ID, D_Text, Documentation),
	validate_xpath(XPathString).

% saves the context, in which the xpath expression is evaluated
save_context(D_File, D_ID, D_Text, Documentation) :-
	nb_setval(context_file, D_File),
	nb_setval(context_id, D_ID),
	(
		D_Text = false
		;
		D_Text \= false, nb_setval(context_value, D_Text)	
	),
	nb_setval(context_documentation, Documentation).

% evaluate the xpath expression and check the result
validate_xpath(XPathString) :-
	term_string(XPathExpr, XPathString),
	!,
	(
		xpath_expr(XPathExpr, Result), Result = data(T, R), (T \= 'boolean'; R = [true]) ->
			true
			;
			nb_current(context_documentation, Documentation),
			(
				Documentation = null ->
					warning('An assert is not fulfilled.')
					;
					warning('The following assert is not fulfilled: ~w', Documentation)
			),
			false
	).


/* ### Special Cases ### */

/* --- atomic values --- */
% atomic values are converted to our internal data structure with a suitable constructor
xpath_expr(Value, Result) :-
	\+compound(Value),
	(
		number(Value) ->
			atom_number(ValueAtom, Value)
			;
			ValueAtom = Value
	),
	(
		member(ValueAtom, ['false', 'true']) ->
			xpath_expr(boolean(ValueAtom), Result);
			(
				xpath_expr(string(ValueAtom), Result);
				xpath_expr(decimal(ValueAtom), Result);
				xpath_expr(float(ValueAtom), Result);
				xpath_expr(double(ValueAtom), Result);
				xpath_expr(duration(ValueAtom), Result);
				xpath_expr(dateTime(ValueAtom), Result);
				xpath_expr(time(ValueAtom), Result);
				xpath_expr(date(ValueAtom), Result);
				xpath_expr(gYearMonth(ValueAtom), Result);
				xpath_expr(gYear(ValueAtom), Result);
				xpath_expr(gMonthDay(ValueAtom), Result);
				xpath_expr(gDay(ValueAtom), Result);
				xpath_expr(gMonth(ValueAtom), Result);
				xpath_expr(hexBinary(ValueAtom), Result);
				xpath_expr(base64Binary(ValueAtom), Result);
				xpath_expr(anyURI(ValueAtom), Result);
				xpath_expr(QName(ValueAtom), Result);
				xpath_expr(normalizedString(ValueAtom), Result);
				xpath_expr(token(ValueAtom), Result);
				xpath_expr(language(ValueAtom), Result);
				xpath_expr(NMTOKEN(ValueAtom), Result);
				xpath_expr(NCName(ValueAtom), Result);
				xpath_expr(Name(ValueAtom), Result);
				xpath_expr(ID(ValueAtom), Result);
				xpath_expr(IDREF(ValueAtom), Result);
				xpath_expr(ENTITY(ValueAtom), Result);
				xpath_expr(integer(ValueAtom), Result);
				xpath_expr(nonPositiveInteger(ValueAtom), Result);
				xpath_expr(negativeInteger(ValueAtom), Result);
				xpath_expr(long(ValueAtom), Result);
				xpath_expr(int(ValueAtom), Result);
				xpath_expr(short(ValueAtom), Result);
				xpath_expr(byte(ValueAtom), Result);
				xpath_expr(nonNegativeInteger(ValueAtom), Result);
				xpath_expr(unsignedLong(ValueAtom), Result);
				xpath_expr(unsignedInt(ValueAtom), Result);
				xpath_expr(unsignedShort(ValueAtom), Result);
				xpath_expr(unsignedByte(ValueAtom), Result);
				xpath_expr(positiveInteger(ValueAtom), Result);
				xpath_expr(yearMonthDuration(ValueAtom), Result);
				xpath_expr(dayTimeDuration(ValueAtom), Result);
				xpath_expr(untypedAtomic(ValueAtom), Result)
			)
	).

/* --- $value --- */
xpath_expr($value, Result) :-
	nb_current(context_value, Value),
	xpath_expr(Value, Result).


/* ### Location path expressions ### */

/* --- attributes --- */
xpath_expr(@Attribute, Result) :-
	nb_current(context_file, D_File),
	nb_current(context_id, D_ID),
	descendant_or_self(D_File, D_ID, D_Desc_ID),
	node_attribute(D_File, D_Desc_ID, Attribute, AttributeValue),
	xpath_expr(AttributeValue, Result).


/* ### Operators ### */

xpath_expr(Value1+Value2, Result) :-
	xpath_expr(numeric-add(Value1, Value2), Result).

/* --- numerics --- */
xpath_expr(numeric-add(Value1, Value2), data(Type, [ResultValue])) :-
	xpath_expr(Value1, data(Type, [InternalValue1])),
	xpath_expr(Value2, data(Type, [InternalValue2])),

	member(Type, ['decimal', 'double', 'float']),
	(
		% if one value is nan, return it
		(InternalValue1 = nan; InternalValue2 = nan), ResultValue = nan;
		% if one value is infinite, return it
		\+is_inf(InternalValue1), is_inf(InternalValue2), ResultValue = InternalValue2;
		is_inf(InternalValue1), \+is_inf(InternalValue2), ResultValue = InternalValue1;
		% if both values are infinite, return them if they are equal, otherwise return nan
		is_inf(InternalValue1), is_inf(InternalValue2), (InternalValue1 = InternalValue2 -> ResultValue = InternalValue1; ResultValue = nan);
		% if both values are finite, perform an arithmetic addition.
		\+is_inf(InternalValue1), \+is_inf(InternalValue2), ResultValue is InternalValue1 + InternalValue2
	).

/* --- eq --- */
xpath_expr(Value1 eq Value2, data('boolean', [ResultValue])) :-
	xpath_expr(Value1, Result1),
	xpath_expr(Value2, Result2),

	Result1 = data(Type1, ValueList1),
	Result2 = data(Type2, ValueList2),

	Type1 = Type2, ValueList1 = ValueList2 ->
		ResultValue = true;
		ResultValue = false.
/* --- mod --- */
xpath_expr(Value1 mod Value2, data(T, [ModuloValue])) :-
	xpath_expr(Value1, data(T, [EvaluatedValue1])),
	xpath_expr(Value2, data(T, [EvaluatedValue2])),

	member(T, [
		'decimal', 'integer', 'nonPositiveInteger', 'negativeInteger',
		'long', 'int', 'short', 'byte',
		'nonNegativeInteger', 'unsignedLong', 'unsingedInt', 'unsignedShort', 'unsignedByte', 'positiveInteger'
	]),
	
	ModuloValue is EvaluatedValue1 mod EvaluatedValue2.


/* ### Functions ### */

/* ~~~ Constructors ~~~ */
xpath_expr(data(T, VL), data(T, VL)).
/* --- string --- */
xpath_expr(string(Value), data('string', [Value])) :-
	validate_xsd_simpleType('string', Value).
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
	atom_number(ProcValue, ResultValue).
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
xpath_expr(duration(Value), data('duration', DurationValue)) :-
	validate_xsd_simpleType('duration', Value),
	parse_duration(Value, DurationValue).
/* --- dateTime --- */
xpath_expr(dateTime(Value), data('dateTime', [Year, Month, Day, Hour, Minute, Second, TimeZoneOffset])) :-
	validate_xsd_simpleType('dateTime', Value),
	atom_string(Value, ValueString),
	split_string(ValueString, 'T', '', TSplit),
	TSplit = [DateString, TimeString],
	atom_string(Date, DateString),
	atom_string(Time, TimeString),
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
	atom_string(Value, ValueString),
	(
		% negative TC
		split_string(ValueString, '-', '', MinusSplit),
		MinusSplit = [TimeTMP, TimeZoneTMP],
		TimeZoneSign = '-'
		;
		% positive TC
		split_string(ValueString, '+', '', PlusSplit),
		PlusSplit = [TimeTMP, TimeZoneTMP],
		TimeZoneSign = '+'
		;
		% UTC TC
		split_string(ValueString, 'Z', '', ZSplit),
		ZSplit = [TimeTMP, _],
		TimeZoneSign = '+',
		TimeZoneTMP = '00:00'
		;
		% no TC
		split_string(ValueString, 'Z+-', '', AllSplit),
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
	atom_string(Value, ValueString),
	split_string(ValueString, '-', '', MinusSplit),
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
	atom_string(Value, ValueString),
	split_string(ValueString, '-', '', MinusSplit),
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
	atom_string(Value, ValueString),
	split_string(ValueString, '-', '', MinusSplit),
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
	atom_string(Value, ValueString),
	split_string(ValueString, '-', '', MinusSplit),
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
	atom_string(Value, ValueString),
	split_string(ValueString, '-', '', MinusSplit),
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
	atom_string(Value, ValueString),
	split_string(ValueString, '-', '', MinusSplit),
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
xpath_expr(hexBinary(Value), data('hexBinary', [ResultValue])) :-
	validate_xsd_simpleType('hexBinary', Value),
	atom_string(Value, ValueString),
	string_upper(ValueString, UpperCaseValue),
	atom_string(ResultValue, UpperCaseValue).
/* --- base64Binary --- */
xpath_expr(base64Binary(Value), data('base64Binary', [ResultValue])) :-
	validate_xsd_simpleType('base64Binary', Value),
	atom_string(Value, ValueString),
	string_upper(ValueString, UpperCaseValue),
	atomic_list_concat(TMP, ' ', UpperCaseValue),
	atomic_list_concat(TMP, '', ResultValue).
/* --- anyURI --- */
xpath_expr(anyURI(Value), data('anyURI', [Value])) :-
	validate_xsd_simpleType('anyURI', Value).
/* --- QName --- */
xpath_expr(QName(Value), data('QName', [Value])) :-
	validate_xsd_simpleType('QName', Value).
/* --- normalizedString --- */
xpath_expr(normalizedString(Value), data('normalizedString', [Value])) :-
	validate_xsd_simpleType('normalizedString', Value).
/* --- token --- */
xpath_expr(token(Value), data('token', [Value])) :-
	validate_xsd_simpleType('token', Value).
/* --- language --- */
xpath_expr(language(Value), data('language', [Value])) :-
	validate_xsd_simpleType('language', Value).
/* --- NMTOKEN --- */
xpath_expr(NMTOKEN(Value), data('NMTOKEN', [ValueSanitized])) :-
	normalize_space(atom(ValueSanitized), Value),
	validate_xsd_simpleType('NMTOKEN', ValueSanitized).
/* --- NCName --- */
xpath_expr(NCName(Value), data('NCName', [Value])) :-
	validate_xsd_simpleType('NCName', Value).
/* --- Name --- */
xpath_expr(Name(Value), data('Name', [Value])) :-
	validate_xsd_simpleType('Name', Value).
/* --- ID --- */
xpath_expr(ID(Value), data('ID', [Value])) :-
	validate_xsd_simpleType('ID', Value).
/* --- IDREF --- */
xpath_expr(IDREF(Value), data('IDREF', [Value])) :-
	validate_xsd_simpleType('IDREF', Value).
/* --- ENTITY --- */
xpath_expr(ENTITY(Value), data('ENTITY', [Value])) :-
	validate_xsd_simpleType('ENTITY', Value).
/* --- integer --- */
xpath_expr(integer(Value), data('integer', [NumberValue])) :-
	validate_xsd_simpleType('integer', Value),
	atom_number(Value, NumberValue).
/* --- nonPositiveInteger --- */
xpath_expr(nonPositiveInteger(Value), data('nonPositiveInteger', [NumberValue])) :-
	validate_xsd_simpleType('nonPositiveInteger', Value),
	atom_number(Value, NumberValue).
/* --- negativeInteger --- */
xpath_expr(negativeInteger(Value), data('negativeInteger', [NumberValue])) :-
	validate_xsd_simpleType('negativeInteger', Value),
	atom_number(Value, NumberValue).
/* --- long --- */
xpath_expr(long(Value), data('long', [NumberValue])) :-
	validate_xsd_simpleType('long', Value),
	atom_number(Value, NumberValue).
/* --- int --- */
xpath_expr(int(Value), data('int', [NumberValue])) :-
	validate_xsd_simpleType('int', Value),
	atom_number(Value, NumberValue).
/* --- short --- */
xpath_expr(short(Value), data('short', [NumberValue])) :-
	validate_xsd_simpleType('short', Value),
	atom_number(Value, NumberValue).
/* --- byte --- */
xpath_expr(byte(Value), data('byte', [NumberValue])) :-
	validate_xsd_simpleType('byte', Value),
	atom_number(Value, NumberValue).
/* --- nonNegativeInteger --- */
xpath_expr(nonNegativeInteger(Value), data('nonNegativeInteger', [NumberValue])) :-
	validate_xsd_simpleType('nonNegativeInteger', Value),
	atom_number(Value, NumberValue).
/* --- unsignedLong --- */
xpath_expr(unsignedLong(Value), data('unsignedLong', [NumberValue])) :-
	validate_xsd_simpleType('unsignedLong', Value),
	atom_number(Value, NumberValue).
/* --- unsignedInt --- */
xpath_expr(unsignedInt(Value), data('unsignedInt', [NumberValue])) :-
	validate_xsd_simpleType('unsignedInt', Value),
	atom_number(Value, NumberValue).
/* --- unsignedShort --- */
xpath_expr(unsignedShort(Value), data('unsignedShort', [NumberValue])) :-
	validate_xsd_simpleType('unsignedShort', Value),
	atom_number(Value, NumberValue).
/* --- unsignedByte --- */
xpath_expr(unsignedByte(Value), data('unsignedByte', [NumberValue])) :-
	validate_xsd_simpleType('unsignedByte', Value),
	atom_number(Value, NumberValue).
/* --- positiveInteger --- */
xpath_expr(positiveInteger(Value), data('positiveInteger', [NumberValue])) :-
	validate_xsd_simpleType('positiveInteger', Value),
	atom_number(Value, NumberValue).
/* --- yearMonthDuration --- */
xpath_expr(yearMonthDuration(Value), data('yearMonthDuration', DurationValue)) :-
	validate_xsd_simpleType('yearMonthDuration', Value),
	parse_duration(Value, DurationValue).
/* --- dayTimeDuration --- */
xpath_expr(dayTimeDuration(Value), data('dayTimeDuration', DurationValue)) :-
	validate_xsd_simpleType('dayTimeDuration', Value),
	parse_duration(Value, DurationValue).
/* --- untypedAtomic --- */
xpath_expr(untypedAtomic(Value), data('untypedAtomic', [Value])) :-
	validate_xsd_simpleType('untypedAtomic', Value).


/* ~~~ Parsing ~~~ */

parse_duration(Value, Result) :-
	atom_string(Value, ValueString),
	split_string(ValueString, 'P', '', PSplit),
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
		data('duration', Result)
	).

parse_float(Value, nan) :-
	Value =~ '^(\\+|-)?NaN$'.
parse_float(Value, inf) :-
	Value =~ '^\\+?INF$'.
parse_float(Value, -inf) :-
	Value =~ '^-INF$'.
parse_float(Value, ResultValue) :-
	atom_number(Value, ResultValue).

is_inf(V) :-
	V = inf.
is_inf(V) :-
	V = -inf.


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