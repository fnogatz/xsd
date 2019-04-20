:- module(xpath, 
	[
		assert/5
	]).

:- use_module(library(regex)).
:- use_module(library(url)).
:- use_module(library(xsd/flatten)).
:- use_module(library(xsd/simpletype)).
:- use_module(library(xsd/xsd_helper)).
:- use_module(library(xsd/xsd_messages)).

:- op(1, fx, user:($)).
:- op(200, fy, user:(@)).
:- op(250, yf, user:([])).
:- op(300, yfx, user:(::)).
:- op(400, yfx, user:(idiv)).
:- op(400, yfx, user:(/)).
:- op(700, xfx, user:(eq)).
:- op(700, xfx, user:(ne)).
:- op(700, xfx, user:(le)).
:- op(700, xfx, user:(lt)).
:- op(700, xfx, user:(ge)).
:- op(700, xfx, user:(gt)).

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
		xpath_expr(XPathExpr, Result), !, isValid(Result) ->
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

/* --- steps --- */
/* -- axes -- */
xpath_expr(Nodename, Result) :-
	\+compound(Nodename),
	xpath_expr(child::Nodename, Result).
xpath_expr(Axe::Nodename, data('node', [D_Node_ID])) :-
	nb_current(context_file, D_File),
	nb_current(context_id, D_ID),
	(
		Axe = child, child(D_File, D_ID, D_Node_ID)
		% TODO: implement other axes
	),
	(
		Nodename = '*';
		node(D_File, D_Node_ID, _, Nodename)
	).
xpath_expr(Node/Nodename, Result) :-
	\+compound(Nodename),
	xpath_expr(Node/child::Nodename, Result).
xpath_expr(Node/Axe::Nodename, data('node', [D_Node_ID])) :-
	xpath_expr(Node, data('node', [D_Parent_ID])),
	nb_current(context_file, D_File),
	(
		Axe = child, child(D_File, D_Parent_ID, D_Node_ID)
		% TODO: implement other axes
	),
	(
		Nodename = '*';
		node(D_File, D_Node_ID, _, Nodename)
	).

/* --- predicates --- */
xpath_expr(Node[Predicate], data('node', [D_Node_ID])) :-
	xpath_expr(Node, data('node', [D_Node_ID])),
	xpath_expr(Predicate, PredicateResult),
	isValid(PredicateResult).

/* --- attributes --- */
xpath_expr(@Attribute, Result) :-
	nb_current(context_file, D_File),
	nb_current(context_id, D_ID),
	node_attribute(D_File, D_ID, Attribute, AttributeValue),
	xpath_expr(AttributeValue, Result).


/* ### Operators ### */

xpath_expr(Value1 + Value2, Result) :-
	xpath_expr(numeric-add(Value1, Value2), Result).

xpath_expr(Value1 - Value2, Result) :-
	/* the next two lines are there to avoid performance issues caused by recursion */
	(compound(Value1); number(Value1); Value1 =~ '^(\\+|-)?INF|NaN$'),
	(compound(Value2); number(Value2); Value2 =~ '^(\\+|-)?INF|NaN$'),
	xpath_expr(numeric-subtract(Value1, Value2), Result).

xpath_expr(Value1 * Value2, Result) :-
	xpath_expr(numeric-multiply(Value1, Value2), Result).

xpath_expr(Value1 div Value2, Result) :-
	xpath_expr(numeric-divide(Value1, Value2), Result).

xpath_expr(Value1 idiv Value2, Result) :-
	xpath_expr(numeric-integer-divide(Value1, Value2), Result).

xpath_expr(Value1 mod Value2, Result) :-
	/* the next two lines are there to avoid performance issues caused by recursion */
	(compound(Value1); number(Value1); Value1 =~ '^(\\+|-)?INF|NaN$'),
	(compound(Value2); number(Value2); Value2 =~ '^(\\+|-)?INF|NaN$'),
	xpath_expr(numeric-mod(Value1, Value2), Result).

xpath_expr(+Value, Result) :-
	xpath_expr(numeric-unary-plus(Value), Result).

xpath_expr(-Value, Result) :-
	xpath_expr(numeric-unary-minus(Value), Result).

xpath_expr(Value1 eq Value2, Result) :-
	% TODO: other types
	xpath_expr(numeric-equal(Value1, Value2), Result).
xpath_expr(Value1 ne Value2, data('boolean', [ResultValue])) :-
	% TODO: other types
	xpath_expr(numeric-equal(Value1, Value2), data('boolean', [EqualValue])),
	(
		EqualValue = true ->
			ResultValue = false;
			ResultValue = true
	).
xpath_expr(Value1 le Value2, data('boolean', [ResultValue])) :-
	xpath_expr(numeric-less-than(Value1, Value2), data('boolean', [ResultValue1])),
	xpath_expr(numeric-equal(Value1, Value2), data('boolean', [ResultValue2])),
	(
		ResultValue1 = true; ResultValue2 = true ->
			ResultValue = true;
			ResultValue = false 
	).
xpath_expr(Value1 lt Value2, Result) :-
	xpath_expr(numeric-less-than(Value1, Value2), Result).
xpath_expr(Value1 ge Value2, data('boolean', [ResultValue])) :-
	xpath_expr(numeric-greater-than(Value1, Value2), data('boolean', [ResultValue1])),
	xpath_expr(numeric-equal(Value1, Value2), data('boolean', [ResultValue2])),
	(
		ResultValue1 = true; ResultValue2 = true ->
			ResultValue = true;
			ResultValue = false 
	).
xpath_expr(Value1 gt Value2, Result) :-
	xpath_expr(numeric-greater-than(Value1, Value2), Result).


/* ### constructors ### */

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


/* ### numerics ### */

xpath_expr(numeric-add(Value1, Value2), data(Type, [ResultValue])) :-
	xpath_expr(Value1, Inter1),
	xpath_expr(Value2, Inter2),
	!,
	xpath_expr_cast(Inter1, data(Type, [InternalValue1])),
	xpath_expr_cast(Inter2, data(Type, [InternalValue2])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% if one operand is nan, return it
		(InternalValue1 = nan; InternalValue2 = nan), ResultValue = nan;
		% if one operand is infinite, return it
		\+is_inf(InternalValue1), is_inf(InternalValue2), ResultValue = InternalValue2;
		is_inf(InternalValue1), \+is_inf(InternalValue2), ResultValue = InternalValue1;
		% if both operands are infinite, return them if they are equal, otherwise return nan
		is_inf(InternalValue1), is_inf(InternalValue2), (InternalValue1 = InternalValue2 -> ResultValue = InternalValue1; ResultValue = nan);
		% if both operands are finite, perform an arithmetic addition.
		\+is_inf(InternalValue1), \+is_inf(InternalValue2), ResultValue is InternalValue1 + InternalValue2
	).
xpath_expr(numeric-subtract(Value1, Value2), data(Type, [ResultValue])) :-
	xpath_expr(Value1, Inter1),
	xpath_expr(Value2, Inter2),
	!,
	xpath_expr_cast(Inter1, data(Type, [InternalValue1])),
	xpath_expr_cast(Inter2, data(Type, [InternalValue2])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% if one operand is nan, return it
		(InternalValue1 = nan; InternalValue2 = nan), ResultValue = nan;
		% if the first operand is not inf and the second one is, return the second one's negation
		\+is_inf(InternalValue1), is_inf(InternalValue2), 
			xpath_expr(-Value2, data(Type, [ResultValue]));
		% if the first operand is inf and the second is not, return the first one
		is_inf(InternalValue1), \+is_inf(InternalValue2), ResultValue = InternalValue1;
		% if both operands are inf, return nan if they are equal, otherwise the appropriate inf value
		is_inf(InternalValue1), is_inf(InternalValue2), (InternalValue1 = InternalValue2 -> ResultValue = nan; ResultValue = InternalValue1);
		% if both operands are finite, perform a regular subtraction
		\+is_inf(InternalValue1), \+is_inf(InternalValue2), ResultValue is InternalValue1 - InternalValue2		
	).
xpath_expr(numeric-multiply(Value1, Value2), data(Type, [ResultValue])) :-
	xpath_expr(Value1, Inter1),
	xpath_expr(Value2, Inter2),
	!,
	xpath_expr_cast(Inter1, data(Type, [InternalValue1])),
	xpath_expr_cast(Inter2, data(Type, [InternalValue2])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% if one operand is nan, return it
		(InternalValue1 = nan; InternalValue2 = nan), ResultValue = nan;
		% if one operand is zero and the other is an infinity, return nan
		InternalValue1 =:= 0, is_inf(InternalValue2), ResultValue = nan;
		is_inf(InternalValue1), InternalValue2 =:= 0, ResultValue = nan;
		% if one operand is an infinity and the other one is a finite number != 0, return the effective infinity
		InternalValue1 =\= 0, \+is_inf(InternalValue1), is_inf(InternalValue2),
			(InternalValue1 < 0 ->
				xpath_expr(-Value2, data(_, [ResultValue]))
				;
				ResultValue = InternalValue2
			);
		is_inf(InternalValue1), InternalValue2 =\= 0, \+is_inf(InternalValue2),
			(InternalValue2 < 0 ->
				xpath_expr(-Value1, data(_, [ResultValue]))
				;
				ResultValue = InternalValue1
			);
		% if both operands are infinities, multiply them
		is_inf(InternalValue1), is_inf(InternalValue2), (InternalValue1 = InternalValue2 -> ResultValue = inf; ResultValue = -inf);
		% if no operand is an infinity, perform a regular arithmetic multiplication
		\+is_inf(InternalValue1), \+is_inf(InternalValue2), ResultValue is InternalValue1 * InternalValue2
	).
xpath_expr(numeric-divide(Value1, Value2), data(Type, [ResultValue])) :-
	xpath_expr(Value1, Inter1),
	xpath_expr(Value2, Inter2),
	!,
	xpath_expr_cast(Inter1, data(Type, [InternalValue1])),
	xpath_expr_cast(Inter2, data(Type, [InternalValue2])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% if one operand is nan, return it
		(InternalValue1 = nan; InternalValue2 = nan), ResultValue = nan;
		% if a positive number is divided by a zero, return inf with the same sign as the zero
		% PROBLEM: prolog cannot decide between -0 and 0, so return inf.
		InternalValue1 > 0, InternalValue2 =:= 0, ResultValue = inf;
		% if a negative number is divided by a zero, return inf with the opposite sign as the zero
		% PROBLEM: prolog cannot decide between -0 and 0, so return -inf.
		InternalValue1 < 0, InternalValue2 =:= 0, ResultValue = -inf;
		% if a zero is divided by a zero, return nan
		InternalValue1 =:= 0, InternalValue2 =:= 0, ResultValue = nan;
		% if an inf is divided by an inf, return nan
		is_inf(InternalValue1), is_inf(InternalValue2), ResultValue = nan;
		% else perform a regular arithmetic division
		ResultValue is InternalValue1 / InternalValue2
	).
xpath_expr(numeric-integer-divide(Value1, Value2), data('integer', [ResultValue])) :-
	xpath_expr(Value1, Inter1),
	xpath_expr(Value2, Inter2),
	!,
	xpath_expr_cast(Inter1, data(Type, [InternalValue1])),
	xpath_expr_cast(Inter2, data(Type, [InternalValue2])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% the first operand may not be an inf or nan
		\+is_inf(InternalValue1),
		InternalValue1 \= nan,
		% the second operand may not be zero or nan
		InternalValue2 =\= 0,
		InternalValue2 \= nan,
		(
			% if the second operand is an inf, return 0
			is_inf(InternalValue2), ResultValue = 0
			;
			% $a idiv $b is the same as ($a div $b) cast as xs:integer (flooring)
			xpath_expr(numeric-divide(Value1, Value2), data(_, [DivisionResultValue])),
			!,
			ResultValue is truncate(DivisionResultValue)
		)
	).
xpath_expr(numeric-mod(Value1, Value2), data(Type, [ResultValue])) :-
	xpath_expr(Value1, Inter1),
	xpath_expr(Value2, Inter2),
	!,
	xpath_expr_cast(Inter1, data(Type, [InternalValue1])),
	xpath_expr_cast(Inter2, data(Type, [InternalValue2])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% if the type is decimal, a zero as second operator is not allowed
		Type \= 'decimal';
		InternalValue2 \= 0
	),
	(
		% if at least one operand is nan, return nan
		(InternalValue1 = nan; InternalValue2 = nan), ResultValue = nan, !;
		% if the first operand is an infinity, return nan
		is_inf(InternalValue1), ResultValue = nan;
		% if the second operand is a zero, return nan
		InternalValue2 =:= 0, ResultValue = nan;
		% if the first operand is finite and the second is an infinity, return the first operand
		is_inf(InternalValue2), ResultValue = InternalValue1;
		% if the first operand is a zero and the second is finite, return the first operand
		InternalValue1 =:= 0, \+is_inf(InternalValue2), ResultValue = InternalValue1;
		% else perform regular modulo operation
		InternalValue1 =\= 0,
		InternalValue1 \= nan,
		\+is_inf(InternalValue1),
		InternalValue2 =\= 0,
		InternalValue2 \= nan,
		\+is_inf(InternalValue2),
		xpath_expr(numeric-integer-divide(InternalValue1, InternalValue2), data(_, [Divident])),
		!,
		ResultValue is InternalValue1 - InternalValue2 * Divident
	).
xpath_expr(numeric-unary-plus(Value), data(Type, [ResultValue])) :-
	xpath_expr(Value, data(Type, [ResultValue])),
	xsd_simpleType_is_a(Type, AllowedType),
	member(AllowedType, ['decimal', 'double', 'float']).

xpath_expr(numeric-unary-minus(Value), data(Type, [ResultValue])) :-
	xpath_expr(Value, data(Type, [InternalValue])),
	xsd_simpleType_is_a(Type, AllowedType),
	member(AllowedType, ['decimal', 'double', 'float']),
	(
		% if the operand is nan, return it
		InternalValue = nan, ResultValue = InternalValue;
		% inf is negated separately
		InternalValue = inf, ResultValue = -inf;
		InternalValue = -inf, ResultValue = inf;
		% otherwise the negation is equal to 0 - value
		ResultValue is 0 - InternalValue
	).
xpath_expr(numeric-equal(Value1, Value2), data('boolean', [ResultValue])) :-
	xpath_expr(Value1, Inter1),
	xpath_expr(Value2, Inter2),
	!,
	xpath_expr_cast(Inter1, data(Type, [InternalValue1])),
	xpath_expr_cast(Inter2, data(Type, [InternalValue2])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% +0 equals -0, but it is the same for prolog anyway
		% nan does not equal itself, all other values do equal themselves
		InternalValue1 \= nan, InternalValue1 = InternalValue2 ->
			ResultValue = true;
			ResultValue = false
	).
xpath_expr(numeric-less-than(Value1, Value2), data('boolean', [ResultValue])) :-
	xpath_expr(Value1, Inter1),
	xpath_expr(Value2, Inter2),
	!,
	xpath_expr_cast(Inter1, data(Type, [InternalValue1])),
	xpath_expr_cast(Inter2, data(Type, [InternalValue2])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% positive inf is greater than everything else, except nan and itself
		InternalValue1 \= inf, InternalValue1 \= nan, InternalValue2 = inf;
		% negative inf is less than everything else, except nan and itself
		InternalValue1 = -inf, InternalValue2 \= nan, InternalValue2 \= -inf;
		% if both values are neither an inf nor nan, return true if operand1 < operand2
		\+is_inf(InternalValue1), \+is_inf(InternalValue2),
		InternalValue1 \= nan, InternalValue2 \= nan,
		InternalValue1 < InternalValue2
	) -> ResultValue = true; ResultValue = false.
xpath_expr(numeric-greater-than(Value1, Value2), data('boolean', [ResultValue])) :-
	xpath_expr(Value1, Inter1),
	xpath_expr(Value2, Inter2),
	!,
	xpath_expr_cast(Inter1, data(Type, [InternalValue1])),
	xpath_expr_cast(Inter2, data(Type, [InternalValue2])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% positive inf is greater than everything else, except nan and itself
		InternalValue1 = inf, InternalValue2 \= inf, InternalValue2 \= nan;
		% negative inf is less than everything else, except nan and itself
		InternalValue1 \= nan, InternalValue1 \= -inf, InternalValue2 = -inf;
		% if both values are neither an inf nor nan, return true if operand1 < operand2
		\+is_inf(InternalValue1), \+is_inf(InternalValue2),
		InternalValue1 \= nan, InternalValue2 \= nan,
		InternalValue1 > InternalValue2
	) -> ResultValue = true; ResultValue = false.
xpath_expr(abs(Value), data(Type, [ResultValue])) :-
	xpath_expr(Value, Inter),
	!,
	xpath_expr_cast(Inter, data(Type, [InternalValue])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% return nan for nan operands
		InternalValue = nan, ResultValue = nan;
		% return positive infinity for infinity operands
		is_inf(InternalValue), ResultValue = inf;
		% return the negation for negative values
		InternalValue < 0, xpath_expr(numeric-unary-minus(Value), data(Type, [ResultValue]));
		% return the operand itself for positive operands
		InternalValue >= 0, ResultValue = InternalValue
	).
xpath_expr(ceiling(Value), data(Type, [ResultValue])) :-
	xpath_expr(Value, Inter),
	!,
	xpath_expr_cast(Inter, data(Type, [InternalValue])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% return the operand itself for nan and any inf
		member(InternalValue, [nan, inf, -inf]), ResultValue = InternalValue;
		% otherwise return the ceiling of the operand
		ResultValue is ceiling(InternalValue)
	).
xpath_expr(floor(Value), data(Type, [ResultValue])) :-
	xpath_expr(Value, Inter),
	!,
	xpath_expr_cast(Inter, data(Type, [InternalValue])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% return the operand itself for nan and any inf
		member(InternalValue, [nan, inf, -inf]), ResultValue = InternalValue;
		% otherwise return the floor of the operand
		ResultValue is floor(InternalValue)
	).
xpath_expr(round(Value), data(Type, [ResultValue])) :-
	xpath_expr(Value, Inter),
	!,
	xpath_expr_cast(Inter, data(Type, [InternalValue])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% return the operand itself for nan and any inf
		member(InternalValue, [nan, inf, -inf]), ResultValue = InternalValue;
		% otherwise return the rounded value of the operand
		(
			xpath_expr(numeric-mod(InternalValue, 1), data(_, [-0.5])) ->
				% -.5 is rounded towards positive infinity, so it is ceiled
				ResultValue is ceiling(InternalValue);
				ResultValue is round(InternalValue)
		)
	).
xpath_expr(round-half-to-even(Value), Result) :-
	xpath_expr(round-half-to-even(Value, 0), Result).
xpath_expr(round-half-to-even(Value, Precision), data(Type, [ResultValue])) :-
	xpath_expr(Value, Inter),
	!,
	xpath_expr_cast(Inter, data(Type, [InternalValue])),
	member(Type, ['decimal', 'double', 'float']),
	(
		% return the operand itself for nan and any inf
		member(InternalValue, [nan, inf, -inf]), ResultValue = InternalValue
		;
		% otherwise return the rounded value of the operand with respect to the precision
		% apply precision
		PrecisionedValue is InternalValue * 10 ** Precision,
		(
			xpath_expr(numeric-mod(PrecisionedValue, 1), data(_, [Mod])),
			member(Mod, [0.5, -0.5]) ->
				% -.5 is rounded towards the adjacent even number
				Ceil is ceiling(PrecisionedValue),
				Floor is floor(PrecisionedValue), 
				(
					Ceil mod 2 =:= 0 ->
						PrecisionedResultValue = Ceil;
						PrecisionedResultValue = Floor	
				)
				;
				% otherwise it is rounded as usual
				PrecisionedResultValue is round(PrecisionedValue)
		),
		% reverse precision
		ResultValue is PrecisionedResultValue * 10 ** (0-Precision)
	).


/* ### strings ### */
xpath_expr(IN, Result) :-
	IN =.. [concat|Tail],
	string_concat(Tail, ResultString),
	atom_string(ResultAtom, ResultString),
	xpath_expr(string(ResultAtom), Result).
xpath_expr(matches(Value, Pattern), Result) :-
	xpath_expr(matches(Value, Pattern, ''), Result). 
xpath_expr(matches(Value, Pattern, Flags), data('boolean', [ResultValue])) :-
	% TODO: add support for providing nodes as the value
	xpath_expr(Value, data(_, [InternalValue])),
	!,
	(
		InternalValue =~ Pattern/Flags ->
			ResultValue = true;
			ResultValue = false	
	).


/* ### anyURI ### */
xpath_expr(resolve-uri(Relative), Result) :-
	% TODO: replace with fn:static-base-uri when implemented
	xpath_expr(resolve-uri(Relative, 'http://localhost'), Result). 
xpath_expr(resolve-uri(Relative, Base), data('anyURI', [ResultValue])) :-
	xpath_expr(Relative, data(_, [InternalRelativeValue])),
	xpath_expr(Base, data(_, [InternalBaseValue])),
	!,
	(
		% TODO: return Relative, if it is the empty sequence
		xpath_expr(matches(InternalRelativeValue,'^[a-z]+:'), data(_, [true])) ->
			ResultValue = InternalRelativeValue
			;
			xpath_expr(matches(InternalBaseValue,'^[a-z]+:'), data(_, [true])),
			parse_url(InternalBaseValue, UrlList),
			(
				member(protocol(Protocol), UrlList) ->
					BaseUrlProtocol = Protocol
					;
					BaseUrlProtocol = ''
			),
			(
				member(host(Host), UrlList) ->
					atomic_list_concat([BaseUrlProtocol, Host], ://, BaseUrlHost)
					;
					BaseUrlHost = BaseUrlProtocol
			),
			(
				member(port(Port), UrlList) ->
					atomic_list_concat([BaseUrlHost, Port], :, BaseUrl)
					;
					BaseUrl = BaseUrlHost
			),
			(
				InternalRelativeValue = '' ->
					ResultValue = BaseUrl
					;
					atomic_list_concat([BaseUrl, InternalRelativeValue], /, ResultValue)
			)
	).



/* ~~~ Parsing ~~~ */

/**
 * This predicate mimics casting functionality.
 * Only numbers can be casted for now!
 * 
 * E.g. an unsignedLong with value 3 can be casted to a float with value 3,
 * although the type unsignedLong is no direct descendant of type float,
 * but it can be casted to it due to its value space.
 */
xpath_expr_cast(data(_, [InternalValue]), Result) :-
	number(InternalValue),
	xpath_expr(InternalValue, Result).
xpath_expr_cast(data(_, [-inf]), Result) :-
	xpath_expr('-INF', Result).
xpath_expr_cast(data(_, [inf]), Result) :-
	xpath_expr('INF', Result).
xpath_expr_cast(data(_, [nan]), Result) :-
	xpath_expr('NaN', Result).
xpath_expr_cast(data(_, [InternalValue]), Result) :-
	\+number(InternalValue),
	xpath_expr(InternalValue, Result).

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
/* --- checks if data structure is valid ("true / not empty ...") --- */
isValid(data(T, R)) :-
	T \= 'boolean'; R = [true].
/* --- recursive string concatenation function --- */
string_concat([], "").
string_concat([H|T], OUT) :-
	string_concat(T, IN),
	!,
	xpath_expr(H, data(_, [InternalValue])),
	!,
	string_concat(InternalValue, IN, OUT).
/* --- checks if the given value is a positive or negative infinity --- */
is_inf(V) :-
	V = inf.
is_inf(V) :-
	V = -inf.
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