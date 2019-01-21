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
xpath_expr(Value1 eq Value2, Result) :-
	xpath_expr(Value1, Result1),
	xpath_expr(Value2, Result2),
	
	% just a simple numeric comparison for now (needs to be replaced)
	Result1 =:= Result2 ->
		Result = data('boolean', [true]);
		Result = data('boolean', [false]).


/* ### Functions ### */

/* ~~~ Constructors ~~~ */

/* --- string --- */
xpath_expr(string(Value), Result) :-
	validate_xsd_simpleType('string', Value),
	term_string(Value, ValueString),
	Result = data('string', [ValueString]).
/* --- boolean --- */
xpath_expr(boolean(Value), Result) :-
	member(Value, ['false', '0']) ->
		Result = data('boolean', [false]);
		Result = data('boolean', [true]).
/* --- decimal --- */
xpath_expr(decimal(Value), Result) :-
	validate_xsd_simpleType('decimal', Value),

	( % add leading 0 in front of decimal point, as prolog cannot handle decimals like ".32"
		Value =~ '^(\\+|-)?\\..*$' ->
		(
			atomic_list_concat(TMP, '.', Value),
			atomic_list_concat(TMP, '0.', ProcValue)	
		);
		ProcValue = Value
	),

	term_string(DataValue, ProcValue),
	Result = data('decimal', [DataValue]).
/* float and double */
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
	warning('~wP~wY~wM~wDT~wH~wM~wS', [Sign, Years, Months, Days, Hours, Minutes, Seconds]),
	UnnormalizedDuration = data('duration', [Sign, Years, Months, Days, Hours, Minutes, Seconds]),
	normalize_duration(UnnormalizedDuration, NormalizedDuration),
	Result = NormalizedDuration.



normalize_duration(Duration, Duration).
normalize_duration(UnnormalizedDuration, NormalizedDuration) :-
	normalize_duration_algo(UnnormalizedDuration, NormalizedDuration).

normalize_duration_algo(
	data('duration', [USign, UYears, UMonths, UDays, UHours, UMinutes, USeconds]),
	data('duration', [NSign, NYears, NMonths, NDays, NHours, NMinutes, NSeconds])) :-
	% 0 =< Seconds =< 60
	NMinutesDiv is USeconds div 60, NSeconds is USeconds mod 60.
	% TODO