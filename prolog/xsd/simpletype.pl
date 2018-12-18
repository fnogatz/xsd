/*
	This module is responsible for the validation of xml schema types.
	@see https://www.w3.org/TR/xmlschema11-2/ for more information.
*/
:- module(simpletype,
	[
		validate_xsd_simpleType/2,
		facet/3
	]).

:- use_module(library(xsd/xsd_messages)).

% https://github.com/mndrix/regex
:- use_module(library(regex)).


/*
	TYPE VALIDATION
	validate_xsd_simpleType(Type, Value)
	--> validates `Value` against (XML-Schema) type `Type`
*/

% top of hierarchy (semantically equivalent in our case, but required by specification)
validate_xsd_simpleType('anyType', _).
validate_xsd_simpleType('anySimpleType', V) :-
	validate_xsd_simpleType('anyType', V).
validate_xsd_simpleType('untyped', V) :-
	validate_xsd_simpleType('anyType', V).

% non atomic types
validate_xsd_simpleType('IDREFS', V) :-
	split_string(V, " ", "", List),
	length(List, Length),
	Length > 0,
	validate_xsd_simpleType_list('IDREF', List).
validate_xsd_simpleType('NMTOKENS', V) :-
	split_string(V, " ", "", List),
	length(List, Length),
	Length > 0,
	validate_xsd_simpleType_list('NMTOKEN', List).
validate_xsd_simpleType('ENTITIES', V) :-
	split_string(V, " ", "", List),
	length(List, Length),
	Length > 0,
	validate_xsd_simpleType_list('ENTITY', List).

% atomic types
validate_xsd_simpleType('anyAtomicType', V) :-
	validate_xsd_simpleType('anySimpleType', V).
validate_xsd_simpleType('untypedAtomic', V) :-
	validate_xsd_simpleType('anyAtomicType', V).
validate_xsd_simpleType('datetime', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	V =~ '^-?([1-9][0-9]*)?[0-9]{4}-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])T(([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?|24:00:00(\\.0+)?)((\\+|-)(14:00|1[0-3]:[0-5][0-9]|0[0-9]:[0-5][0-9])|Z)?$'.
validate_xsd_simpleType('date', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	V =~ '^-?([1-9][0-9]*)?[0-9]{4}-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])((\\+|-)(14:00|1[0-3]:[0-5][0-9]|0[0-9]:[0-5][0-9])|Z)?$'.
validate_xsd_simpleType('time', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	V =~ '^(([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?|24:00:00(\\.0+)?)((\\+|-)(14:00|1[0-3]:[0-5][0-9]|0[0-9]:[0-5][0-9])|Z)?$'.
validate_xsd_simpleType('float', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	% TODO: validate value range (32bit)
	V =~ '^((\\+|-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([Ee](\\+|-)?[0-9]+)?|(\\+|-)?INF|NaN)$'.
validate_xsd_simpleType('double', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	% TODO: validate value range (64bit)
	validate_xsd_simpleType('float', V).
validate_xsd_simpleType('gYearMonth', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	V =~ '^-?([1-9][0-9]{3,}|0[0-9]{3})-(0[1-9]|1[0-2])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?$'.
validate_xsd_simpleType('gYear', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	V =~ '^-?([1-9][0-9]{3,}|0[0-9]{3})(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?$'.
validate_xsd_simpleType('gMonthDay', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	V =~ '^--((0[1-9]|1[0-2])-([01][1-9]|10|2[0-8]))|((0[13-9]|1[0-2])-(29|30))|((0[13578]|1[0-2])/31)(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?$'.
validate_xsd_simpleType('gDay', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	V =~ '^---(0[1-9]|[12][0-9]|3[01])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?$'.
validate_xsd_simpleType('gMonth', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	V =~ '^--(0[1-9]|1[0-2])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?$'.
validate_xsd_simpleType('boolean', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	facet(enumeration, ['true', 'false', '1', '0'], V).
validate_xsd_simpleType('base64Binary', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	V =~ '^((([A-Za-z0-9+/] ?){4})*(([A-Za-z0-9+/] ?){3}[A-Za-z0-9+/]|([A-Za-z0-9+/] ?){2}[AEIMQUYcgkosw048] ?=|[A-Za-z0-9+/] ?[AQgw] ?= ?=))?$'.
validate_xsd_simpleType('hexBinary', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	V =~ '^([0-9a-fA-F]{2})*$'.
validate_xsd_simpleType('anyURI', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	% whoever has to debug the following regex is a poor sod
	V =~ '^([a-zA-Z][a-zA-Z0-9+\\-.]*:(((//)?((([a-zA-Z0-9\\-._~!$&()*+,;=:]|(%[0-9a-fA-F][0-9a-fA-F]))*@)?((\\[(([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)|([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?:|([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?:([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)|([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?|([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?|([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?|([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?:)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?|([0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?):((:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?)|:((:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?(:[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)?|:))])|(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]).([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]).([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]).([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]))|([a-zA-Z0-9\\-._~!$&()*+,;=]|(%[0-9a-fA-F][0-9a-fA-F]))*)(:[0-9]*)?))((/([a-zA-Z0-9\\-._~!$&()*+,;=:@]|(%[0-9a-fA-F][0-9a-fA-F]))*)*|/(([a-zA-Z0-9\\-._~!$&()*+,;=:@]|(%[0-9a-fA-F][0-9a-fA-F]))(/([a-zA-Z0-9\\-._~!$&()*+,;=:@]|(%[0-9a-fA-F][0-9a-fA-F]))*)*)?|([a-zA-Z0-9\\-._~!$&()*+,;=:@]|(%[0-9a-fA-F][0-9a-fA-F]))(/([a-zA-Z0-9\\-._~!$&()*+,;=:@]|(%[0-9a-fA-F][0-9a-fA-F]))*)*))(\\?([a-zA-Z0-9\\-._~!$&()*+,;=:@/?]|(%[0-9a-fA-F][0-9a-fA-F]))*)?(#([a-zA-Z0-9\\-._~!$&()*+,;=:@/?]|(%[0-9a-fA-F][0-9a-fA-F]))*)?)$'.
%
% TODO: QName, NOTATION
%
validate_xsd_simpleType('duration', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	V =~ '^-?P([0-9]+Y)?([0-9]+M)?([0-9]+D)?(T([0-9]+H)?([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?)?$', % general regexp
	V =~ '^.*[YMDHS].*$', % at least one of the properties (year, month, day, hour, minute or second) must be specified
	V =~ '^.*[^T]$'. % if there is a 'T' (separator between day and time properties), it must be followed by a time property (hour, minute or second)
validate_xsd_simpleType('yearMonthDuration', V) :-
	validate_xsd_simpleType('duration', V),
	V =~ '^[^DT]*$'. % only durations with year and/or month properties are allowed
validate_xsd_simpleType('dayTimeDuration', V) :-
	validate_xsd_simpleType('duration', V),
	V =~ '^[^YM]*[DT].*$'. % only durations with day, hour, minute and/or second properties are allowed

% decimals
validate_xsd_simpleType('decimal', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	V =~ '^((\\+|-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+))$'.
validate_xsd_simpleType('integer', V) :-
	validate_xsd_simpleType('decimal', V),
	V =~ '^(\\+|-)?[0-9]+$'.
% non positive integers
validate_xsd_simpleType('nonPositiveInteger', V) :-
	validate_xsd_simpleType('integer', V),
	facet(maxInclusive, 0, V).
validate_xsd_simpleType('negativeInteger', V) :-
	validate_xsd_simpleType('integer', V),
	facet(maxInclusive, -1, V).
% longs
validate_xsd_simpleType('long', V) :-
	validate_xsd_simpleType('integer', V),
	facet(minInclusive, -9223372036854775808, V),
	facet(maxInclusive,  9223372036854775807, V).
validate_xsd_simpleType('int', V) :-
	validate_xsd_simpleType('long', V),
	facet(minInclusive, -2147483648, V),
	facet(maxInclusive,  2147483647, V). 
validate_xsd_simpleType('short', V) :-
	validate_xsd_simpleType('int', V),
	facet(minInclusive, -32768, V),
	facet(maxInclusive,  32767, V).
validate_xsd_simpleType('byte', V) :- 
	validate_xsd_simpleType('short', V),
	facet(minInclusive, -128, V),
	facet(maxInclusive,  127, V).
% non negative integers
validate_xsd_simpleType('nonNegativeInteger', V) :-
	validate_xsd_simpleType('integer', V),
	facet(minInclusive, 0, V).
% unsigned longs
validate_xsd_simpleType('unsignedLong', V) :-
	validate_xsd_simpleType('nonNegativeInteger', V),
	facet(minInclusive, 0, V),
	facet(maxInclusive, 18446744073709551615, V). 
validate_xsd_simpleType('unsignedInt', V) :-
	validate_xsd_simpleType('unsignedLong', V),
	facet(minInclusive, 0, V),
	facet(maxInclusive, 4294967295, V). 
validate_xsd_simpleType('unsignedShort', V) :-
	validate_xsd_simpleType('unsignedInt', V),
	facet(minInclusive, 0, V),
	facet(maxInclusive, 65535, V). 
validate_xsd_simpleType('unsignedByte', V) :-
	validate_xsd_simpleType('unsignedShort', V),
	facet(minInclusive, 0, V),
	facet(maxInclusive, 255, V).
% positive integers
validate_xsd_simpleType('positiveInteger', V) :-
	validate_xsd_simpleType('nonNegativeInteger', V),
	facet(minInclusive, 1, V).

% strings
validate_xsd_simpleType('string', V) :-
	validate_xsd_simpleType('anyAtomicType', V),
	string_codes(V, CL),
	forall(
		member(C, CL), 
		validate_xsd_character(C)
	).
validate_xsd_simpleType('normalizedString', V) :-
	validate_xsd_simpleType('string', V),
	V \~ '[\f|\r|\t]'.
validate_xsd_simpleType('token', V) :-
	validate_xsd_simpleType('normalizedString', V),
	V \~ '^[ ]',
	V \~ '[ ]$',
	V \~ '[ ]{2,}'.
validate_xsd_simpleType('language', V) :-
	validate_xsd_simpleType('token', V),
	V =~ '^[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*$'.
validate_xsd_simpleType('NMTOKEN', V) :-
	validate_xsd_simpleType('token', V),
	string_codes(V, CL),
	length(CL, CLL),
	CLL >= 1,
	forall(
		member(C, CL), 
		validate_xsd_name_character(C)
	).
validate_xsd_simpleType('Name', V) :-
	validate_xsd_simpleType('token', V),
	string_codes(V, CL),
	CL = [H|T],
	validate_xsd_name_start_character(H),
	forall(
		member(C, T),
		validate_xsd_name_character(C)	
	).
validate_xsd_simpleType('NCName', V) :-
	validate_xsd_simpleType('Name', V),
	string_codes(V, CL),
	forall(
		member(C, CL),
		C =\= 58 % [ : ], as NCName is a not colonized Name
	).
validate_xsd_simpleType('ID', V) :-
	% same value space as NCName plus the following restrictions, which are not validated here:
	%	- IDs must be unique within an XML instance
	%	- a complex type may not have more than one attribute with a from ID derived type
	%	- ID attributes cannot have a default or fixed value
	validate_xsd_simpleType('NCName', V).
validate_xsd_simpleType('IDREF', V) :-
	% same value space as NCName plus the following restriction, which is not validated here:
	%	- each IDREF must have a corresponding ID in the same XML instance
	validate_xsd_simpleType('NCName', V).
validate_xsd_simpleType('ENTITY', V) :-
	% same value space as NCName plus the following restriction, which is not validated here:
	%	- each ENTITY must match the name of an unparsed entity in a document type definition for the instance
	validate_xsd_simpleType('NCName', V).

validate_xsd_simpleType(T, _) :-
	check_for_single(T).


/*
	validate_xsd_simpleType_list(Type, List)
	--> validates every item in `List` against (XML-Schema) type `Type`
*/
validate_xsd_simpleType_list(_, []).
validate_xsd_simpleType_list(Type, [H|T]) :-
	validate_xsd_simpleType(Type, H),
	validate_xsd_simpleType_list(Type, T).

check_for_single(T) :-
	\+((clause(validate_xsd_simpleType(T,_), B), B \= check_for_single(_))),
	!,
	warning('Type ~w is not yet supported.', [T]),
	false.


/* 
	FACETS
*/
facet(enumeration, List, V) :-
	!,
	member(V, List).
facet(maxInclusive, Max, V) :-
	!,
	number(Max, Max_),
	number(V, V_),
	V_ =< Max_.
facet(maxExclusive, Max, V) :-
	!,
	number(Max, Max_),
	number(V, V_),
	V_ < Max_.
facet(minInclusive, Min, V) :-
	!,
	number(Min, Min_),
	number(V, V_),
	V_ >= Min_.
facet(minExclusive, Min, V) :-
	!,
	number(Min, Min_),
	number(V, V_),
	V_ > Min_.
facet(pattern, Pattern, V) :-
	!,
	regex(Pattern, [], V, _).
facet(length, Length, V) :-
	!,
	number(Length, Length_),
	atom_length(V, Length_).
facet(minLength, Length, V) :-
	!,
	number(Length, Length_),
	atom_length(V, V_Length),
	V_Length >= Length_.
facet(maxLength, Length, V) :-
	!,
	number(Length, Length_),
	atom_length(V, V_Length),
	V_Length =< Length_.
facet(fractionDigits, MaxLength, Value) :-
	!,
	validate_xsd_simpleType(nonNegativeInteger, MaxLength),
	split_string(Value, ".eE", "", ValueParts), %["<integer_digits>", "<fraction_digits>", [...]]
	length(ValueParts, ValuePartsLength),
	(
		% value has no fraction digits, so restriction is fulfilled
		ValuePartsLength < 2; 

		% otherwise fraction digit length must be validated
		(
			number(MaxLength, MaxFractionDigitLength),
			ValueParts = [_, FractionDigits|_],
			digit_length_fraction_part(FractionDigits, FractionDigitLength),
			!,
			FractionDigitLength =< MaxFractionDigitLength
		)
	).
facet(totalDigits, _, Value) :-
	Value =~ '^(\\+|-)?INF|NaN$'.
facet(totalDigits, MaxLength, Value) :-
	!,
	validate_xsd_simpleType(positiveInteger, MaxLength),
	number(MaxLength, MaxDigitLength),
	split_string(Value, ".eE", "", ValueParts), %["<integer_digits>", "<fraction_digits>", [...]]
	length(ValueParts, ValuePartsLength),
	(
		(
			% value has only integer digits
			ValuePartsLength =:= 1,
			ValueParts = [IntDigits|_],
			digit_length_integer_part(IntDigits, DigitLength)
		);
		(
			% value has both integer and fraction digits
			ValuePartsLength =:= 2,
			ValueParts = [IntDigits,FractionDigits|_],
			digit_length_integer_part(IntDigits, IntDigitsLength),
			digit_length_fraction_part(FractionDigits, FractionDigitsLength),
			DigitLength is IntDigitsLength + FractionDigitsLength
		)
	),
	!,
	DigitLength =< MaxDigitLength.

facet(Facet, _, _) :-
	!,
	warning('Facet ~w is not yet supported.', [Facet]),
	fail.


/*
	HELPER FUNCTIONS
*/

number(In, In) :-
	number(In),
	!.
number(In, Out) :-
	atom_number(In, Out).

% returns the length of significant integer digits
digit_length_integer_part(IntegerDigitString, IntegerDigitLength) :-
	% remove insignificant leading zeroes
	string_to_list(IntegerDigitString, IntegerDigitList),
	remove_leading_zeroes(IntegerDigitList, SanitizedIntegerDigitList),
	length(SanitizedIntegerDigitList, SanitizedIntegerDigitListLength),

	% if we removed all digits, then we removed a significant zero
	(SanitizedIntegerDigitListLength =:= 0 ->
		IntegerDigitLength = 1;
		IntegerDigitLength = SanitizedIntegerDigitListLength
	).

% returns the length of significant fraction digits
digit_length_fraction_part(FractionDigitString, FractionDigitLength) :-
	% remove insignificant trailing zeroes
	string_to_list(FractionDigitString, FractionDigitList),
	reverse(FractionDigitList, ReversedFractionDigitList),
	remove_leading_zeroes(ReversedFractionDigitList, SanitizedReversedFractionDigitList),
	length(SanitizedReversedFractionDigitList, FractionDigitLength).

% removes leading zeroes from a char code list
remove_leading_zeroes([], []).
remove_leading_zeroes([H|T], [H|T]) :-
	H =\= 48. % 48 ='0'
remove_leading_zeroes([48|T], T2) :-
	remove_leading_zeroes(T, T2).

% validates whether a given character is a valid xml character
validate_xsd_character(C) :-
	% see xml spec for 'char'
	%  - unicode values have been translated to prolog character code values)
	%  - character values > 65535 are not supported in prolog
	(0 =< C, C < 9249); % [ #x0 to #x2420 ]
	(9249 < C, C < 57347); % [ #x2422 to #x20 ]
	(57352 < C, C < 57355); % [ #x9 to #xA ]
	(57356 < C, C < 57358); % [ #xD ]
	(57476 < C, C < 57478); % [ #x85 ]
	(57503 < C, C < 64976); % [ #xE09F to #xFDCF ]
	(64991 < C, C =< 65535). % [ #FDE0 to Prologs Limit ]

% validates whether a given character is a valid xml name start character
validate_xsd_name_start_character(C) :-
	% see xml spec for 'NameStartChar'
	(57 < C, C < 59); % [ : ]
	(64 < C, C < 91); % [ A to Z ]
	(94 < C, C < 96); % [ _ ]
	(96 < C, C < 123); % [ a to z ]
	(192 < C, C < 215); % [ #xC0 to #xD6 ]
	(215 < C, C < 247); % [ #xD8 to #xF6 ]
	(247 < C, C < 768); % [ #xF8 to #x2FF ]
	(879 < C, C < 894); % [ #x370 to #x37D ]
	(894 < C, C < 8192); % [ #x37F to #x1FFF ]
	(8203 < C, C < 8206); % [ #x200C to #x200D ]
	(8303 < C, C < 8592); % [ #x2070 to #x218F ]
	(11263 < C, C < 12272); % [ #x2C00 to #x2FEF ]
	(12288 < C, C < 55296); % [ #x3001 to #xD7FF ]
	(63743 < C, C < 64976); % [ #xF900 to #xFDCF ]
	(65007 < C, C < 65534). % [ #xFDF0 to #xFFFD ]

% validates whether a given character is a valid xml name character
validate_xsd_name_character(C) :-
	% see xml spec for 'NameChar'
	validate_xsd_name_start_character(C);
	(44 < C, C < 47); % [ - to . ]
	(47 < C, C < 58); % [ 0 to 9 ]
	(182 < C, C < 184); % [ #xB7 ]
	(767 < C, C < 880); % [ #x0300 to #x036F ]
	(8254 < C, C < 8257). % [ #x203F to #x2040 ]