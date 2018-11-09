:- module(simpletype, 
	[
		validate_xsd_simpleType/2, 
		facet/3
	]).

:- use_module(library(xsd/xsd_messages)).

% https://github.com/mndrix/regex
:- use_module(library(regex)). 

/*
	SIMPLE TYPES
	validate_xsd_simpleType(Type, Value)
	
	Validates `Value` against (XML-Schema) simpleType `Type`
*/
validate_xsd_simpleType('ID', _).
validate_xsd_simpleType('IDREFS', _).
validate_xsd_simpleType('IDREF', _).
validate_xsd_simpleType('NMTOKEN', V) :-
	V =~ '^[a-zA-Z0-9_-]+$'.
validate_xsd_simpleType('NMTOKENS', V) :-
	V =~ '^[a-zA-Z0-9_-]+( [a-zA-Z0-9_-]+)*$'.

% any
validate_xsd_simpleType(anySimpleType, _).
validate_xsd_simpleType(anyAtomicType, _).
% string
validate_xsd_simpleType(string, _). 
% numbers
validate_xsd_simpleType(boolean, V) :-
	facet(enumeration, ['true', 'false', '1', '0'], V).	
validate_xsd_simpleType(byte, V) :- 
	validate_xsd_simpleType(integer, V),
	facet(minInclusive, -128, V),
	facet(maxInclusive,  127, V).	
validate_xsd_simpleType(decimal, V) :-
	V =~ '^((\\+|-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+))$'. 	
validate_xsd_simpleType(float, V) :-
	V =~ '^((\\+|-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([Ee](\\+|-)?[0-9]+)?|(\\+|-)?INF|NaN)$'.
validate_xsd_simpleType(double, V) :-
	% TODO: 32 bit float/64bit double checken -> wie?
	validate_xsd_simpleType(float, V).
validate_xsd_simpleType(int, V) :- 
	validate_xsd_simpleType(integer, V),
	facet(minInclusive, -2147483648, V),
	facet(maxInclusive,  2147483647, V).  
validate_xsd_simpleType(integer, V) :-
	V =~ '^(\\+|-)?[0-9]+$'.
validate_xsd_simpleType(long, V) :-
	validate_xsd_simpleType(integer, V),
	facet(minInclusive, -9223372036854775808, V),
	facet(maxInclusive,  9223372036854775807, V).
validate_xsd_simpleType(short, V) :-
	validate_xsd_simpleType(integer, V),
	facet(minInclusive, -32768, V),
	facet(maxInclusive,  32767, V). 
% restricted numbers
validate_xsd_simpleType(negativeInteger, V) :-
	validate_xsd_simpleType(integer, V),
	facet(maxInclusive, -1, V).
validate_xsd_simpleType(nonNegativeInteger, V) :-
	validate_xsd_simpleType(integer, V),
	facet(minInclusive, 0, V).
validate_xsd_simpleType(nonPositiveInteger, V) :-
	validate_xsd_simpleType(integer, V),
	facet(maxInclusive, 0, V).	
validate_xsd_simpleType(positiveInteger, V) :-
	validate_xsd_simpleType(integer, V),
	facet(minInclusive, 1, V).		
% unsigned numbers
validate_xsd_simpleType(unsignedByte, V) :-
	validate_xsd_simpleType(integer, V),
	facet(minInclusive, 0, V),
	facet(maxInclusive, 255, V). 	
validate_xsd_simpleType(unsignedInt, V) :-
	validate_xsd_simpleType(integer, V),
	facet(minInclusive, 0, V),
	facet(maxInclusive, 4294967295, V). 
validate_xsd_simpleType(unsignedLong, V) :-
	validate_xsd_simpleType(integer, V),
	facet(minInclusive, 0, V),
	facet(maxInclusive, 18446744073709551615, V). 
validate_xsd_simpleType(unsignedShort, V) :-
	validate_xsd_simpleType(integer, V),
	facet(minInclusive, 0, V),
	facet(maxInclusive, 65535, V). 
validate_xsd_simpleType(date, V) :-
	V =~ '^-?([1-9][0-9]*)?[0-9]{4}-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])((\\+|-)(14:00|1[0-3]:[0-5][0-9]|0[0-9]:[0-5][0-9])|Z)?$'.
validate_xsd_simpleType(time, V) :-
	V =~ '^(([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?|24:00:00(\\.0+)?)((\\+|-)(14:00|1[0-3]:[0-5][0-9]|0[0-9]:[0-5][0-9])|Z)?$'.
validate_xsd_simpleType(datetime, V) :-
	V =~ '^-?([1-9][0-9]*)?[0-9]{4}-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])T(([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?|24:00:00(\\.0+)?)((\\+|-)(14:00|1[0-3]:[0-5][0-9]|0[0-9]:[0-5][0-9])|Z)?$'.
validate_xsd_simpleType(T, _) :-
	check_for_single(T).

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