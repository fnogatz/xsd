:- module(xsd_simpleType, 
	[
		validate_xsd_simpleType/2, 
		facet/3
	]).

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
	V =~ '^-?([1-9][0-9]*)?[0-9]{4}-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])((\\+|-)(14:00|1[0-3]:[0-5][0-9]|0[0-9]:[0-5][0-9]))?$'.
validate_xsd_simpleType(time, V) :-
	V =~ '^(([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?|24:00:00(\\.0+)?)((\\+|-)(14:00|1[0-3]:[0-5][0-9]|0[0-9]:[0-5][0-9]))?$'.
%% hh:mm:ss.sss
%%	V =~ '^-?([1-9][0-9]*)?[0-9]{4}-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])((\\+|-)(14:00|1[0-3]:[0-5][0-9]|0[0-9]:[0-5][0-9]))?$'.

/* 
	FACETS
*/
facet(enumeration, List, V) :-
	member(V, List).
facet(maxInclusive, Max, V) :-
	atom_number(V, N),
	N =< Max.
facet(maxExclusive, Max, V) :-
	atom_number(V, N),
	N < Max.
facet(minInclusive, Min, V) :-
	atom_number(V, N),
	N >= Min.
facet(minExclusive, Min, V) :-
	atom_number(V, N),
	N > Min.