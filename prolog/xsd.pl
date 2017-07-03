:- module(xsd, [
		xsd_validate/2
	]).

:- use_module(library(xsd/validate)).
:- use_module(library(xsd/flatten)).

:- dynamic xml_loaded/2.

xsd_validate(Xsd, Xml) :-
	xsd_validate(Xsd, Xml, []).

xsd_validate(Xsd, Xml, _Options) :-
	ensure_flattened(Xsd, Xsd_),
	ensure_flattened(Xml, Xml_),
	validate:validate(Xsd_, Xml_).

ensure_flattened(Id, Id) :-
	xml_loaded(Id, _).

ensure_flattened(A, Id) :-
	\+xml_loaded(A, _),
	flatten:xml_flatten(A, Id),
	assert(xml_loaded(A, Id)).


