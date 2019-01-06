:- module(xpath, 
	[
		assert/3,
		assertion/4
	]).

:- use_module(library(xsd/xsd_messages)).

:- op(300, xfx, user:(eq)).

assert(D_File, D_ID, XPathExpr) :-
	warning('Testing for ~w with File ~w and ID ~w!', [XPathExpr, D_File, D_ID]).
assertion(D_File, D_ID, D_Text, XPath) :-
	nb_setval(context_file, D_File),
	nb_setval(context_id, D_ID),

	atomic_list_concat(XPathSplit, '$value', XPath),
	atomic_list_concat(XPathSplit, D_Text, XPathSubs),

	term_to_atom(XPathExpr, XPathSubs),
	term_to_atom(EmptyTerm, ''),
	!,
	dif(XPathExpr, EmptyTerm),
	(compound(XPathExpr) ->
		XPathExpr;
		true).

/* ### Operators ### */

/* --- eq --- */

% decimal
eq(Value1, Value2) :-
	% todo: check for type
	Value1 =:= Value2.
% TODO: other data types for eq