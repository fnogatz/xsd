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
			\=(Value, false)
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
		Result = data('boolean', true);
		Result = data('boolean', false).


/* ### Functions ### */

/* ~~~ Constructors ~~~ */

/* --- string --- */
xpath_expr(string(Value), Result) :-
	validate_xsd_simpleType('string', Value),
	term_string(Value, ValueString),
	Result = data('string', ValueString).
/* --- boolean --- */
xpath_expr(boolean(Value), Result) :-
	member(Value, [false, 0]) ->
		Result = data('boolean', false);
		Result = data('boolean', true).
