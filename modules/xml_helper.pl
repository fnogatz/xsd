:- module(xml_helper, 
	[
		node_type/3, 
		split_id/3, 
		parent/2, 
		child/3, 
		get_children/3, 
		get_n_children/4, 
		get_nth_child/4,
		count_children/3,
		sibling/3,
		next_sibling/3,
		last_sibling/3,
		count_remaining_siblings/3,
		get_n_siblings/4,
		get_nth_sibling/4,
		resolve_namespace/4,
		namespace/3
	]).


/*
	HELPER 
	Various predicates for data traversal, recieving children and siblings and namespace handling using nodes created by `flatten_xml/2` inside the Module `xml_flatten`
	
	parent(ID, Parent_ID)
	child(File,ID,Child_ID)
	sibling(File,ID,Sibling_ID)
	
	?- child("test/schema.xsd", [0], [0,0]).
	true.
	?- child("test/schema.xsd", [0,0], _).
	false.
*/
node_type(File, ID, node) :-
	node(File, _NS, _Name, ID).
node_type(File, ID, text_node) :-
	text_node(File, ID, _).
	
split_id(ID, Pos, Parent_ID) :-
	ID = [Pos|Parent_ID].

parent(ID, Parent_ID) :-
	split_id(ID, _, Parent_ID).

child(File, ID, Child_ID) :-
	Child_ID = [_|ID],
	node_type(File, Child_ID, _).
	
get_children(File, ID, Children_IDs) :-
	findall(Child_ID, child(File, ID, Child_ID), Children_IDs).
	
get_n_children(File, ID, N, Children_IDs) :-
	get_children(File, ID, All_Children_IDs),
	length(Children_IDs, N),
	append(Children_IDs, _, All_Children_IDs).

/*
	Counting starts at 1 (Nth1 = 1 -> first child)
*/
get_nth_child(File, ID, Nth, Child_ID) :-
	Nth0 is Nth - 1,
	Nth0 >= 0,
	split_id(Child_ID, Nth0, ID),
	node_type(File, Child_ID, _).
	
count_children(File, ID, N_Children) :-
	get_children(File, ID, Children),
	length(Children, N_Children).

sibling(File, ID, Sibling_ID) :-
	parent(ID, Parent_ID),
	child(File, Parent_ID, Sibling_ID),
	ID \= Sibling_ID.

next_sibling(File, ID, Next_Sibling_ID) :-
	split_id(ID, Pos, Parent_ID),
	Next is Pos + 1,
	split_id(Next_Sibling_ID, Next, Parent_ID),
	child(File, Parent_ID, Next_Sibling_ID).
	
last_sibling(File, ID, Last_Sibling_ID) :-
	parent(ID, Parent_ID),
	child(File, Parent_ID, Last_Sibling_ID),
	\+next_sibling(File, Last_Sibling_ID, _). 

/*
	Remaining = 1 -> Only current Element left. (Counting starts at 1).
*/	
count_remaining_siblings(File, ID, Remaining) :-
	split_id(ID, Pos0, Parent_ID),
	last_sibling(File, ID, Last_ID),
	split_id(Last_ID, Pos1, Parent_ID),
	Remaining is Pos1 - Pos0 + 1.

get_n_siblings(_File, _ID, 0, []).
get_n_siblings(_File, ID, 1, [ID]).
get_n_siblings(File, ID, N, [ID|Sibling_IDs]) :- 
	count_remaining_siblings(File, ID, Remaining),
	between(2, Remaining, N), 
	split_id(ID, Pos, Parent_ID),
	Pos0 is Pos + 1,
	N0 is N - 1,
	split_id(ID0, Pos0, Parent_ID),
	node_type(File, ID0, _),
	get_n_siblings(File, ID0, N0, Sibling_IDs).
	
	
/*
	Get nth sibling id `Nth_ID`, counted from given `ID`.
	Nth = 1 -> this id.
	(Counting starts at 1)
	Nth = 2 -> next sibling
*/
get_nth_sibling(D_File, ID, Nth, Nth_ID) :-
	split_id(ID, Pos, Parent_ID),
	N is Pos + Nth - 1,
	split_id(Nth_ID, N, Parent_ID),
	node_type(D_File, Nth_ID, _).


/*
	resolve_namespace/4
	resolve_namespace(File, ID, Prefix, URI)
	
	Resolves namespace `URI` for given `Prefix` inside Document `File` at position `id`
*/
resolve_namespace(File, ID, Prefix, URI) :-
	node(File, ns(Prefix, URI), _, ID),
	!.
resolve_namespace(File, ID, Prefix, URI) :-
	node_attribute(File, ID, xmlns:Prefix, URI),
	!.
resolve_namespace(File, ID, '', URI) :-
	node_attribute(File, ID, xmlns, URI),
	!.
resolve_namespace(File, ID, Prefix, URI) :-
	parent(ID, Parent_ID),
	resolve_namespace(File, Parent_ID, Prefix, URI).
	
/*
	namespace/3
	namespace(Name,Namespace,Name_Without_Namespace)
	
	Predicate to split a Name into its Namespace and 
	Name_Without_Namespace.
	
	Examples:
		namespace(xs:element,xs,element).
		namespace(element,'',element).
*/
namespace(Name,Namespace,Name_Without_NS) :-
	Name = Namespace:Name_Without_NS,
	!.
% Namespace in 'single:quotes'	
namespace(Name_String,Namespace,Name_Without_NS) :-
	atom(Name_String),
	atomic_list_concat([Namespace,Name_Without_NS], ':', Name_String),
	!.
% no namespace present
namespace(Name,'',Name).
