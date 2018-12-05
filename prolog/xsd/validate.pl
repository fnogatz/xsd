:- module(validate, [
		validate/2,
		validate/3,
		xsd_table/2,
		cleanup/0
	]).

:- use_module(library(xsd/xsd_helper)).
:- use_module(library(xsd/simpletype)).
:- use_module(library(xsd/flatten)).

:- use_module(library(statistics)).
:- use_module(library(settings)).
:- setting('without-tabling', boolean, false, 'Run without tabling').
:- setting(profile, boolean, false, 'Show profile').


/*
	validate_tabled/6
	
	Tabling for validate/5
	Checks, wether validate/6 was previously called with the given parameters, 
	then the result is already saved as a xsd_table/2 fact. 
	otherwise, validate/6 is called and the result is saved to avoid double calculations
*/
:- dynamic xsd_table/2.
validate_tabled(D_File, D_ID, Validated_Nodes, S_File, S_ID) :-
	setting('without-tabling', true),
	!,
	validate(D_File, D_ID, Validated_Nodes, S_File, S_ID).

validate_tabled(D_File, D_ID, Validated_Nodes, S_File, S_ID) :-
	(xsd_table(validate(D_File, D_ID, Validated_Nodes, S_File, S_ID), Valid) ->
		!, call(Valid)
	;
		(validate(D_File, D_ID, Validated_Nodes, S_File, S_ID) ->
			asserta(xsd_table(validate(D_File, D_ID, Validated_Nodes, S_File, S_ID), true))
		;
			asserta(xsd_table(validate(D_File, D_ID, Validated_Nodes, S_File, S_ID), false)),
			!,
			false
		)
	).
cleanup :-
	retractall(xsd_table(_,_)).

/*
	validate/2
	Validates given XML document `D_File` against XSD schema `S_File`. 
	(Both files must be loaded using `flatten_xml/2`)
	
	?- validate(xml_file, xsd_file).
*/
validate(S_File, D_File) :-
	validate(S_File, D_File, []).

validate(S_File, D_File, Options) :-
	set_options(Options),
	(setting(profile, true) ->
		time(validate_tabled(D_File, [0], 1, S_File, [0]))
	;	validate_tabled(D_File, [0], 1, S_File, [0])),
%	validate_tabled(D_File, [0], 1, S_File, [0]),
	% only one solution
	!,
	set_default_options(Options).

set_options(Options) :-
	maplist(set_option, Options),
	!.

set_option(Option) :-
	Option =.. [Key, Value],
	current_setting(Key),
	set_setting(Key, Value).
set_option(_Option) :-
	true.

set_default_options(Options) :-
	maplist(set_default_option, Options),
	!.
set_default_option(Option) :-
	Option =.. [Key, _Value],
	current_setting(Key),
	restore_setting(Key).
set_default_option(_Option) :-
	true.

validate(D_File, D_ID, Validated_Nodes, S_File, S_ID) :-
	% schema
	Validated_Nodes = 1,
	node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), schema),
	% validate: D_ID and one child 'element' in schema
	child(S_File, S_ID, S_Child_ID),
	node(S_File, S_Child_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), element),
	validate_tabled(D_File, D_ID, 1, S_File, S_Child_ID)
	;
	% ref
	% Resolves references to other elements in the document using the attribute ref
	attribute(S_File, S_ID, ref, S_QName),
	
	attribute(S_File, S_ID0, name, S_QName),
	validate_tabled(D_File, D_ID, Validated_Nodes, S_File, S_ID0)
	;
	node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), element), 
	(
		% minOccurs = 0
		Validated_Nodes = 0,
		attribute(S_File, S_ID, minOccurs, '0')
		;
		% minOccurs =< # of elements =< maxOccurs
		% Min/MaxOccurs
		attribute(S_File, S_ID, minOccurs, MinOccurs),
		atom_number(MinOccurs, Min),
		attribute(S_File, S_ID, maxOccurs, MaxOccurs),
		(
		MaxOccurs = unbounded ->
			count_remaining_siblings(D_File, D_ID, Max)
			;
			atom_number(MaxOccurs, Max)
		),
		% Validated_Nodes in Range
		between(Min, Max, Validated_Nodes),
		% Validate each Element Node
		forall(between(1, Validated_Nodes, Nth), 
			(get_nth_sibling(D_File, D_ID, Nth, Nth_ID), validate_element(D_File, Nth_ID, S_File, S_ID)))
	)
	;
	% complexType
	Validated_Nodes = 1,
	node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), complexType),
	(
		child(S_File, S_ID, S_Type_ID),
		node(S_File, S_Type_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), S_Type),
		
		member(S_Type, [sequence, choice, all]),
		validate_all_attributes(D_File, D_ID, S_File, S_ID),
		
		count_children(D_File, D_ID, N_Children),
		(
		N_Children = 0 ->
			% no children -> validate schema against no element (equals non-existing element -> [1])
			validate_tabled(D_File, [1], 0, S_File, S_Type_ID)
		;
			% validate all N_Children otherwise
			get_nth_child(D_File, D_ID, 1, Child_ID),
			validate_tabled(D_File, Child_ID, N_Children, S_File, S_Type_ID)
		)
		;
		% empty complexType (except attributes)
		% no content defining children
		forall((child(S_File, S_ID, S_Type_ID), member(S_Type, [sequence, choice, all])),
			\+node(S_File, S_Type_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), S_Type)),
		
		% no children in document
		count_children(D_File, D_ID, 0),
		validate_all_attributes(D_File, D_ID, S_File, S_ID)
		
	)
	;
	% simpleType as content of nodes (actual validation of content or attribute values handled by `validate_simpleType/3`)
	Validated_Nodes = 1,
	node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), simpleType),
	
	% no attributes in xml file, 1 or 0 children
	\+attribute(D_File, D_ID, _Name, _Value), 
	(
		count_children(D_File, D_ID, 1),
		child(D_File, D_ID, D_Child), 
		text_node(D_File, D_Child, D_Text)
		;
		count_children(D_File, D_ID, 0),
		D_Text = ''
	),
	validate_simpleType(D_Text, S_File, S_ID)
	;
	% sequence / choice
	get_n_siblings(D_File, D_ID, Validated_Nodes, D_Nodes),
	get_children(S_File, S_ID, S_Children),
	% Min/MaxOccurs
	attribute(S_File, S_ID, minOccurs, MinOccurs),
	atom_number(MinOccurs, Min),
	attribute(S_File, S_ID, maxOccurs, MaxOccurs),
	(
	MaxOccurs = unbounded ->
		count_remaining_siblings(D_File, D_ID, Max)
		;
		atom_number(MaxOccurs, Max)
	),
	(
		% sequence
		node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), sequence),
		
		validate_sequence(D_File, D_Nodes, S_File, S_Children, S_Children, Min, Max)
		;
		% choice
		node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), choice),
		
		validate_choice(D_File, D_Nodes, S_File, S_Children, Min, Max)
	)
	;
	% all
	node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), all), 
	attribute(S_File, S_ID, maxOccurs, '1'),
	get_children(S_File, S_ID, S_IDs), 
	count_remaining_siblings(D_File, D_ID, Validated_Nodes),
	get_n_siblings(D_File, D_ID, Validated_Nodes, D_IDs),
	validate_all(D_File, D_IDs, S_File, S_IDs);
	Validated_Nodes = 0,
	node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), all), 
	attribute(S_File, S_ID, minOccurs, '0')
	.

/*
	#### (End of validate/5) ####
*/	

/*
	attribute
*/
validate_all_attributes(D_File, D_ID, S_File, S_ID) :-
	get_children(S_File, S_ID, S_Children), 
	findall(S_Child, 
		(member(S_Child, S_Children), node(S_File, S_Child, ns(_, 'http://www.w3.org/2001/XMLSchema'), attribute)),
		S_Attribute_IDs),
	findall(attribute(D_File, D_ID, Name, Value), 
		attribute(D_File, D_ID, Name, Value), 
		D_Attribute_List),
	validate_attributes(D_File, D_Attribute_List, S_File, S_Attribute_IDs).

/* 
	validate_element/4
	validate_element(D_File, D_ID, S_File, S_ID)
	
	Validates a single element `D_ID` from file `D_File` against schema node `S_ID` from `S_File`.
	
	Is called by validate(element,_,_,_,_,_). (min/maxOccurs is handled there)
	
*/

validate_element(D_File, D_ID, S_File, S_ID) :-
	validate_element_name(D_File, D_ID, S_File, S_ID),
	(
		get_children(S_File, S_ID, []),
		(
			% no type
			\+attribute(S_File, S_ID, type, _),
			\+attribute(S_File, S_ID, ref, _),
			
			get_children(D_File, D_ID, []),
			\+attribute(D_File, D_ID, _Name, _Value)
			;
			% xsd simple Type
			% no child nodes in Schema
			attribute(S_File, S_ID, type, S_Type_NameNS),
			namespace(S_Type_NameNS, NS_Prefix, S_Type_Name),
			resolve_namespace(S_File, S_ID, NS_Prefix, 'http://www.w3.org/2001/XMLSchema'),
			(
				% one child node in Document (is text node with ID = Child_Node)
				get_children(D_File, D_ID, [Child_Node]),
				text_node(D_File, Child_Node, D_Text)
				;
				% otherwise: no children
				get_children(D_File, D_ID, []),
				D_Text = ''
			),
			\+attribute(D_File, D_ID, _Name1, _Value1),
			validate_xsd_simpleType(S_Type_Name, D_Text)
		)
		;
		% xsd simple Type
		(
			% nested type: type definition in child node
			child(S_File, S_ID, S_Type_ID)
			;
			% global type: type definition anywhere in document
			attribute(S_File, S_ID, type, S_User_Type), 
			attribute(S_File, S_Type_ID, name, S_User_Type)
		), 
		% S_Type: [complexType, simpleType]
		node(S_File, S_Type_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), S_Type),
		member(S_Type, [complexType, simpleType]),
		validate_tabled(D_File, D_ID, 1, S_File, S_Type_ID)
	)
	.

/*
	validate_element_name/4
	
	Validates name of `D_ID` against specified name in `S_ID`
*/
validate_element_name(D_File, D_ID, S_File, S_ID) :-
	% check Name (currently: ignoring namespaces; TODO (?))
	node(D_File, D_ID, _D_NS, D_Name),
	attribute(S_File, S_ID, name, D_Name).

/*
	validate_simpleType
*/
% find simpleType by name `S_Type` and validate (S_ID only for namespace handling)


validate_simpleType(D_Text, S_File, S_ID, S_Type) :-
	node(S_File, S_ID0, ns(_, 'http://www.w3.org/2001/XMLSchema'), simpleType),
	attribute(S_File, S_ID0, name, S_Type),
	validate_simpleType(D_Text, S_File, S_ID0)
	;
	namespace(S_Type, NS_Prefix, S_Type_Name),
	resolve_namespace(S_File, S_ID, NS_Prefix, 'http://www.w3.org/2001/XMLSchema'),
	validate_xsd_simpleType(S_Type_Name, D_Text)
	. 

validate_simpleType(D_Text, S_File, S_ID) :-
	child(S_File, S_ID, S_Child),
	(
		% restriction
		node(S_File, S_Child, ns(_, 'http://www.w3.org/2001/XMLSchema'), restriction),
		
		attribute(S_File, S_Child, base, S_Type),
		validate_simpleType(D_Text, S_File, S_ID, S_Type),
		
		get_children(S_File, S_Child, S_Facets), 
		validate_restriction(D_Text, S_File, S_Facets)
		;
		node(S_File, S_Child, ns(_, 'http://www.w3.org/2001/XMLSchema'), union),
		(
			% types as memberTypes-attribute
			attribute(S_File, S_Child, memberTypes, S_Types), 
			atomic_list_concat(Types_List, ' ', S_Types),
			member(S_Type, Types_List),
			validate_simpleType(D_Text, S_File, S_ID, S_Type)
			;
			% types as children
			get_children(S_File, S_Child, S_SimpleTypes), 
			member(S_SimpleType, S_SimpleTypes),
			node(S_File, S_SimpleType, ns(_, 'http://www.w3.org/2001/XMLSchema'), simpleType),
			validate_simpleType(D_Text, S_File, S_SimpleType)
		)
		;
		node(S_File, S_Child, ns(_, 'http://www.w3.org/2001/XMLSchema'), list),
		(
			% type as itemType-attribute
			attribute(S_File, S_Child, itemType, S_Type),
			atomic_list_concat(D_Items, ' ', D_Text),
			subtract(D_Items, [''], D_Items0),
			forall(member(D_Item, D_Items0), validate_simpleType(D_Item, S_File, S_ID, S_Type))
			;
			% type as child
			child(S_File, S_Child, S_SimpleType), 
			node(S_File, S_SimpleType, ns(_, 'http://www.w3.org/2001/XMLSchema'), simpleType),
			atomic_list_concat(D_Items, ' ', D_Text),
			subtract(D_Items, [''], D_Items0),
			forall(member(D_Item, D_Items0), validate_simpleType(D_Item, S_File, S_SimpleType))
		)
	)
	. 

/*
	validate_restriction
*/
validate_restriction(D_Text, S_File, SIDs) :-
	SIDs = []
	;
	SIDs = [S_ID|S_IDs],
	(
		% min/max facets
		node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), Facet), 
		attribute(S_File, S_ID, value, Val),
		facet(Facet, Val, D_Text),
		validate_restriction(D_Text, S_File, S_IDs)
		;
		% enumeration
		findall(S_ID0, 
			(member(S_ID0, [S_ID|S_IDs]), node(S_File, S_ID0, ns(_, 'http://www.w3.org/2001/XMLSchema'), enumeration)),
			Enum_IDs),
		member(Enum_ID, Enum_IDs), 
		attribute(S_File, Enum_ID, value, D_Text),
		
		subtract([S_ID|S_IDs], Enum_IDs, S_IDs0),
		validate_restriction(D_Text, S_File, S_IDs0)
	)
	.

/*
	validate_sequence/6
	validate_sequence(D_File, D_Remaining_IDs, S_File, S_Remaining_IDs, S_IDs, MinOccurs, MaxOccurs)
	
	Validates a list of document nodes `D_Remaining_IDs` against a list of schema nodes `S_Remaining_IDs`.
	There must be a corresponding number of document nodes for each schema node in the given order.
	
	
	Is called by validate(sequence,_,_,_,_,_)
*/

validate_sequence(D_File, D_IDs, S_File, S_Remaining_IDs, S_IDs, Min, Max) :-
	D_IDs = [],
	(
		% minOccurs = 0 / end of recursion
		member(S_Remaining_IDs, [[], S_IDs]),
		Min =< 0,
		Max >= 0
		;
		% empty sequence -> every element in sequence validates against zero elements
		forall(member(S_ID, S_Remaining_IDs), (node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), S_Type), validate_tabled(D_File, [], 0, S_File, S_ID))),
		(
			Min =< 1
			;
			forall(member(S_ID, S_IDs), (node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), S_Type), validate_tabled(D_File, null, 0, S_File, S_ID)))
		)
	)
	;
	S_Remaining_IDs = [S_ID|S_Remaining_IDs0],
	Max > 0, 
	D_IDs = [D_ID|_],
	node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), S_Type),
	member(S_Type, [element, sequence, choice]),
	length(D_IDs, D_Remaining),
	between(0, D_Remaining, Val_Nodes),
	validate_tabled(D_File, D_ID, Val_Nodes, S_File, S_ID),
	length(TempList, Val_Nodes),
	append(TempList, D_IDs0, D_IDs),
	(
		S_Remaining_IDs0 = [],
		% reset S_Remaining_IDs and validate next sequence
		Min0 is Min - 1,
		Max0 is Max - 1,
		validate_sequence(D_File, D_IDs0, S_File, S_IDs, S_IDs, Min0, Max0)
		;
		% Validate `Val_Nodes` many document nodes against next schema node
		S_Remaining_IDs0 \= [],
		validate_sequence(D_File, D_IDs0, S_File, S_Remaining_IDs0, S_IDs, Min, Max)
	)
	.

/*
	validate_choice/7
	validate_choice(D_File, D_Nodes, S_File, S_Children, S_Children, Min, Max)
*/
validate_choice(D_File, D_IDs, S_File, S_IDs, Min, Max) :-
	D_IDs = [],
	(
		% end of recursion & minOccurs = 0 
		Min =< 0,
		Max >= 0
		;
		% empty choice declaration
		S_IDs = []
		;
		% no document nodes -> one element in choice validates against zero elements in document
		member(S_ID, S_IDs),
		node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), S_Type),
		member(S_Type, [element, choice, sequence]),
		validate_tabled(D_File, [1], 0, S_File, S_ID)
	)
	;
	Max > 0,
	% Validate `Val_Nodes` many document nodes against schema node
	D_IDs = [D_ID|_],
	member(S_ID, S_IDs),
	node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), S_Type),
	member(S_Type, [element, choice, sequence]),
	length(D_IDs, D_Remaining),
	between(0, D_Remaining, Val_Nodes),
	validate_tabled(D_File, D_ID, Val_Nodes, S_File, S_ID),
	length(TempList, Val_Nodes),
	append(TempList, D_IDs0, D_IDs),
	
	Min0 is Min - 1,
	Max0 is Max - 1,
	validate_choice(D_File, D_IDs0, S_File, S_IDs, Min0, Max0)
	.

/*
	validate_all/4
	validate_sequence(D_File, D_IDs, S_File, S_IDs)
	
	Validates a list of document nodes `D_IDs` against a list of schema nodes `S_IDs`. 
	Without regard to the order, each given document node must validate against one schema node. 
*/
validate_all(D_File, DIDs, S_File, S_IDs) :-
	DIDs = [],
	(
	S_IDs = []
	;
	forall(member(S_ID, S_IDs), (node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), _S_Type), validate_tabled(D_File, null, 0, S_File, S_ID)))
	)
	;
	DIDs = [D_ID|D_IDs],
	member(S_ID, S_IDs),
	node(S_File, S_ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), element),
	validate_tabled(D_File, D_ID, 1, S_File, S_ID),
	delete(S_IDs, S_ID, S_IDs0),
	validate_all(D_File, D_IDs, S_File, S_IDs0)
	.

/*
	validate_attributes/4
	validate_attributes(D_File, D_Attribute_List, S_File, S_Attribute_IDs)
	
	`D_Attribute_List`: List of `attribute/4` nodes.
	`S_Attribute_IDs`: List of IDs correspoding to <xs:attribute .. /> nodes
*/
validate_attributes(D_File, DAttributes, S_File, S_Attribute_IDs) :-
	DAttributes = [],
	(
		S_Attribute_IDs = []
		;
		forall(member(S_ID, S_Attribute_IDs), 
			\+attribute(S_File, S_ID, use, 'required'))
	)
	;
	DAttributes = [D_Attribute|D_Attributes],
	D_Attribute = attribute(D_File, _D_ID, Name, _Value),
	(
	
		% skip xmlns attributes
		( Name = xmlns ; Name = xmlns:_ ),
	
		validate_attributes(D_File, D_Attributes, S_File, S_Attribute_IDs)
		;
		member(S_ID, S_Attribute_IDs),
		attribute(S_File, S_ID, name, Name),
		\+attribute(S_File, S_ID, use, 'prohibited'),
	
		validate_attribute(D_Attribute, S_File, S_ID),
	
		delete(S_Attribute_IDs, S_ID, S_Attribute_IDs0),
		validate_attributes(D_File, D_Attributes, S_File, S_Attribute_IDs0)
	)
	. 

validate_attribute(D_Attribute, S_File, S_ID) :-
	D_Attribute = attribute(_D_File, _D_ID, _Name, D_Value),
	% check fixed values
	(attribute(S_File, S_ID, fixed, S_FixedVal) ->
		S_FixedVal = D_Value
		;
		true
	),
	% validate simpleType (reference or nested)
	(
		attribute(S_File, S_ID, type, S_Type),
		validate_simpleType(D_Value, S_File, S_ID, S_Type)
	;
		child(S_File, S_ID, S_Child),
		validate_simpleType(D_Value, S_File, S_Child)
	).

/*
	attribute/4
	attribute(File_ID, ID, Attribute_Name, Value)
	
	Determines the attribute value `Value` of given `File_ID`, `ID` and `Attribute_Name` using `node_attribute/4`. 
		Specifies all permitted attributes, including default ones, if `Attribute_Name` is left empty.
	
	Currently, only some (minOccurs, maxOccurs, use) XML-Schema-Defaults are supported.
*/
attribute(File_ID, ID, Attribute_Name, Value) :-
	node_attribute(File_ID, ID, Attribute_Name, Value),
	Attribute_Name \= _NS:_Name.

% XML-Schema Defaults
attribute(File_ID, ID, minOccurs, '1') :-
	\+node_attribute(File_ID, ID, minOccurs, _),
	% Check type and namespace
	member(Element_Type, [element, choice, sequence, all]),
	node(File_ID, ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), Element_Type).
	
attribute(File_ID, ID, maxOccurs, '1') :-
	\+node_attribute(File_ID, ID, maxOccurs, _),
	% Check type and namespace
	member(Element_Type, [element, choice, sequence, all]),
	node(File_ID, ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), Element_Type).
	
attribute(File_ID, ID, use, 'optional') :-
	\+node_attribute(File_ID, ID, use, _), 
	% Check type and namespace
	member(Element_Type, [attribute]),
	node(File_ID, ID, ns(_, 'http://www.w3.org/2001/XMLSchema'), Element_Type).