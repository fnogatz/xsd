:- module(flatten, 
	[
		xml_flatten/2, 
		node/4, 
		node_attribute/4, 
		text_node/3, 
		remove_file/1
	]).

:- use_module(library(sgml)).
:- use_module(library(xsd/xsd_helper)).

/*
	node/4
	node(File_ID,ID,Namespace,Node_Type_Without_NS)
	
	node_attribute/4
	node_attribute(File_ID,ID,Attribute,Value)
	
	text_node/3
	text_node(File_ID,ID,Node)
	
	file_id/1
	file_id(File_ID)
	
*/
:- dynamic node/4, node_attribute/4, text_node/3, file_id/1.


/*
	parse_options/1
	parse_options(List_Of_Options)
	
	Define a `List_Of_Options` for parse a XML file by use
		of the built-in `load_structure/3`.
*/
parse_options([
	% read in XML file with handling of namespaces in mind
	dialect(xmlns),

	% remove spaces before and after tags, ignore whitespace-only elements
	space(remove),
	
	% quiet namespace handling
	% if a node has a namespace (like `xs` in `xs:element`) which hasn't
	%	 been declared via `xmlns:xs="...URI..."` this will suppress the
	%	 error message
	%xml_no_ns(quiet),
	
	% create ns(xs, uri) 
	keep_prefix(true)
	]).


/**
	load_xml/2
	load_xml(Input,XML)
	
	Load a XML file specified by `Input` and return the
		DOM tree in `XML`.
	
	Examples:
		load_xml(stream(user_input),XML).	%% binds XML to the DOM tree
*/
load_xml(Input,XML) :- 
	parse_options(Parse_Options), 
	load_structure(Input,XML,Parse_Options).


/*
	xml_flatten/2
	xml_flatten(Input, File_ID)
	
	Load a XML file specified by `Input` and flatten
		its DOM tree. This will result in multiple `node/4`, `node_attribute/4`
		and `text_node/3` facts.
	`File_ID` can be user-defined, otherwise will be set to the next unused integer. 
	
	Examples:
		xml_flatten(string(input_as_atom),File_ID).
		xml_flatten(path_to_file,File_ID)
*/
xml_flatten(Input, File_ID) :- 
	load_xml(Input,XML),
	root_id(Root_ID),
	register_file_id(File_ID),
	xml_flatten_nodes(File_ID,Root_ID,0,XML),
	!.


/*
	root_id/1
	root_id(ID)
	
	Hold the default ID for the root element
*/
root_id([]).


/*
	new_id/3
	new_id(Parents_ID,Position,New_ID)
	
	Create a new ID by the parent's ID and the element's
	position.
*/
new_id(Base_ID,Pos,ID) :-
	ID = [Pos|Base_ID].


/*
	register_file_id/2
	register_file_id(+Input, ?File_ID)
	
	Register new File_ID (filename)
*/
register_file_id(File_ID) :-
	var(File_ID),
	% generate an (ascending) integer as ID ... (Starts at '1', corresponds to the TAP numbering) 
	length(_,File_ID),
	File_ID > 0,
	% ... which isn't used yet.
	\+file_id(File_ID),!,
	assertz(file_id(File_ID)).
register_file_id(File_ID) :-
	\+var(File_ID),
	% user chosen ID mustn't be already in use by another file
	(\+file_id(File_ID),!; throw("File_ID is already in use.")),
	assertz(file_id(File_ID)).


/*
	xml_flatten_nodes/4
	
	Flatten a XML DOM tree by creating `node/6`,
		`attribute/4`
		and `text_node/4` facts.
*/
xml_flatten_nodes(_File_ID,_Base_ID,_Pos,[]).

% ignore xsd-annotation nodes
xml_flatten_nodes(File_ID,Base_ID,Pos,[Node|Nodes]) :-
	Node = element(Node_Type,_Node_Attributes,_Child_Nodes),
	namespace(Node_Type,Namespace,Node_Type_Without_NS),
	% is annotation
	Namespace = ns(_, 'http://www.w3.org/2001/XMLSchema'),
	Node_Type_Without_NS = annotation, 
	!,
	% don't flatten this node, don't flatten child nodes
	% flatten sibling nodes
	xml_flatten_nodes(File_ID,Base_ID,Pos,Nodes).

xml_flatten_nodes(File_ID,Base_ID,Pos,[Node|Nodes]) :-
	Node = element(Node_Type,Node_Attributes,Child_Nodes),	%% is an XML node, no text
	new_id(Base_ID,Pos,ID),
	namespace(Node_Type,Namespace,Node_Type_Without_NS),
	% flatten the node's attributes
	xml_flatten_attributes(File_ID,ID,Node_Attributes),
	assertz(node(File_ID,ID,Namespace,Node_Type_Without_NS)),
	% flatten sibling nodes
	Next_Pos is Pos+1,
	xml_flatten_nodes(File_ID,Base_ID,Next_Pos,Nodes),
	% flatten all children
	xml_flatten_nodes(File_ID,ID,0,Child_Nodes).

xml_flatten_nodes(File_ID,Base_ID,Pos,[Node|Nodes]) :-
	atom(Node),	%% is simply a text node
	new_id(Base_ID,Pos,ID),
	assertz(text_node(File_ID,ID,Node)),
	% flatten sibling nodes
	Next_Pos is Pos+1,
	xml_flatten_nodes(File_ID,Base_ID,Next_Pos,Nodes).


/*
	xml_flatten_attributes/3
	xml_flatten_attributes(File_ID,ID,List_Of_Attributes)
	
	Flatten a `List_Of_Attributes` of the form
		[attribute1=valu1,attribute2=value2,...]
		by creating a `node_attribute/4` facts.
	
	Examples:
		xml_flatten_attributes(filename,[0],[minOccurs='1'])
			==> node_attribute(filename,[0],minOccurs,'1')
*/
xml_flatten_attributes(_File_ID,_ID,[]).
xml_flatten_attributes(File_ID,ID,[Attribute=Value|List_Of_Attributes]) :-
	assertz(node_attribute(File_ID,ID,Attribute,Value)),
	xml_flatten_attributes(File_ID,ID,List_Of_Attributes).
	
/**
	remove_file/1
	remove_file(File_ID)
	
	Deletes asserted node/5, node_attribute/4, text_node/3 for given File_ID from database. 
	Deletes nodes for every asserted file, if File_ID left empty.
*/
remove_file(File_ID) :-
	retractall(node(File_ID,_,_,_)),
	retractall(text_node(File_ID,_,_)),
	retractall(node_attribute(File_ID,_,_,_)),
	retractall(file_id(File_ID)).
