:- use_module(library(xsd)).

/*
	A simple example to show the difference in the calculation speed when using tabling.
	
	Usage:
	load_example.
	run_example.

*/

run_example :- 

	writeln('-- Without Tabling--'),
	xsd:validate:set_setting(use_tabling,false),
	retractall(validate:xsd_table(_,_)),
	
	writeln('choice_basic'),
	time(xsd_validate('choice_basic.xsd','choice_basic.xml')),
	
	writeln('choice_minmax (fail)'),
	\+time(xsd_validate('choice_minmax.xsd','choice_minmax.xml')),
	
	writeln('sequence_minmax'),
	time(xsd_validate('sequence_minmax.xsd','sequence_minmax.xml')),
	
	
	writeln('\n-- With Tabling--'),
	xsd:validate:set_setting(use_tabling,true),
	retractall(validate:xsd_table(_,_)),
	
	writeln('choice_basic'),
	time(xsd_validate('choice_basic.xsd','choice_basic.xml')),
	
	writeln('choice_minmax (fail)'),
	\+time(xsd_validate('choice_minmax.xsd','choice_minmax.xml')),
	
	writeln('sequence_minmax'),
	time(xsd_validate('sequence_minmax.xsd','sequence_minmax.xml')),
	
	!.
	

load_example :- 
	xsd:validate:xml_flatten('choice_basic.xml', 'choice_basic.xml'),
	xsd:validate:xml_flatten('choice_basic.xsd', 'choice_basic.xsd'),

	xsd:validate:xml_flatten('choice_minmax.xml', 'choice_minmax.xml'),
	xsd:validate:xml_flatten('choice_minmax.xsd', 'choice_minmax.xsd'),

	xsd:validate:xml_flatten('sequence_minmax.xml', 'sequence_minmax.xml'),
	xsd:validate:xml_flatten('sequence_minmax.xsd', 'sequence_minmax.xsd').