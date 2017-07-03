:- asserta(user:file_search_path(library, 'prolog')).
:- use_module(library(quasi_quotations)).

:- use_module(library(xsd)).
:- use_module(library(xsd/flatten)).
:- use_module(library(xsd/validate)).

% Support {|xsd|| ... |} quasi quotations
:- quasi_quotation_syntax(xml).
xml(Content, _Args, _Variables, Result) :-
   with_quasi_quotation_input(
      Content,
      Stream,
      flatten:xml_flatten(stream(Stream), File_ID)
   ),
   Result = File_ID.

/* Dynamic test generation */
:- dynamic test_definition/3.

term_expansion((Testname : Test_File_ID), (test_definition(Testname, Test_File_ID, success))) :-
   atom(Testname).
term_expansion((Test : Test_File_ID), (test_definition(Testname, Test_File_ID, fail))) :-
   Test =.. [Testname, fail], 
   atom(Testname).

term_expansion(run_tests, Tests) :-
   set_test_paths(_Path, Schema_Path, _Validation_Path),
   directory_files(Schema_Path, Schema_Filenames),
   findall(
      Test_Definitions,
      (
         member(Schema_Filename, Schema_Filenames),
         \+member(Schema_Filename, [., ..]),
         file_name_extension(Identifier, xsd, Schema_Filename),
         define_tests(Identifier, Test_Definitions)
      ),
      Nested_Test_Definitions
   ),
   flatten(Nested_Test_Definitions, Test_Definitions),
   maplist(define_tap_test, Test_Definitions, Tests),
   maplist(get_tap_test_name, Test_Definitions, Tap_Test_Names),
   forall(
      member(Tap_Test_Name, Tap_Test_Names),
      tap:register_test(Tap_Test_Name)
   ).

set_test_paths(Path, Schema_Path, Validation_Path) :-
   source_file_property(Main_Module_File_Location, module(xsd)),
   file_directory_name(Main_Module_File_Location, Main_Module_Path),
   absolute_file_name('../test', Path, [relative_to(Main_Module_Path), file_type(directory)]),
   absolute_file_name('./schema', Schema_Path, [relative_to(Path), file_type(directory)]),
   absolute_file_name('./validation', Validation_Path, [relative_to(Path), file_type(directory)]),
   assert(path(test, Path)),
   assert(path(schema, Schema_Path)),
   assert(path(validation, Validation_Path)).

define_tests(Identifier, Sub_Tests) :-
   path(schema, Path),
   absolute_file_name(Identifier, Absolute_Filename, [relative_to(Path),extensions([xsd])]),
   flatten:xml_flatten(Absolute_Filename, Identifier),
   define_sub_tests(Identifier, Sub_Tests).

define_sub_tests(Identifier, Sub_Tests) :-
   path(validation, Path),
   absolute_file_name(Identifier, Absolute_Filename, [relative_to(Path)]),
   load_files(Absolute_Filename, [module(Identifier)]),
   findall(
      test_definition(Identifier, Test_Name, Test_File_ID, Mode),
      retract(test_definition(Test_Name, Test_File_ID, Mode)),
      Sub_Tests
   ).

define_tap_test(Test_Definition, Tap_Test) :-
   get_tap_test_name(Test_Definition, Tap_Test_Name),
   get_tap_test_run(Test_Definition, Test_Run),
   Test = (
      % suppress output from validate/2
      % implicitly calls once/1, so no left choice points
      with_output_to(
         codes(_Output),
         Test_Run
      ),
      % remove all the tabled predicates
      validate:cleanup
   ),
   Tap_Test = (Tap_Test_Name :- Test).

get_tap_test_name(Test_Definition, Tap_Test_Name) :-
   Test_Definition = test_definition(Identifier, Test_Name, _Test_File_ID, _Mode),
   format(atom(Tap_Test_Name), '[~s] ~s', [Identifier, Test_Name]).

get_tap_test_run(
   test_definition(Identifier, _, Test_File_ID, success),
   validate:validate(Identifier, Test_File_ID)
).
get_tap_test_run(
   test_definition(Identifier, _, Test_File_ID, fail),
   (\+validate:validate(Identifier, Test_File_ID))
).


/* End of dynamic test generation */

% define tests below
:- use_module(library(tap)).

 % replaced via term expansion
run_tests.
