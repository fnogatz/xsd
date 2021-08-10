:- use_module(library(quasi_quotations)).
:- use_module(library(xsd)).
:- use_module(library(xsd/flatten)).
:- use_module(library(xsd/validate)).
:- use_module(library(xsd/xpath)).

% operator for xpath expression unit tests
:- op(800, xfx, user:(==>)).
% suppress all warnings for ungrouped clause definitions
:- style_check(-discontiguous).


/* --- Term Expansion --- */

% all consulted tests are temporarily stored as a test_definition/3 during term expansion
:- dynamic test_definition/3.

/**
 * {|xsd|| ... |} quasi quotation support for embedding xml instances in pl files.
 * The embedded instance is flattened and the file id is returned.
 */
:- quasi_quotation_syntax(xml).
xml(Content, _Args, _Variables, Result) :-
  with_quasi_quotation_input(
    Content,
    Stream,
    flatten:xml_flatten(stream(Stream), File_ID)
  ),
  Result = File_ID.

/**
 * Asserts the test_definition that represents the read schema validation test definition.
 * 
 * e.g.:
 * 'xs:float valid':
 * {|xml||
 *  <float>NaN</float>
 * |}.
 * or
 * 'xs:float invalid'(fail):
 * {|xml||
 *  <float>NAN</float>
 * |}.
 */
term_expansion((Testname : Test_File_ID), (test_definition(Testname, Test_File_ID, success))) :-
  atom(Testname).
term_expansion((Test : Test_File_ID), (test_definition(Testname, Test_File_ID, fail))) :-
  Test =.. [Testname, fail], 
  atom(Testname).

/**
 * Asserts the test_definition that represents the read xpath unit test definition.
 * 
 * e.g.:
 * float('NaN') ==> data('float', [nan])
 * or
 * float('NAN') ==> false
 */ 
term_expansion((Input ==> Output), (test_definition(Input, (Input ==> Output), success))) :-
  Output \= false,
  % automatically asserting the 2nd argument of term_expansion does not work
  assertz(test_definition(Input, (Input ==> Output), success)).
term_expansion((Input ==> Output), (test_definition(Input, (Input ==> Output), fail))) :-
  Output = false,
  % automatically asserting the 2nd argument of term_expansion does not work
  assertz(test_definition(Input, (Input ==> Output), fail)).

/**
 * Executes this test runner by replacing the run(tests) fact with the test definitions,
 * registering the tests with tap and finally executing them.
 */
term_expansion(run(tests), TermReplacement) :-
  get_test_paths(Schema_Path, Validation_Path, XPath_Path),
  load_tests(Schema_Path, Validation_Path, Schema_Validation_Tests),
  load_tests(XPath_Path, XPath_Tests),
  append(XPath_Tests, Schema_Validation_Tests, UnsortedTests),
  sort(UnsortedTests, SortedTests),
  execute_tests(SortedTests, TermReplacement).


/* --- Test Loading and Execution --- */

/**
 * Get the paths to the schema, validation and xpath directory.
 */
get_test_paths(Schema_Path, Validation_Path, XPath_Path) :-
  source_file_property(Main_Module_File_Location, module(xsd)),
  file_directory_name(Main_Module_File_Location, Main_Module_Path),
  absolute_file_name('../test', Path, [relative_to(Main_Module_Path), file_type(directory)]),
  absolute_file_name('./schema', Schema_Path, [relative_to(Path), file_type(directory)]),
  absolute_file_name('./validation', Validation_Path, [relative_to(Path), file_type(directory)]),
  absolute_file_name('./xpath', XPath_Path, [relative_to(Path), file_type(directory)]).

/**
 * Translates the provided identifiers between local and global mode.
 * 
 * e.g.: <test> in the <schema> directory is identified as <schema:test>,
 * whereas <test> in the <xpath> directory is identified as <xpath:test>.
 */
path_translation(Directory, LocalIdentifier, GlobalIdentifier) :-
  atomic_list_concat([Directory, LocalIdentifier], ':', GlobalIdentifier).

/**
 * Returns the directory name of the provided directory path.
 */
directory_name(DirectoryPath, DirectoryName) :-
  atomic_list_concat(DirectoryPathParts, '/', DirectoryPath),
  reverse(DirectoryPathParts, [DirectoryName|_]).

/**
 * Loads the test definitions of files inside the provided path(s).
 * 
 * If the tests require no xml schema and instance, only the primary path, 
 * that contains test definitions inside .pl-files, must be provided.
 * 
 * If the tests are based on an xml schema and instance, then the primary path
 * references the schema directory, that contains .xsd-files, and the secondary 
 * path references the directory, that contains test definitions inside .pl-files.
 */
load_tests(PrimaryPath, Tests) :-
  load_tests(PrimaryPath, null, Tests).
load_tests(PrimaryPath, SecondaryPath, Tests) :-
  directory_files(PrimaryPath, Filenames),
  load_test_file(Filenames, PrimaryPath, SecondaryPath, Tests).

load_test_file([], _, _, []).
load_test_file([Filename|Filenames], PrimaryPath, SecondaryPath, AllTests) :-
  load_test_file(Filenames, PrimaryPath, SecondaryPath, OtherTests),
  (
    \+member(Filename, [., ..]),
    file_name_extension(LocalIdentifier, Extension, Filename),
    absolute_file_name(LocalIdentifier, Absolute_Filename, [relative_to(PrimaryPath), extensions([Extension])]),
    directory_name(PrimaryPath, Directory),
    (
      Extension = pl,
      ensure_loaded(Absolute_Filename) % term expansions are triggered here for schema/instance-independent tests
      ;
      Extension = xsd,
      flatten:xml_flatten(Absolute_Filename, LocalIdentifier),
      absolute_file_name(LocalIdentifier, Instance_Absolute_Filename, [relative_to(SecondaryPath), extensions([pl])]),
      ensure_loaded(Instance_Absolute_Filename) % term expansions are triggered here for schema/instance-dependent tests
    ),
    findall(
      test_definition(GlobalIdentifier, Test_Name, Test_Definition, Mode),
      (
        test_definition(Test_Name, Test_Definition, Mode),
        path_translation(Directory, LocalIdentifier, GlobalIdentifier)
      ),
      LoadedTests  
    ),
    append(OtherTests, LoadedTests, AllTests),
    retractall(test_definition(_, _, _))
    ;
    AllTests = OtherTests
  ).

/**
 * Executes the provided test definitions by generating and returning the test rules as well as registering them with tap.
 */
execute_tests([], []).
execute_tests([test_definition(GlobalIdentifier, Test_Name, Test_Definition, Mode)|Tests], [(Tap_Test_Name :- Test)|TermReplacement]) :-
  format(atom(Tap_Test_Name), '[~w] ~w', [GlobalIdentifier, Test_Name]),
  (
    compound(Test_Definition), % schema/instance independent tests
    Test_Definition = (IN ==> OUT),
    (
      Mode = success,
      Test_Run = (xpath:xpath_expr(_, IN, OUT))
      ;
      Mode = fail,
      Test_Run = (\+xpath:xpath_expr(_, IN, _))  
    )
    ;
    \+compound(Test_Definition), % schema/instance dependent tests
    path_translation(_, LocalIdentifier, GlobalIdentifier),
    (
      Mode = success,
      Test_Run = (validate:validate(LocalIdentifier, Test_Definition))
      ;
      Mode = fail,
      Test_Run = (\+validate:validate(LocalIdentifier, Test_Definition))
    )
  ),
  Test = (
    % suppress output during test run
    with_output_to(
      codes(_Output),
      Test_Run
    )
  ),
  tap:register_test(Tap_Test_Name),
  execute_tests(Tests, TermReplacement).


/* --- Testrunner entry point --- */

:- use_module(library(tap)).
run(tests).
