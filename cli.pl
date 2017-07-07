:- use_module(library(xsd)).

opts_spec([
  [
    opt(help),
    type(boolean),
    default(false),
    shortflags([ h ]),
    longflags([ 'help' ]),
    help([
      'display this help'
    ])
  ],
  [
    opt(without_tabling),
    type(boolean),
    default(false),
    longflags([ 'without-tabling ']),
    help([
      'Run validation without tabling'
    ])
  ],
  [
    opt(profile),
    type(boolean),
    default(false),
    longflags([ 'profile' ]),
    help([
      'Print profiling information'
    ])
  ]
]).

main :-
  opts_spec(OptsSpec),
  opt_arguments(OptsSpec,Opts,PositionalArgs),
  main(Opts,PositionalArgs).

main(Opts,PositionalArgs) :-
  (
    memberchk(help(true),Opts)
  ;
    PositionalArgs = []
  ), !,
  opts_spec(OptsSpec),
  opt_help(OptsSpec,Help),
  writeln('USAGE: xsdpl [options] <xsd-path> <xml-path>'), nl,
  writeln('Validate a XML Document against XML Schema'), nl,
  writeln('Options:'),
  writeln(Help),
  halt(0).

main(Opts,PositionalArgs) :-
  PositionalArgs = [Xsd, Xml],
  ( xsd_validate(Xsd, Xml, Opts) ->
    success
  ; no_success ).

main(_,_) :- halt(1).

success :-
  writeln('Valid'),
  halt(0).

no_success :-
  writeln('Not valid'),
  halt(1).
