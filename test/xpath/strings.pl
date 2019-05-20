:- module(strings, [
      user:('==>')/2
   ]).

% concat
concat('pre', 'fix') ==> data('string', ['prefix']).
% emtpy sequence () cannot be handled yet:
% concat('pre', (), 'fix') ==> data('string', ['prefix']).
% concat((), 'suf', 'fix') ==> data('string', ['suffix']).
concat('post', 'fix', 'operator') ==> data('string', ['postfixoperator']).
concat(token('a'), long('3')) ==> data('string', ['a3']).

% matches
matches('abracadabra', 'bra') ==> data('boolean', [true]).
matches('abracadabra', '^a.*a$') ==> data('boolean', [true]).
matches('abracadabra', '^bra') ==> data('boolean', [false]).
matches('abracadabra', 'ABRA') ==> data('boolean', [false]).
matches('abracadabra', 'ABRA', 'i') ==> data('boolean', [true]).