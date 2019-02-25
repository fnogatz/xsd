:- module(op_numerics, [
      user:('==>')/2
   ]).

% numeric-add
numeric-add(3, 2) ==> data('decimal', [5]).
true + 1.5 ==> false.
1.5 + 'NaN' ==> data('float', [nan]).
'NaN' + 1.5 ==> data('float', [nan]).
'INF' + -3 ==> data('float', [inf]).
4.5 + '-INF' ==> data('float', [-inf]).
'INF' + 'INF' ==> data('double', [inf]).
'-INF' + 'INF' ==> data('double', [nan]).
-1 + 5 ==> data('decimal', [4]).
1.2 + 1.8 ==> data('float', [3.0]).
1.4 + 1 ==> data('float', [2.4]).
