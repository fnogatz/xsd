:- module(op_numerics, [
      user:('==>')/2
   ]).

% numeric-add
numeric-add(3, 2) ==> data('decimal', [5]).
1.5 + 'NaN' ==> data('float', [nan]).
'NaN' + 1.5 ==> data('float', [nan]).
'INF' + -3 ==> data('float', [inf]).
-3 + 'INF' ==> data('float', [inf]).
'-INF' + 3 ==> data('float', [-inf]).
4.5 + '-INF' ==> data('float', [-inf]).
'INF' + 'INF' ==> data('double', [inf]).
'-INF' + '-INF' ==> data('double', [-inf]).
'-INF' + 'INF' ==> data('double', [nan]).
'INF' + '-INF' ==> data('double', [nan]).
-1 + 5 ==> data('decimal', [4]).
1.2 + 1.8 ==> data('float', [3.0]).
1.4 + 1 ==> data('float', [2.4]).
1.5 + 1.5 + 1.5 ==> data('float', [4.5]).
2.4 + 1.6 + 'NaN' ==> data('float', [nan]).
unsignedLong('3') + 2.5 ==> data('float', [5.5]).
boolean('true') + 2.5 ==> false.

% numeric-subtract
numeric-subtract(3, 2) ==> data('decimal', [1]).
1.5 - 'NaN' ==> data('float', [nan]).
'NaN' - 1.5 ==> data('float', [nan]).
'INF' - 3 ==> data('float', [inf]).
'-INF' - 1.2 ==> data('float', [-inf]).
5.25 - 'INF' ==> data('double', [-inf]).
5.25 - '-INF' ==> data('double', [inf]).
'INF' - 'INF' ==> data('float', [nan]).
'-INF' - '-INF' ==> data('double', [nan]).
'INF' - '-INF' ==> data('double', [inf]).
'-INF' - 'INF' ==> data('double', [-inf]).
-1 - -2 ==> data('decimal', [1]).
-2.3 - 1.7 ==> data('float', [-4.0]).
5 - 2.5 ==> data('float', [2.5]).
5 - 7 ==> data('decimal', [-2]).
13 - 7 - 8 ==> data('decimal', [-2]).
'INF' - 7 - 8 ==> data('float', [inf]).
unsignedLong('3') - 1 ==> data('decimal', [2]).
boolean('true') - 2.5 ==> false.

% numeric-multiply
numeric-multiply(3, 2) ==> data('decimal', [6]).
1.5 * 'NaN' ==> data('float', [nan]).
'NaN' * -1.5 ==> data('float', [nan]).
0 * '+INF' ==> data('double', [nan]).
'-INF' * 0.0 ==> data('double', [nan]).
3 * 'INF' ==> data('float', [inf]).
3 * '-INF' ==> data('float', [-inf]).
-3 * 'INF' ==> data('float', [-inf]).
-3 * '-INF' ==> data('float', [inf]).
'INF' * 3 ==> data('float', [inf]).
'INF' * -3 ==> data('float', [-inf]).
'-INF' * 3 ==> data('float', [-inf]).
'-INF' * -3 ==> data('float', [inf]).
'INF' * 'INF' ==> data('double', [inf]).
'-INF' * 'INF' ==> data('double', [-inf]).
'INF' * '-INF' ==> data('double', [-inf]).
'-INF' * '-INF' ==> data('double', [inf]).
-3 * 2 ==> data('decimal', [-6]).
0 * 0 ==> data('decimal', [0]).
'INF' * '-INF' * 0 ==> data('float', [nan]).
'INF' * -1 * 5 ==> data('float', [-inf]).
5 * 2 * 10 ==> data('decimal', [100]).
unsignedLong('3') * 1.5 ==> data('float', [4.5]).
boolean('true') * 2 ==> false.

% numeric-divide
numeric-divide(3, 2) ==> data('float', [1.5]).
1.5 div 'NaN' ==> data('float', [nan]).
'NaN' div -1.5 ==> data('float', [nan]).
5 div 0 ==> data('float', [inf]).
5 div -0 ==> data('float', [inf]). % should theoretically result in -inf
-5 div 0 ==> data('float', [-inf]).
-5 div -0 ==> data('float', [-inf]). % should theoretically result in inf
0 div 0 ==> data('double', [nan]).
0 div -0 ==> data('double', [nan]).
-0 div 0 ==> data('double', [nan]).
-0 div -0 ==> data('double', [nan]).
'INF' div 'INF' ==> data('double', [nan]).
'INF' div '-INF' ==> data('double', [nan]).
'-INF' div 'INF' ==> data('double', [nan]).
'-INF' div '-INF' ==> data('double', [nan]).
6 div 3 ==> data('decimal', [2]).
1.5 div 3 ==> data('float', [0.5]).
3 div 1.5 ==> data('float', [2.0]).
4.5 div 1.5 ==> data('double', [3.0]).

% numeric-integer-divide
numeric-integer-divide(3, 2) ==> data('integer', [1]).
1.5 idiv 'NaN' ==> false.
'NaN' idiv -1.5 ==> false.
5 idiv 0 ==> false.
5 idiv +0 ==> false.
5 idiv -0 ==> false.
'INF' idiv 3 ==> false.
'+INF' idiv 3 ==> false. 
'-INF' idiv 3 ==> false.
'INF' idiv 'INF' ==> false.
'-INF' idiv 'INF' ==> false.
'INF' idiv '-INF' ==> false.
'-INF' idiv '-INF' ==> false.
5 idiv 'INF' ==> data('integer', [0]).
5 idiv '+INF' ==> data('integer', [0]).
5 idiv '-INF' ==> data('integer', [0]).
10 idiv 3 ==> data('integer', [3]).
3 idiv -2 ==> data('integer', [-1]).
-3 idiv 2 ==> data('integer', [-1]).
-3 idiv -2 ==> data('integer', [1]).
9.0 idiv 3 ==> data('integer', [3]).
-3.5 idiv 3 ==> data('integer', [-1]).
3.0 idiv 4 ==> data('integer', [0]).
3.1E1 idiv 6 ==> data('integer', [5]).
3.1E1 idiv 7 ==> data('integer', [4]).

% numeric-unary-plus
numeric-unary-plus(1) ==> data('decimal', [1]).
+'NaN' ==> data('float', [nan]).
+'INF' ==> data('float', [inf]).
+'-INF' ==> data('float', [-inf]).
+0.0 ==> data('float', [0.0]).
+0 ==> data('decimal', [0]).
+5 ==> data('decimal', [5]).
+unsignedLong('3') ==> data('unsignedLong', [3]).
+boolean('true') ==> false.

% numeric-unary-minus
numeric-unary-minus(1) ==> data('decimal', [-1]).
-'NaN' ==> data('float', [nan]).
-'INF' ==> data('float', [-inf]).
-'-INF' ==> data('float', [inf]).
-0.0 ==> data('float', [0.0]).
-0 ==> data('decimal', [0]).
-5 ==> data('decimal', [-5]).
-unsignedLong('3') ==> data('unsignedLong', [-3]).
-boolean('true') ==> false.