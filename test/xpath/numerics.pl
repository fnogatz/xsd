:- module(numerics, [
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

% numeric-mod
numeric-mod(3, 2) ==> data('decimal', [1]).
decimal(1) mod decimal(0) ==> false.
'NaN' mod 3 ==> data('float', [nan]).
3 mod 'NaN' ==> data('double', [nan]).
'NaN' mod 'NaN' ==> data('float', [nan]).
'INF' mod 3 ==> data('float', [nan]).
'-INF' mod 3 ==> data('float', [nan]).
1.5 mod 0 ==> data('float', [nan]).
1.5 mod -0 ==> data('float', [nan]).
'INF' mod 0 ==> data('float', [nan]).
'-INF' mod 0 ==> data('float', [nan]).
'INF' mod -0 ==> data('float', [nan]).
'-INF' mod -0 ==> data('float', [nan]).
-3 mod 'INF' ==> data('float', [-3]).
3 mod '-INF' ==> data('float', [3]).
0 mod 3.5 ==> data('float', [0]).
-0 mod 4 ==> data('float', [0]).
10 mod 3 ==> data('float', [1]).
6 mod -2 ==> data('decimal', [0]).
4.5 mod 1.2 ==> data('float', [0.9000000000000004]).
1.23e2 mod 0.6e1 ==> data('float', [3.0]).

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

% numeric-equal
numeric-equal(1, 1) ==> data('boolean', [true]).
% rest see eq, ne, le, ge

% numeric-less-than
numeric-less-than(1, 2) ==> data('boolean', [true]).
% rest see lt, le

% numeric-greater-than
numeric-greater-than(2, 1) ==> data('boolean', [true]).
% rest see gt, ge

% eq
0 eq 0 ==> data('boolean', [true]).
-0 eq +0 ==> data('boolean', [true]).
1 eq 1 ==> data('boolean', [true]).
-1 eq +1 ==> data('boolean', [false]).
0 eq 1 ==> data('boolean', [false]).
'-INF' eq '-INF' ==> data('boolean', [true]).
'-INF' eq 'INF' ==> data('boolean', [false]).
'+INF' eq 'INF' ==> data('boolean', [true]).
'INF' eq 'INF' ==> data('boolean', [true]).
1 eq 'INF' ==> data('boolean', [false]).
'-INF' eq -999999999 ==> data('boolean', [false]).
'NaN' eq 'NaN' ==> data('boolean', [false]).
'NaN' eq 5 ==> data('boolean', [false]).
unsignedLong('3') eq long('3') ==> data('boolean', [true]).

% ne
0 ne 0 ==> data('boolean', [false]).
-0 ne +0 ==> data('boolean', [false]).
1 ne 1 ==> data('boolean', [false]).
-1 ne +1 ==> data('boolean', [true]).
0 ne 1 ==> data('boolean', [true]).
'-INF' ne '-INF' ==> data('boolean', [false]).
'-INF' ne 'INF' ==> data('boolean', [true]).
'+INF' ne 'INF' ==> data('boolean', [false]).
'INF' ne 'INF' ==> data('boolean', [false]).
1 ne 'INF' ==> data('boolean', [true]).
'-INF' ne -999999999 ==> data('boolean', [true]).
'NaN' ne 'NaN' ==> data('boolean', [true]).
'NaN' ne 5 ==> data('boolean', [true]).
unsignedLong('3') ne long('3') ==> data('boolean', [false]).

% le
-1 le 0 ==> data('boolean', [true]).
0 le -1 ==> data('boolean', [false]).
0 le -0 ==> data('boolean', [true]).
'-INF' le '-INF' ==> data('boolean', [true]).
'-INF' le -3 ==> data('boolean', [true]).
'-INF' le 'INF' ==> data('boolean', [true]).
'-INF' le 'NaN' ==> data('boolean', [false]).
3 le 'INF' ==> data('boolean', [true]).
'INF' le 'INF' ==> data('boolean', [true]).
'NaN' le 'INF' ==> data('boolean', [false]).
'NaN' le 'NaN' ==> data('boolean', [false]).
unsignedLong('3') le long('3') ==> data('boolean', [true]).

% lt
-1 lt 0 ==> data('boolean', [true]).
0 lt -1 ==> data('boolean', [false]).
0 lt -0 ==> data('boolean', [false]).
'-INF' lt '-INF' ==> data('boolean', [false]).
'-INF' lt -3 ==> data('boolean', [true]).
'-INF' lt 'INF' ==> data('boolean', [true]).
'-INF' lt 'NaN' ==> data('boolean', [false]).
3 lt 'INF' ==> data('boolean', [true]).
'INF' lt 'INF' ==> data('boolean', [false]).
'NaN' lt 'INF' ==> data('boolean', [false]).
'NaN' lt 'NaN' ==> data('boolean', [false]).
unsignedLong('2') lt long('3') ==> data('boolean', [true]).

% ge
-1 ge 0 ==> data('boolean', [false]).
0 ge -1 ==> data('boolean', [true]).
0 ge -0 ==> data('boolean', [true]).
'-INF' ge '-INF' ==> data('boolean', [true]).
-3 ge '-INF' ==> data('boolean', [true]).
'INF' ge '-INF' ==> data('boolean', [true]).
'NaN' ge '-INF' ==> data('boolean', [false]).
'INF' ge 3 ==> data('boolean', [true]).
'INF' ge 'INF' ==> data('boolean', [true]).
'INF' ge 'NaN' ==> data('boolean', [false]).
'NaN' ge 'NaN' ==> data('boolean', [false]).
unsignedLong('3') ge long('3') ==> data('boolean', [true]).

% gt
-1 gt 0 ==> data('boolean', [false]).
0 gt -1 ==> data('boolean', [true]).
0 gt -0 ==> data('boolean', [false]).
'-INF' gt '-INF' ==> data('boolean', [false]).
-3 gt '-INF' ==> data('boolean', [true]).
'INF' gt '-INF' ==> data('boolean', [true]).
'NaN' gt '-INF' ==> data('boolean', [false]).
'INF' gt 3 ==> data('boolean', [true]).
'INF' gt 'INF' ==> data('boolean', [false]).
'INF' gt 'NaN' ==> data('boolean', [false]).
'NaN' gt 'NaN' ==> data('boolean', [false]).
unsignedLong('3') gt long('2') ==> data('boolean', [true]).

% abs
abs(-0) ==> data('decimal', [0]).
abs(+0) ==> data('decimal', [0]).
abs(-10.5) ==> data('float', [10.5]).
abs(10.5e2) ==> data('float', [10.5e2]).
abs('NaN') ==> data('float', [nan]).
abs('-INF') ==> data('float', [inf]).
abs('INF') ==> data('float', [inf]).
abs('+INF') ==> data('float', [inf]).
abs('1') ==> data('decimal', [1]).
abs('STRING') ==> false.

% ceiling
ceiling(-0) ==> data('decimal', [0]).
ceiling(+0) ==> data('decimal', [0]).
ceiling(-10.5) ==> data('float', [-10]).
ceiling(10.5) ==> data('float', [11]).
ceiling('NaN') ==> data('float', [nan]).
ceiling('-INF') ==> data('float', [-inf]).
ceiling('INF') ==> data('float', [inf]).
ceiling('+INF') ==> data('float', [inf]).
ceiling('1') ==> data('decimal', [1]).
ceiling('STRING') ==> false.

% floor
floor(-0) ==> data('decimal', [0]).
floor(+0) ==> data('decimal', [0]).
floor(-10.5) ==> data('float', [-11]).
floor(10.5) ==> data('float', [10]).
floor('NaN') ==> data('float', [nan]).
floor('-INF') ==> data('float', [-inf]).
floor('INF') ==> data('float', [inf]).
floor('+INF') ==> data('float', [inf]).
floor('1') ==> data('decimal', [1]).
floor('STRING') ==> false.

% round
round(-0) ==> data('decimal', [0]).
round(+0) ==> data('decimal', [0]).
round(-10.5) ==> data('float', [-10]).
round(-11.5) ==> data('float', [-11]).
round(10.5) ==> data('float', [11]).
round(11.5) ==> data('float', [12]).
round('NaN') ==> data('float', [nan]).
round('-INF') ==> data('float', [-inf]).
round('INF') ==> data('float', [inf]).
round('+INF') ==> data('float', [inf]).
round('1') ==> data('decimal', [1]).
round('STRING') ==> false.

% round-half-to-even
round-half-to-even(-0) ==> data('decimal', [0]).
round-half-to-even(+0) ==> data('decimal', [0]).
round-half-to-even(-11.6) ==> data('float', [-12]).
round-half-to-even(-11.5) ==> data('float', [-12]).
round-half-to-even(-11.4) ==> data('float', [-11]).
round-half-to-even(-10.6) ==> data('float', [-11]).
round-half-to-even(-10.5) ==> data('float', [-10]).
round-half-to-even(-10.4) ==> data('float', [-10]).
round-half-to-even(10.4) ==> data('float', [10]).
round-half-to-even(10.5) ==> data('float', [10]).
round-half-to-even(10.6) ==> data('float', [11]).
round-half-to-even(11.4) ==> data('float', [11]).
round-half-to-even(11.5) ==> data('float', [12]).
round-half-to-even(11.6) ==> data('float', [12]).
round-half-to-even(3.567812E+3, 2) ==> data('float', [3567.810000000001]).
round-half-to-even(4.7564E-3, 2) ==> data('float', [0.0]).
round-half-to-even(35612.25, -2) ==> data('float', [35600]).
round-half-to-even('NaN') ==> data('float', [nan]).
round-half-to-even('-INF') ==> data('float', [-inf]).
round-half-to-even('INF') ==> data('float', [inf]).
round-half-to-even('+INF') ==> data('float', [inf]).
round-half-to-even('1') ==> data('decimal', [1]).
round-half-to-even('STRING') ==> false.