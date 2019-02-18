:- module(test_xpath_constructors, [
      '==>'/2,
      op(800, xfx, '==>')
   ]).

:- discontiguous test_xpath_constructors:'==>'/2.

% string
string('NaN') ==> data('string', ['NaN']).

% boolean
boolean('1') ==> data('boolean', [true]).
boolean('true') ==> data('boolean', [true]).
boolean('0') ==> data('boolean', [false]).
boolean('false') ==> data('boolean', [false]).
boolean('arbitrary') ==> data('boolean', [true]).

% decimal
decimal('+1.234') ==> data('decimal', [1.234]).
decimal('.234') ==> data('decimal', [0.234]).
decimal('test') ==> false.

% float
float('-3.1415') ==> data('float', [-3.1415]).
float('-0') ==> data('float', [0]).
float('-3E2') ==> data('float', [-300.0]).
float('4268.22752E11') ==> data('float', [426822752000000.0]).
float('-INF') ==> data('float', [-inf]).
float('NaN') ==> data('float', [nan]).
float('NAN') ==> false.

% double
double('-3.1415') ==> data('double', [-3.1415]).
double('-0') ==> data('double', [0]).
double('-3E2') ==> data('double', [-300.0]).
double('4268.22752E11') ==> data('double', [426822752000000.0]).
double('-INF') ==> data('double', [-inf]).
double('NaN') ==> data('double', [nan]).
double('NAN') ==> false.

% duration
duration('P2Y6M5DT12H35M30S') ==> data('duration', [+, 2, 6, 5, 12, 35, 30]).
duration('P2M') ==> data('duration', [+, 0, 2, 0, 0, 0, 0]).
duration('-PT01M30.5S') ==> data('duration', [+, 0, 0, 0, 0, 1, 30.5]).
duration('-P0Y') ==> data('duration', [+, 0, 0, 0, 0, 0, 0]).
duration('P2Y13M40DT30H65M92S') ==> data('duration', [+, 3, 2, 10, 7, 6, 32]).

% dateTime
dateTime('-0032-04-12T13:20:00') ==> data('dateTime', [-32, 4, 12, 13, 20, 0, 0]).
dateTime('-0032-04-12T13:20:00Z') ==> data('dateTime', [-32, 4, 12, 13, 20, 0, 0]).
dateTime('-12345-04-12T13:20:00-14:00') ==> data('dateTime', [-12345, 4, 12, 13, 20, 0, -840]).
dateTime('-1234-04-12T13:20:00+14:00') ==> data('dateTime', [-1234, 4, 12, 13, 20, 0, 840]).
dateTime('0000-01-01T00:00:00') ==> data('dateTime', [0, 1, 1, 0, 0, 0, 0]).
dateTime('2004-04-12T13:20:00Z') ==> data('dateTime', [2004, 4, 12, 13, 20, 0, 0]).
dateTime('2004-04-12T13:20:00-05:00') ==> data('dateTime', [2004, 4, 12, 13, 20, 0, -300]).
dateTime('2004-04-12T13:20:00+11:59') ==> data('dateTime', [2004, 4, 12, 13, 20, 0, 719]).
dateTime('2004-04-12', '13:20:00') ==> data('dateTime', [2004, 4, 12, 13, 20, 0, 0]).
dateTime('2004-04-12+01:30', '13:20:00+01:30') ==> data('dateTime', [2004, 4, 12, 13, 20, 0, 90]).
dateTime('2004-04-12+01:30', '13:20:00') ==> data('dateTime', [2004, 4, 12, 13, 20, 0, 90]).
dateTime('2004-04-12', '13:20:00+01:30') ==> data('dateTime', [2004, 4, 12, 13, 20, 0, 90]).
dateTime('2004-04-12-01:30', '13:20:00+01:30').

% time
time('13:20:30.5555') ==> data('time', [0, 0, 0, 13, 20, 30.5555, 0]).
time('07:39:00Z') ==> data('time', [0, 0, 0, 7, 39, 0, 0]).
time('13:20:00-05:20') ==> data('time', [0, 0, 0, 13, 20, 0, -320]).
time('13:20:00+10:45') ==> data('time', [0, 0, 0, 13, 20, 0, 645]).

% date
date('-0032-01-01') ==> data('date', [-32, 1, 1, 0, 0, 0, 0]).
date('-0032-04-12Z') ==> data('date', [-32, 4, 12, 0, 0, 0, 0]).
date('-0032-04-12-12:11') ==> data('date', [-32, 4, 12, 0, 0, 0, -731]).
date('-0032-04-12+06:39') ==> data('date', [-32, 4, 12, 0, 0, 0, 399]).
date('12004-04-12') ==> data('date', [12004, 4, 12, 0, 0, 0, 0]).
date('2004-04-12Z') ==> data('date', [2004, 4, 12, 0, 0, 0, 0]).
date('2004-04-12-13:59') ==> data('date', [2004, 4, 12, 0, 0, 0, -839]).
date('2004-04-12+13:59') ==> data('date', [2004, 4, 12, 0, 0, 0, 839]).

% gYearMonth
gYearMonth('-0032-01') ==> data('gYearMonth', [-32, 1, 0, 0, 0, 0, 0]).
gYearMonth('-0032-04Z') ==> data('gYearMonth', [-32, 4, 0, 0, 0, 0, 0]).
gYearMonth('-0032-04-12:11') ==> data('gYearMonth', [-32, 4, 0, 0, 0, 0, -731]).
gYearMonth('-0032-04+06:39') ==> data('gYearMonth', [-32, 4, 0, 0, 0, 0, 399]).
gYearMonth('12004-04') ==> data('gYearMonth', [12004, 4, 0, 0, 0, 0, 0]).
gYearMonth('2004-04Z') ==> data('gYearMonth', [2004, 4, 0, 0, 0, 0, 0]).
gYearMonth('2004-04-13:59') ==> data('gYearMonth', [2004, 4, 0, 0, 0, 0, -839]).
gYearMonth('2004-04+13:59') ==> data('gYearMonth', [2004, 4, 0, 0, 0, 0, 839]).

% gYear
gYear('-0032') ==> data('gYear', [-32, 0, 0, 0, 0, 0, 0]).
gYear('-0032Z') ==> data('gYear', [-32, 0, 0, 0, 0, 0, 0]).
gYear('-0032-12:11') ==> data('gYear', [-32, 0, 0, 0, 0, 0, -731]).
gYear('-0032+06:39') ==> data('gYear', [-32, 0, 0, 0, 0, 0, 399]).
gYear('12004') ==> data('gYear', [12004, 0, 0, 0, 0, 0, 0]).
gYear('2004Z') ==> data('gYear', [2004, 0, 0, 0, 0, 0, 0]).
gYear('2004-13:59') ==> data('gYear', [2004, 0, 0, 0, 0, 0, -839]).
gYear('2004+13:59') ==> data('gYear', [2004, 0, 0, 0, 0, 0, 839]).

% gMonthDay
gMonthDay('--01-01') ==> data('gMonthDay', [0, 1, 1, 0, 0, 0, 0]).
gMonthDay('--04-12Z') ==> data('gMonthDay', [0, 4, 12, 0, 0, 0, 0]).
gMonthDay('--04-12-12:11') ==> data('gMonthDay', [0, 4, 12, 0, 0, 0, -731]).
gMonthDay('--04-12+06:39') ==> data('gMonthDay', [0, 4, 12, 0, 0, 0, 399]).

% gDay
gDay('---01') ==> data('gDay', [0, 0, 1, 0, 0, 0, 0]).
gDay('---12Z') ==> data('gDay', [0, 0, 12, 0, 0, 0, 0]).
gDay('---12-12:11') ==> data('gDay', [0, 0, 12, 0, 0, 0, -731]).
gDay('---12+06:39') ==> data('gDay', [0, 0, 12, 0, 0, 0, 399]).

% gMonth
gMonth('--01') ==> data('gMonth', [0, 1, 0, 0, 0, 0, 0]).
gMonth('--04Z') ==> data('gMonth', [0, 4, 0, 0, 0, 0, 0]).
gMonth('--04-12:11') ==> data('gMonth', [0, 4, 0, 0, 0, 0, -731]).
gMonth('--04+06:39') ==> data('gMonth', [0, 4, 0, 0, 0, 0, 399]).

% hexBinary
hexBinary('0FB8') ==> data('hexBinary', ['0FB8']).
hexBinary('0fb8') ==> data('hexBinary', ['0FB8']).
hexBinary('') ==> data('hexBinary', ['']).

% base64Binary
base64Binary('0 FB8 0F+9') ==> data('base64Binary', ['0FB80F+9']).
base64Binary('0F+40A==') ==> data('base64Binary', ['0F+40A==']).
base64Binary('') ==> data('base64Binary', ['']).

% anyURI
anyURI('https://john.doe@www.example.com:123/forum/questions/?tag=networking#top') ==> data('anyURI', ['https://john.doe@www.example.com:123/forum/questions/?tag=networking#top']).

% QName
QName('pre:myElement') ==> data('QName', ['pre:myElement']).

% normalizedString
normalizedString('   Separated by 3 spaces.') ==> data('normalizedString', ['   Separated by 3 spaces.']).

% token
token('This is a string!') ==> data('token', ['This is a string!']).

% language
language('en-US') ==> data('language', ['en-US']).

% NMTOKEN
NMTOKEN('123_456') ==> data('NMTOKEN', ['123_456']).
NMTOKEN('   additional_spaces_around   ') ==> data('NMTOKEN', ['additional_spaces_around']).

% NCName
NCName('_my-element') ==> data('NCName', ['_my-element']).

% Name
Name('_my:element') ==> data('Name', ['_my:element']).

% ID
ID('_my-element') ==> data('ID', ['_my-element']).

% IDREF
IDREF('_my-element') ==> data('IDREF', ['_my-element']).

% ENTITY
ENTITY('_my-element') ==> data('ENTITY', ['_my-element']).

% integer
integer('0123') ==> data('integer', [123]).
integer('+123') ==> data('integer', [123]).
integer('-123') ==> data('integer', [-123]).

% nonPositiveInteger
nonPositiveInteger('-01234') ==> data('nonPositiveInteger', [-1234]).

% negativeInteger
negativeInteger('-1234') ==> data('negativeInteger', [-1234]).

% long
long('-21474836480') ==> data('long', [-21474836480]).

% int
int('-123') ==> data('int', [-123]).

% short
short('-1234') ==> data('short', [-1234]).

% byte
byte('-128') ==> data('byte', [-128]).

% nonNegativeInteger
nonNegativeInteger('1234') ==> data('nonNegativeInteger', [1234]).

% unsignedLong
unsignedLong('18446744073709551615') ==> data('unsignedLong', [18446744073709551615]).

% unsignedInt
unsignedInt('1234') ==> data('unsignedInt', [1234]).

% unsignedShort
unsignedShort('65432') ==> data('unsignedShort', [65432]).

% unsignedByte
unsignedByte('255') ==> data('unsignedByte', [255]).

% positiveInteger
positiveInteger('1234') ==> data('positiveInteger', [1234]).

% yearMonthDuration
yearMonthDuration('P2Y6M') ==> data('yearMonthDuration', [+, 2, 6, 0, 0, 0, 0]).
yearMonthDuration('P20M') ==> data('yearMonthDuration', [+, 1, 8, 0, 0, 0, 0]).

% dayTimeDuration
dayTimeDuration('P5DT12H35M30S') ==> data('dayTimeDuration', [+, 0, 0, 5, 12, 35, 30]).
dayTimeDuration('PT120M') ==> data('dayTimeDuration', [+, 0, 0, 0, 2, 0, 0]).
dayTimeDuration('PT65.5S') ==> data('dayTimeDuration', [+, 0, 0, 0, 0, 1, 5.5]).

% untypedAtomic
untypedAtomic('ß汉98af é▓fioe') ==> data('untypedAtomic', ['ß汉98af é▓fioe']).
