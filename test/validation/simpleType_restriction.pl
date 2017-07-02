'basetype only #1':
{|xml||
	<s0>12</s0>
|}.

'basetype only #2'(fail):
{|xml||
	<s0 />
|}.

'basetype only #3'(fail):
{|xml||
	<s0>ab</s0>
|}.

'minExclusive #1':
{|xml||
	<s1>13</s1>
|}.

'minExclusive #2'(fail):
{|xml||
	<s1>12</s1>
|}.

'minInclusive #1':
{|xml||
	<s2>12</s2>
|}.

'minInclusive #2'(fail):
{|xml||
	<s2>11</s2>
|}.

'maxInclusive #1':
{|xml||
	<s3>21</s3>
|}.

'maxInclusive #2'(fail):
{|xml||
	<s3>22</s3>
|}.

'maxExclusive #1':
{|xml||
	<s4>20</s4>
|}.

'maxExclusive #2'(fail):
{|xml||
	<s4>21</s4>
|}.

'minInclusive,maxInclusive #1':
{|xml||
	<s5>12</s5>
|}.

'minInclusive,maxInclusive #2'(fail):
{|xml||
	<s5>11</s5>
|}.

'minInclusive,maxInclusive #3'(fail):
{|xml||
	<s5>22</s5>
|}.

'minExclusive,maxInclusive #1':
{|xml||
	<s6>13</s6>
|}.

'minExclusive,maxInclusive #2'(fail):
{|xml||
	<s6>12</s6>
|}.

'minExclusive,maxInclusive #3'(fail):
{|xml||
	<s6>22</s6>
|}.

'minInclusive,maxExclusive #1':
{|xml||
	<s7>-12</s7>
|}.

'minInclusive,maxExclusive #2'(fail):
{|xml||
	<s7>-13</s7>
|}.

'minInclusive,maxExclusive #3'(fail):
{|xml||
	<s7>-3</s7>
|}.

'minExclusive,maxExclusive #1':
{|xml||
	<s8>-0</s8>
|}.

'minExclusive,maxExclusive #2'(fail):
{|xml||
	<s8>-25</s8>
|}.

'minExclusive,maxExclusive #3'(fail):
{|xml||
	<s8>12</s8>
|}.

'enumeration_1 #1':
{|xml||
	<s9>-1</s9>
|}.

'enumeration_1 #2':
{|xml||
	<s9>13</s9>
|}.

'enumeration_1 #3'(fail):
{|xml||
	<s9>7</s9>
|}.

'enumeration_2 #1':
{|xml||
	<s10>b</s10>
|}.

'enumeration_2 #2'(fail):
{|xml||
	<s10>d</s10>
|}.

'enumeration_2 #3'(fail):
{|xml||
	<s10></s10>
|}.

'user defined basetype #1':
{|xml||
	<s11>4</s11>
|}.

'user defined basetype #2'(fail):
{|xml||
	<s11>7</s11>
|}.

'user defined basetype #3':
{|xml||
	<s12>3</s12>
|}.

'user defined basetype #4'(fail):
{|xml||
	<s12>4</s12>
|}.

'pattern_1 #1':
{|xml||
	<s13>a</s13>
|}.

'pattern_1 #2':
{|xml||
	<s13>z</s13>
|}.

'pattern_1 #3'(fail):
{|xml||
	<s13>0</s13>
|}.

'pattern_1 #4'(fail):
{|xml||
	<s13>A</s13>
|}.

'length #1':
{|xml||
	<s14>abcde</s14>
|}.

'length too short #1'(fail):
{|xml||
	<s14></s14>
|}.

'length too short #2'(fail):
{|xml||
	<s14>abcd</s14>
|}.

'length too big #1'(fail):
{|xml||
	<s14>abcdef</s14>
|}.

'minLength #1':
{|xml||
	<s15>abc</s15>
|}.

'minLength #2':
{|xml||
	<s15>abcde</s15>
|}.

'minLength too short'(fail):
{|xml||
	<s15>ab</s15>
|}.

'maxLength #1':
{|xml||
	<s16>abc</s16>
|}.

'maxLength #2':
{|xml||
	<s16>a</s16>
|}.

'maxLength too big'(fail):
{|xml||
	<s16>abcd</s16>
|}.
