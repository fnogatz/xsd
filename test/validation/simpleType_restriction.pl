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

'fractionDigits=2 without fraction digits':
{|xml||
	<s17>1</s17>
|}.

'fractionDigits=2 with less than maximum fraction digits (positive)':
{|xml||
	<s17>456.1</s17>
|}.

'fractionDigits=2 with less than maximum fraction digits (negative)':
{|xml||
	<s17>-634.3</s17>
|}.

'fractionDigits=2 with exact fraction digits':
{|xml||
	<s17>689.12</s17>
|}.

'fractionDigits=2 with exact fraction digits and trailing zero':
{|xml||
	<s17>984.650</s17>
|}.

'fractionDigits=2 with too many fraction digits'(fail):
{|xml||
	<s17>6844.384</s17>
|}.

'fractionDigits=0 without fraction digits':
{|xml||
	<s18>65468</s18>
|}.

'fractionDigits=0 with redundant fraction digit':
{|xml||
	<s18>6984.0</s18>
|}.

'fractionDigits=0 with invalid fraction digit'(fail):
{|xml||
	<s18>35.1</s18>
|}.

'totalDigits=4 without fraction digits and less than the maximum integer digits #1':
{|xml||
	<s20>845</s20>
|}.

'totalDigits=4 without fraction digits and less than the maximum integer digits #2':
{|xml||
	<s20>0</s20>
|}.

'totalDigits=4 without fraction digits and the exact number of integer digits':
{|xml||
	<s20>8455</s20>
|}.

'totalDigits=4 without fraction digits and too many integer digits'(fail):
{|xml||
	<s20>84551</s20>
|}.

'totalDigits=4 with fraction digits and integer digits less than the maximum number':
{|xml||
	<s20>12.3</s20>
|}.

'totalDigits=4 with fraction digits and integer digits exactly the maximum number #1':
{|xml||
	<s20>0.123</s20>
|}.

'totalDigits=4 with fraction digits and integer digits exactly the maximum number #2':
{|xml||
	<s20>65.12</s20>
|}.

'totalDigits=4 with fraction digits and integer digits exactly the maximum number with redundant zeroes':
{|xml||
	<s20>0065.1200</s20>
|}.

'totalDigits=4 with fraction digits and too many integer digits'(fail):
{|xml||
	<s20>548.89</s20>
|}.

'totalDigits=4 with integer digits and too many fraction digits'(fail):
{|xml||
	<s20>89.465</s20>
|}.

'totalDigits=4 with too many fraction digits and integer digits'(fail):
{|xml||
	<s20>0.1234</s20>
|}.