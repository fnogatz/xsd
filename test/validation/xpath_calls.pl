/* assert on complexType */

'assert on empty complexType (none, invalid)'(fail):
{|xml||
	<assert_complexType_empty_none>
		String
	</assert_complexType_empty_none>
|}.

'assert on empty complexType (none, valid)':
{|xml||
	<assert_complexType_empty_none></assert_complexType_empty_none>
|}.

'assert on empty complexType (single, invalid)'(fail):
{|xml||
	<assert_complexType_empty_single_invalid></assert_complexType_empty_single_invalid>
|}.

'assert on empty complexType (single, valid)':
{|xml||
	<assert_complexType_empty_single_valid></assert_complexType_empty_single_valid>
|}.

'assert on empty complexType with attribute (single, invalid)'(fail):
{|xml||
	<assert_complexType_empty_attribute_single_invalid attr="2"></assert_complexType_empty_attribute_single_invalid>
|}.

'assert on empty complexType with attribute (single, valid)':
{|xml||
	<assert_complexType_empty_attribute_single_valid attr="2"></assert_complexType_empty_attribute_single_valid>
|}.

'assert on empty complexType (multiple, none valid)'(fail):
{|xml||
	<assert_complexType_empty_multiple_none_valid></assert_complexType_empty_multiple_none_valid>
|}.

'assert on empty complexType (multiple, partially valid)'(fail):
{|xml||
	<assert_complexType_empty_multiple_partially_valid></assert_complexType_empty_multiple_partially_valid>
|}.

'assert on empty complexType (multiple, all valid)':
{|xml||
	<assert_complexType_empty_multiple_all_valid></assert_complexType_empty_multiple_all_valid>
|}.

'assert on non-empty complexType (none, invalid)'(fail):
{|xml||
	<assert_complexType_none>
		<item>$asijc</item>
	</assert_complexType_none>
|}.

'assert on non-empty complexType (none, valid)':
{|xml||
	<assert_complexType_none>
		<item>2</item>
	</assert_complexType_none>
|}.

'assert on non-empty complexType (single, invalid)'(fail):
{|xml||
	<assert_complexType_single_invalid>
		<item>2</item>
	</assert_complexType_single_invalid>
|}.

'assert on non-empty complexType (single, valid)':
{|xml||
	<assert_complexType_single_valid>
		<item>2</item>
	</assert_complexType_single_valid>
|}.

'assert on non-empty complexType with attribute (single, invalid)'(fail):
{|xml||
	<assert_complexType_single_attribute_invalid attr="2">
		<item>2</item>
	</assert_complexType_single_attribute_invalid>
|}.

'assert on non-empty complexType with attribute (single, valid)':
{|xml||
	<assert_complexType_single_attribute_valid attr="2">
		<item>2</item>
	</assert_complexType_single_attribute_valid>
|}.

'assert on non-empty complexType (multiple, none valid)'(fail):
{|xml||
	<assert_complexType_multiple_non_valid>
		<item>2</item>
	</assert_complexType_multiple_non_valid>
|}.

'assert on non-empty complexType (multiple, partially valid)'(fail):
{|xml||
	<assert_complexType_multiple_partially_valid>
		<item>2</item>
	</assert_complexType_multiple_partially_valid>
|}.

'assert on non-empty complexType (multiple, all valid)':
{|xml||
	<assert_complexType_multiple_all_valid>
		<item>2</item>
	</assert_complexType_multiple_all_valid>
|}.


/* assertion on simpleType */

'assertion on simpleType restriction (none, invalid)'(fail):
{|xml||
	<assertion_restriction_none>String</assertion_restriction_none>
|}.

'assertion on simpleType restriction (none, valid)':
{|xml||
	<assertion_restriction_none>2</assertion_restriction_none>
|}.

'assertion on simpleType restriction (single, invalid)'(fail):
{|xml||
	<assertion_restriction_single>1</assertion_restriction_single>
|}.

'assertion on simpleType restriction (single, valid)':
{|xml||
	<assertion_restriction_single>2</assertion_restriction_single>
|}.

'assertion on simpleType restriction (multiple, none valid)'(fail):
{|xml||
	<assertion_restriction_multiple>1</assertion_restriction_multiple>
|}.

'assertion on simpleType restriction (multiple, partially valid)'(fail):
{|xml||
	<assertion_restriction_multiple>2</assertion_restriction_multiple>
|}.

'assertion on simpleType restriction (multiple, all valid)':
{|xml||
	<assertion_restriction_multiple>6</assertion_restriction_multiple>
|}.