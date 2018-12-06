'xs:hexBinary with uppercase characters':
{|xml||
	<hexBinary>0FB8</hexBinary>
|}.

'xs:hexBinary with lowercase characters':
{|xml||
	<hexBinary>0fb8</hexBinary>
|}.

'xs:hexBinary with empty value':
{|xml||
	<hexBinary></hexBinary>
|}.

'xs:hexBinary with an odd amount of characters'(fail):
{|xml||
	<hexBinary>FB8</hexBinary>
|}.

'xs:hexBinary with an invalid hex character'(fail):
{|xml||
	<hexBinary>GB8</hexBinary>
|}.




