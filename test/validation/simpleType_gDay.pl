'xs:gDay as required':
{|xml||
	<gDay>---02</gDay>
|}.

'xs:gDay without one leading hyphen'(fail):
{|xml||
	<gDay>--02</gDay>
|}.

'xs:gDay without two leading hyphens'(fail):
{|xml||
	<gDay>-02</gDay>
|}.

'xs:gDay without all leading hyphens'(fail):
{|xml||
	<gDay>02</gDay>
|}.

'xs:gDay with truncated day value'(fail):
{|xml||
	<gDay>---2</gDay>
|}.

'xs:gDay with too small day value'(fail):
{|xml||
	<gDay>---00</gDay>
|}.

'xs:gDay with too large day value'(fail):
{|xml||
	<gDay>---32</gDay>
|}.

'xs:gDay with empty value'(fail):
{|xml||
	<gDay></gDay>
|}.