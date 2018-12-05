'xs:base64Binary with uppercase characters':
{|xml||
	<base64Binary>0FB8</base64Binary>
|}.

'xs:base64Binary with lowercase characters':
{|xml||
	<base64Binary>0fb8</base64Binary>
|}.

'xs:base64Binary with whitespaces in-between':
{|xml||
	<base64Binary>0 FB8 0F+9</base64Binary>
|}.

'xs:base64Binary with trailing equals signs as padding':
{|xml||
	<base64Binary>0F+40A==</base64Binary>
|}.

'xs:base64Binary with empty value':
{|xml||
	<base64Binary></base64Binary>
|}.

'xs:base64Binary with an odd number of characters'(fail):
{|xml||
	<base64Binary>FB8</base64Binary>
|}.

'xs:base64Binary with leading equal signs'(fail):
{|xml||
	<base64Binary>==0F</base64Binary>
|}.