'xs:ENTITIES with one ENTITY':
{|xml||
	<ENTITIES>myElement</ENTITIES>
|}.

'xs:ENTITIES with two ENTITYs':
{|xml||
	<ENTITIES>myElement _myElement</ENTITIES>
|}.

'xs:ENTITIES with an invalid ENTITY'(fail):
{|xml||
	<ENTITIES>myElement :myElement</ENTITIES>
|}.

'xs:ENTITIES with no ENTITY'(fail):
{|xml||
	<ENTITIES></ENTITIES>
|}.