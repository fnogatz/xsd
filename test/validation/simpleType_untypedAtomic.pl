'Arbitrary xs:untypedAtomic':
{|xml||
	<untypedAtomic>ß汉98af é▓fioe</untypedAtomic>
|}.

'Empty xs:untypedAtomic':
{|xml||
	<untypedAtomic />
|}.

'xs:untypedAtomic may not contain a complex type'(fail):
{|xml||
	<untypedAtomic><complex /></untypedAtomic>
|}.