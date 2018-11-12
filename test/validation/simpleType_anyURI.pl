'ftp scheme for File Transfer Protocol services':
{|xml||
	<simple>ftp://ftp.is.co.za/rfc/rfc1808.txt</simple>
|}.

'gopher scheme for Gopher and Gopher+ Protocol services':
{|xml||
	<simple>gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles</simple>
|}.

'http scheme for Hypertext Transfer Protocol services':
{|xml||
	<simple>http://www.math.uio.no/faq/compression-faq/part1.html</simple>
|}.

'mailto scheme for electronic mail addresses':
{|xml||
	<simple>mailto:mduerst@ifi.unizh.ch</simple>
|}. 

'news scheme for USENET news groups and articles':
{|xml||
	<simple>news:comp.infosystems.www.servers.unix</simple>
|}.

'telnet scheme for interactive services via the TELNET Protocol':
{|xml||
	<simple>telnet://melvyl.ucop.edu/</simple>
|}.

'wrong URI'(fail):
{|xml||
	<simple>:ftp</simple>
|}.
'URI IPv6':
{|xml||
	<simple>ldap://[2001:db8::7]/c=GB?objectClass?one</simple>
|}.
'tel':
{|xml||
	<simple>tel:+1-816-555-1212</simple>
|}.
'telnet #2':
{|xml||
	<simple>telnet://192.0.2.16:80/</simple>
|}.
'https':
{|xml||
	<simple>https://john.doe@www.example.com:123/forum/questions/?tag=networking#top</simple>
|}.
'https fail'(fail):
{|xml||
	<simple>https:#john.doe@?:123/forum/questions/?tag=networking#top</simple>
|}.
'missing scheme'(fail):
{|xml||
	<simple>//example.com</simple>
|}.