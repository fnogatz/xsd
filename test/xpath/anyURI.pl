:- module(anyURI, [
      user:('==>')/2
   ]).

% resolve-uri
resolve-uri('prod', 'http://datypic.com/') ==> data('anyURI', ['http://datypic.com/prod']).
resolve-uri('prod2', 'http://datypic.com/prod1') ==> data('anyURI', ['http://datypic.com/prod2']).
resolve-uri('http://example.org', 'http://datypic.com') ==> data('anyURI', ['http://example.org']).
resolve-uri('http://datypic.com', '../base') ==> data('anyURI', ['http://datypic.com']).
resolve-uri('', 'http://datypic.com') ==> data('anyURI', ['http://datypic.com']).
% TODO: replace localhost by static base url
resolve-uri('prod') ==> data('anyURI', ['http://localhost/prod']).
resolve-uri('') ==> data('anyURI', ['http://localhost']).