:- module(cornetto_lod, []).

:- use_module(library(http/http_dirindex)).
:- use_module(library(http/http_dispatch)).

:- use_module(api(lod)).

/* Handler to make Cornetto RDF resources resolvable when redirected from purl to here:
 */
:- http_handler(root('lod/purl/vocabularies/'),
	lod_api([ redirected_from('http://purl.org/vocabularies/'),
		  bounded_description(scbd)
                ]),
	[ prefix ]
   ).

/* Handler to make Cornetto downloadable file resources resolvable when redirected from purl to here:
 *
 * Disabled as long as downloads are not allowed due to license issues...
 *
 */

/*
:- asserta(user:file_search_path(download, 'cornetto/rdf')).
:- http_handler(cliopatria('lod/purl/vocabularies/cornetto/21/download/'),
		serve_static('cornetto/lod/purl/vocabularies/cornetto/21/download', download),
		[prefix]).

*/

:- asserta(user:file_search_path(www, 'cornetto/www')).
:- http_handler(cliopatria('www'),
		serve_static('cornetto/www', www),
		[prefix]).

serve_static(Prefix, Alias, Request) :-
	memberchk(path(PathInfo), Request),
	sub_atom(PathInfo, 1, Len, End, Prefix),
	(   End < 2
	->  Path='.'
	;   debug(foo, '~w', [End]),
	Start is Len + 2,
	sub_atom(PathInfo, Start, _, 0, Path)
	),
	Term =.. [Alias,Path],
	(   absolute_file_name(Term, _,
		[file_type(directory),
		access(read),
		file_errors(fail)
		])
	->  http_reply_dirindex(Term, [unsafe(true)], Request)
	;   http_reply_file(Term, [], Request)
	).

