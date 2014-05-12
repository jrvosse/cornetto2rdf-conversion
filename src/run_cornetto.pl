:- module(cornetto_convert_data,
	  [ run_cornetto/0,
					% Partial steps
	    clean/0,
	    rewrite/0,
	    post_process/0,
	    save/0,

	    clean_all/0,

	    load/0,
	    load_lmf/0
	  ]).

user:file_search_path(data, 'cornetto/xml').

:- load_files(library(semweb/rdf_db), [silent(true)]).

:- rdf_register_ns(lmf,            'http://example.org/lmf/').  % dummy used for input ns

:- rdf_register_ns(cornetto,           'http://purl.org/vocabularies/cornetto/').
:- rdf_register_ns(corn21s,            'http://purl.org/vocabularies/cornetto/21/schema/').
:- rdf_register_ns(corn21i,            'http://purl.org/vocabularies/cornetto/21/instances/').
:- rdf_register_ns(corn21r,            'http://purl.org/vocabularies/cornetto/21/edoal/').

:- rdf_register_ns(wn30,     'http://purl.org/vocabularies/princeton/wn30/').
:- rdf_register_ns(wn20s,    'http://www.w3.org/2006/03/wn/wn20/schema/').
:- rdf_register_ns(wn20i,    'http://www.w3.org/2006/03/wn/wn20/instances/').

:- rdf_register_ns(lex,	     'http://www.lexinfo.net/lmf#').
:- rdf_register_ns(lemon,    'http://www.lemon-model.net/lemon#').
:- rdf_register_ns(isocat,   'http://www.isocat.org/datcat/').
:- rdf_register_ns(dcpos,    'https://catalog.clarin.eu/isocat/rest/dcs/119.rdf#').
:- rdf_register_ns(align,    'http://knowledgeweb.semanticweb.org/heterogeneity/alignment#').

:- load_files([ library(xmlrdf/xmlrdf),
		library(semweb/rdf_cache),
		library(semweb/rdf_library),
		library(semweb/rdf_turtle_write)
	      ], [silent(true)]).

:- use_module(rewrite_lmf).
:- use_module(post_process_lmf).

:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_abstract)).

:- use_module(cliopatria(hooks)).

:- rdf_meta
        context_triple(r, t),
        transitive_context(r).

load_ontologies :-
	rdf_attach_library('.'),
	rdf_load_library(dc),
	rdf_load_library(skos),
	rdf_load_library(rdfs),
	rdf_load_library(owl),
	rdf_load_library(cornetto21Schema),

	rdf_load_library('cornetto12'),
	rdf_load_library('wn20-basic'),
	rdf_load_library('wn30-basic'),
	% rdf_load('http://www.ontologyportal.org/SUMO.owl'),
	% rdf_load('http://catalog.clarin.eu/isocat/rest/dcs/119.rdf'),
	% rdf_load('http://www.lexinfo.net/lmf'),
	% rdf_load('cornetto/rdf/cornetto21-schema.ttl'),
	% rdf_load('cornetto/rdf/cornetto12-synsets.ttl.gz', [graph(cornetto12)]),
	rdf_assert(lemon:value, rdfs:subPropertyOf, rdf:value, schema_hack),
	true.

:- initialization			% run *after* loading this file
	rdf_set_cache_options([ global_directory('cache/rdf'),
				create_global_directory(true)
			      ]),
	% load_ontologies,
	true.

:- debug(xmlrdf).

load :-
	load_lmf,
	true.

load_lmf :-
	% LmfFile = 'lmf100k.xml',
	LmfFile = 'cornetto-lmf.xml.gz',
	absolute_file_name(data(LmfFile), File,
			   [ access(read)
			   ]),
	rdf_current_ns(lmf, Prefix),
	load_xml_as_rdf(File,
			[ dialect(xml),
			  unit(['LexicalEntry','Synset']),
			  prefix(Prefix),
			  predicate_style(one_two),
			  graph(lmf)
			]).

clean :-
	clean_lmf.

clean_all :-
	clean_input,
	clean.

clean_input :-
	rdf_unload_graph(schema),
	rdf_unload_graph(lmf),
	rdf_unload_graph(lu),
	rdf_unload_graph(sy).

save :-
	save_lmf.

post_process:-
	post_process_lmf.

rewrite :-
	rewrite_lmf.

run_cornetto :-
	load,
	rewrite,
	post_process,
	save.

rdf_label:display_label_hook(Synset, _Lang, Label) :-
	rdf(LexicalSense, lemon:reference, Synset),
	rdf_display_label(LexicalSense, LULabel),
	!,
	format(atom(Label), 'synset ~w', [LULabel]).
rdf_label:display_label_hook(LexEnt, _Lang, Label) :-
	rdf_has(LexEnt, lemon:lexicalForm, LF),
	rdf_has(LF, lemon:representation, RP),
	rdf_display_label(RP, LELabel),
	!,
	format(atom(Label), 'entry "~w"', [LELabel]).
rdf_label:display_label_hook(LexForm, _Lang, Label) :-
	rdf_has(LexForm, lemon:representation, LF),
	rdf_display_label(LF, LFLabel),
	!,
	format(atom(Label), 'form "~w"', [LFLabel]).
rdf_label:display_label_hook(LexSense, _Lang, Label) :-
	rdf_has(LexSense, lemon:reference, _),
	rdf_has(LexSense, rdfs:label, LF),
	rdf_display_label(LF, LSLabel),
	(   rdf_has(LexSense, lemon:definition,  Def)
	->  rdf_display_label(Def, DefLabel)
	;   DefLabel = 'no def'
	),
	!,
	format(atom(Label), '"~w" (~w)', [LSLabel, DefLabel]).
cliopatria:context_graph(LemonElement, RDF) :-
	(   rdf_has(LemonElement, lemon:representation, _Representation)
	;   rdf_has(LemonElement, lemon:sense, _Sense)
	;   rdf_has(_, lemon:sense, LemonElement)
	),
        findall(T, lexical_context_triple(LemonElement, T), RDF0),
        sort(RDF0, RDF1),
        minimise_graph(RDF1, RDF2),             % remove inverse/symmetric/...
        bagify_graph(RDF2, RDF3, Bags, []),     % Create bags of similar resources
        append(RDF3, Bags, RDF),
        RDF \= [].

lexical_context_triple(LF, rdf(LF, RP, Representation)) :-
	rdf_has(LF, lemon:representation, Representation, RP).
lexical_context_triple(LF, rdf(LE, RP, LF)) :-
	rdf_has(LE, lemon:lexicalForm, LF, RP).
lexical_context_triple(LF, rdf(LE, RP, LS)) :-
	rdf_has(LE, lemon:lexicalForm, LF),
	rdf_has(LE, lemon:sense, LS, RP).
lexical_context_triple(LE, rdf(LE, RP, LS)) :-
	rdf_has(LE, lemon:sense, LS, RP).
lexical_context_triple(LS, rdf(LE, RP, LS)) :-
	rdf_has(LE, lemon:sense, LS, RP).
lexical_context_triple(LS, rdf(SS, RP, LS)) :-
	rdf_has(LS, lemon:reference, SS, RP).
lexical_context_triple(SS, rdf(SS, RP, LE)) :-
	rdf_has(LS, lemon:reference, SS, RP),
	rdf_has(LE, lemon:sense, LS).

cliopatria:context_graph(CornettoExample, RDF) :-
	(   rdf_has(CornettoExample, corn21s:form, _Bnode)
	;   fail
	),
        findall(T, cornetto_example_triple(CornettoExample, T), RDF0),
        sort(RDF0, RDF1),
        minimise_graph(RDF1, RDF2),             % remove inverse/symmetric/...
        bagify_graph(RDF2, RDF3, Bags, []),     % Create bags of similar resources
        append(RDF3, Bags, RDF),
        RDF \= [].

cornetto_example_triple(E, rdf(E,P,L)) :-
	rdf_equal(corn21s:form, P),
	rdf(E, P, F),
	rdf(F, rdf:value, L).
