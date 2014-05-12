:- module(cornetto, [
		     convert/0,	% Convert synsets to RDF
		     save/0	% Save to ../data
		    ]).

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdf_zlib_plugin')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_turtle_write')).
:- use_module(library('semweb/rdf_turtle')).

:- use_module(library(broadcast)).
:- use_module(library(sgml)).
:- use_module(library(pce_progress)).
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(debug)).

:- use_module(xrdfutil).
:- use_module(xml2rdf).
:- use_module(rdf_convert).
:- use_module(create_scheme).

:- rdf_register_ns(rma, 'http://e-culture.multimedian.nl/ns/rijksmuseum#').
:- rdf_register_ns(owl, 'http://www.w3.org/2002/07/owl#').
:- rdf_register_ns(ic, 'http://e-culture.multimedian.nl/ns/iconclass#').
:- rdf_register_ns(vra, 'http://www.vraweb.org/vracore/vracore3#').
:- rdf_register_ns(wn20s, 'http://www.w3.org/2006/03/wn/wn20/schema/').
:- rdf_register_ns(wn20i, 'http://www.w3.org/2006/03/wn/wn20/instances/').
:- rdf_register_ns(wn30, 'http://purl.org/vocabularies/princeton/wn30/').
:- rdf_register_ns(cornetto, 'http://purl.org/vocabularies/cornetto/').

user:file_search_path(collections, '../../RDF/collections/').
user:file_search_path(vocabularies, '../../RDF/vocabularies/').
user:file_search_path(newvocs, '../../vocs/').
user:file_search_path(cornetto,	   newvocs('cornetto')).
user:file_search_path(wn_w3c,	   newvocs('w3c-wordnet')).
user:file_search_path(wn30,	   newvocs('wordnet/rdf')).


input(cornetto, [cornetto('xml/v1_2_cdb_syn_wn30.xml')]).

ns(skos, 'http://www.w3.org/2004/02/skos/core#').
ns(wn20s, 'http://www.w3.org/2006/03/wn/wn20/schema/').
ns(wn20i, 'http://www.w3.org/2006/03/wn/wn20/instances/').
ns(cornetto, 'http://purl.org/vocabularies/cornetto/').
skos_scheme('http://purl.org/vocabularies/cornetto').


:-dynamic
        local_name_cache/1,
	domain_cache/2.

clear_caches :-
	retractall(local_name_cache(_)),
	retractall(domain_cache(_,_)).


convert(Document, N) :-
	progress_dialog(D),
	(   N == all
	->  Options = []
	;   Options = [max_units(N)]
	),
	input(Document, Files),
	ns(Document, NS),
	forall(nth1(At, Files, File),
	       (   absolute_file_name(File, FileName,
				      [ access(read)
				      ]),
		   xml_file_to_rdf(FileName,
				   [ unit('cdb_synset'),
				     about_hook(uri),
				     class_hook(class),
				     property_hook(property),
				     skip_hook(skip_property),
				     select_by_label(select_by_label),
				     target_namespace(NS),
				     db(Document)
				   | Options
				   ]),
		   broadcast(progress(files, at(At)))
	       )
	      ),
	send(D, destroy).

progress_dialog(D) :-
	new(B1, listening_progress_bar(files, files)),
	new(B2, listening_progress_bar(progress, xml2rdf)),
	new(D, progress_dialog('Processing XML to RDF',
			       B1, B2)),
	send(D, open_centered).


align :-
	format('Loading W3C Wordnet 2.0 for alignment... ~n'), flush,
	rdf_load(wn_w3c('wordnet-synset.rdf')),
	format('Loading	VU Wordnet 3.0 for alignment... ~n'), flush,
	rdf_load(wn30('wordnet-synset.ttl')),
	rdf_transaction(link_to_wordnet).

link_to_wordnet:-
	rdf_has(S, cornetto:externalRelation, literal(Id), P),
	sub_atom(Id,3,2,_,Voc),
	sub_atom(Id,6,_,2,Sub),
	sub_atom(Id,_,1,0,Pos),
	pos2w3c(Pos, Prefix),
	atom_concat(Prefix,Sub,W3CId),
	(   Voc = '20', rdf(W3Csynset, wn20s:synsetId, literal(lang(_,W3CId)))
	->  Map	= wn20map
	;   Voc = '30', rdf(W3Csynset, wn30:synsetId, literal(type(_,W3CId)))
	->  Map = wn30map
	;   debug(convert:w3c, 'target Wordnet synset not found: ~w ~w~n', [Voc,W3CId]),
	    fail
	),
	debug(convert:verbose, '~nAttributes:~w~nValues:~w~n', [W3Csynset,Id]),
	rdf_assert(S,P,W3Csynset,Map),
	rdf_retractall(S,P,literal(Id)),
	fail.


link_to_wordnet :- !.

domains :-
	rdf_transaction(do_domains).

fix_relations :-
	format('Loading Cornetto RDF Schema to replace literal IDs in all internal relations by URIs ... ~n'), flush,
	rdf_load(cornetto('rdf/cornetto-schema.ttl')),
	rdf_transaction(fix_internal_cornetto_relations).

fix_internal_cornetto_relations:-
	rdf_has(S, cornetto:internalRelation, Id, P),
	rdf(URI, cornetto:id, Id),
	rdf_assert(S,P,URI,relations),
	rdf_retractall(S,P,Id),
	fail.

fix_internal_cornetto_relations :- !.

do_domains:-
	rdf(S, cornetto:domain, literal(D), Graph),
	mint_domain(D,URI),
	rdf_assert(S, cornetto:domain, URI, Graph),
	rdf_retractall(S, cornetto:domain, literal(D), Graph),
	fail.

do_domains:- !.


mint_domain(Domain, URI) :-
	domain_cache(Domain, URI),!.

mint_domain(Domain, URI) :-
	make_legal_xml_name(Domain, Local),
	ns(wn20i, NS),
	atomic_list_concat([NS, 'domain-', Local], URI),
	rdf_assert(URI, rdf:type, wn20s:'WordnetDomain', domains),
	rdf_assert(URI, skos:prefLabel, literal(lang(en, Domain)), domains),
	assert(domain_cache(Domain, URI)),!.

save :-
	rdf_save_canonical_turtle('data/duplicates.ttl',         [graph(duplicates),  encoding(utf8)]),
	rdf_save_canonical_turtle('data/cycles.ttl',             [graph(cycles),      encoding(utf8)]),
	rdf_save_canonical_turtle('data/wordnet-domains.ttl',    [graph(domains),     encoding(utf8)]),
	rdf_save_canonical_turtle('data/cornetto-synsets.ttl',   [graph(cornetto),    encoding(utf8)]),
	rdf_save_canonical_turtle('data/cornetto-relations.ttl', [graph(relations),   encoding(utf8)]),
	rdf_save_canonical_turtle('data/cornetto-wn30.ttl',      [graph(wn30map),     encoding(utf8)]),
	rdf_save_canonical_turtle('data/cornetto-wn20.ttl',      [graph(wn20map),     encoding(utf8)]).


convert :- cornetto(all).

cornetto(N) :-
	clear_caches,
	rdf_retractall(_,_,_,cornetto),
	rdf_retractall(_,_,_,relations),
	rdf_retractall(_,_,_,domains),
	rdf_retractall(_,_,_,wn20map),
	rdf_retractall(_,_,_,cycles),
	rdf_retractall(_,_,_,duplicates),

	rdf_discard,
	convert(cornetto, N),
	rdf_commit,

	fix_relations,
	domains,
	align,
	rdf_retractall(_,cornetto:hasHyponym,_,relations).


pos2type(Noun, Class):-
	sub_atom(Noun, 0, 4,_,'NOUN'),
	!,
	rdf_equal(cornetto:'NounSynset', Class).

pos2type(Verb, Class):-
	sub_atom(Verb, 0, 4,_,'VERB'),
	!,
	rdf_equal(cornetto:'VerbSynset', Class).

pos2type(Adj, Class) :-
	sub_atom(Adj, 0, 9, _, 'ADJECTIVE'),
	!,
	rdf_equal(cornetto:'AdjectiveSynset', Class).

pos2type('ADVERB', Class) :-
	rdf_equal(cornetto:'AdverbSynset', Class).

pos2type(Pos,Class) :- !,
	debug(pos2typ, 'No RDFS class for ~w~n', [Pos]),
	rdf_equal(cornetto:'Synset', Class).

pos2w3c(n,1).
pos2w3c(v,2).
pos2w3c(a,3).
pos2w3c(b,4).
pos2w3c(r,4).

pos2label(Noun, noun):-sub_atom(Noun, 0, 4,_,'NOUN'),!.

pos2label(Verb, verb):-
	sub_atom(Verb, 0, 4,_,'VERB'),
	!.

pos2label(Adj, adjective) :-
	sub_atom(Adj, 0, 9, _, 'ADJECTIVE'),
	!.

pos2label('ADVERB', adverb) :- !.

pos2label(_,'') :- !.

literal_of(L, literal(L)).

/************************
*     SPECIFICATION     *
************************/

%%	class(+DOM, -Class, +Ctx)
%
%	Generate a classname for an element

class(element('cdb_synset', Attr, Children), Class, Ctx) :-
	memberchk(posSpecific=PS, Attr),
	pos2type(PS,Class),
	debug(convert:verbose, '~w ~w ~w~n',[Attr,Children,Ctx]).

%%	uri(+DOM, -URI, +Ctx)
%
%	`Invent' a URI for the  given  object.


uri(element('cdb_synset', Attributes, Content), URI, Ctx) :-
	get_assoc(db, Ctx, Document),
	memberchk(c_sy_id=Id, Attributes),
	memberchk(element(synonyms, _, SynsElem), Content),
	memberchk(element(synonym, SAttr, _), SynsElem),
	memberchk('c_lu_id-previewtext'=Preview, SAttr),
	(   memberchk(posSpecific=PosSpecific, Attributes)
	->  pos2label(PosSpecific, PosLabel)
	;   PosLabel='unknownpos'
	),
	make_local_name(Preview, PosLabel, Id, Localname),
	ns(Document, NS),
	atom_concat(NS, Localname, URI),   % Change this to nice purl namespace and w3c-like id
	rdf_assert(URI, cornetto:id, literal(Id), Document),
	skos_scheme(Scheme),
	rdf_assert(URI, skos:inScheme, Scheme, Document),
	(   memberchk(posSpecific=PosSpecific, Attributes)
	->  rdf_assert(URI, cornetto:posSpecific, literal(lang(en, PosSpecific)), Document)
	),
	!.

uri(element('cdb_synset', Attributes, _Content), _ ,_):-
	memberchk(c_sy_id=Id, Attributes),
	debug(convert, 'uri/3 failed for ~w', [Id]),!.

spyme.

% The generic call of Property/6
%

property(differentiae, _, _, Values, cornetto:differentiae, literal(lang(nl,Txt))) :-
	Values = [Txt].
property(definition, _, _, Values, cornetto:definition, literal(lang(nl,Txt))) :-
	Values = [Txt].
property(base_concept, _, _, _, rdf:type, Term) :-
	rdf_equal(cornetto:'BaseConcept', Term).

property(synonym, _Context, Attr, _Values, cornetto:synonym, Term) :-
	memberchk('c_lu_id-previewtext'=L, Attr),
	sub_atom(L,_,_,2,Label),   % Fix, can also be :10 etc
	Term=literal(lang(nl, Label)).

property(dom_relation, Context, Attr, _Values, cornetto:domain, DomainLiteralList) :-
	member(element(wn_domains, _,_), Context),
	memberchk(term=SpaceSeparatedDomains, Attr),
	\+ SpaceSeparatedDomains = '',
	atomic_list_concat(Domains, ' ', SpaceSeparatedDomains),
	maplist(literal_of, Domains, DomainLiteralList).

property(relation, Context, Attr, _Values, Relation, Ids) :-
	member(element(wn_equivalence_relations, _,_), Context),
	memberchk(relation_name=RelationName, Attr),
	(   RelationName='EQ_NEAR_SYNONYM'
	->  rdf_equal(cornetto:eqNearSynonym, Relation)
	;   RelationName='EQ_BE_IN_STATE'
	->  rdf_equal(cornetto:eqBeInState, Relation)
	;   RelationName='EQ_IS_STATE_OF'
	->  rdf_equal(cornetto:isStateOf, Relation)
	;   RelationName='EQ_CAUSES'
	->  rdf_equal(cornetto:eqCauses, Relation)
	;   RelationName='EQ_CO_ROLE'
	->  rdf_equal(cornetto:eqCoRole, Relation)
	;   RelationName='EQ_HAS_HYPERNYM'
	->  rdf_equal(cornetto:eqHasHypernym, Relation)
	;   RelationName='EQ_HAS_HYPERONYM'
	->  rdf_equal(cornetto:eqHasHyperonym, Relation)
	;   RelationName='EQ_HAS_HYPONYM'
	->  rdf_equal(cornetto:eqHasHyponym, Relation)
	;   RelationName='EQ_HAS_HOLONYM'
	->  rdf_equal(cornetto:eqHasHolonym, Relation)
	;   RelationName='EQ_HAS_SUBEVENT'
	->  rdf_equal(cornetto:eqSubEvent, Relation)
	;   RelationName='EQ_IS_SUBEVENT_OF'
	->  rdf_equal(cornetto:eqIsSubEventOf, Relation)
	;   RelationName='EQ_INVOLVED'
	->  rdf_equal(cornetto:eqInvolved, Relation)
	;   RelationName='EQ_HAS_MERONYM'
	->  rdf_equal(cornetto:eqHasMeronym, Relation)
	;   RelationName='EQ_ROLE'
	->  rdf_equal(cornetto:eqRole, Relation)
	;   RelationName='EQ_SYNONYM'
	->  rdf_equal(cornetto:eqSynonym, Relation)
	;   RelationName='EQ_IS_CAUSED_BY'
	->  rdf_equal(cornetto:eqIsCausedBy, Relation)
	;   RelationName=''
	->  fail
	;   debug(convert, 'equivalent relation "~w" not mapped [~w]', [RelationName, Attr]),
	    fail
	),
	findall(literal(Id),
		(   member(T=Id, Attr),
		    sub_atom(T,0,6,2,target) % Find T=target20, target30, target16
		),
		Ids),
	true.

property(relation, Context, Attr, _Values, Relation, literal(Target)  ) :-
	member(element(wn_internal_relations,_,_), Context),
	memberchk(relation_name=RelationName, Attr),
	(   RelationName='HAS_HYPERONYM'
	->  rdf_equal(cornetto:hasHyperonym , Relation)
	;   RelationName='HAS_HYPONYM'
	->  rdf_equal(cornetto:hasHyponym, Relation) %  inverse of hyponym
	;   RelationName='HAS_HOLO_PART'
	->  rdf_equal(cornetto:hasHoloPart, Relation)
	;   RelationName='HAS_MERO_PART'
	->  rdf_equal(cornetto:hasMeroPart, Relation) % inverse of holo part
	;   RelationName='HAS_HOLO_MEMBER'
	->  rdf_equal(cornetto:hasHoloMember, Relation)
	;   RelationName='HAS_MERO_MEMBER'
	->  rdf_equal(cornetto:hasMeroMember, Relation) % inverse of holo member
	;   RelationName='HAS_HOLO_MADEOF'
	->  rdf_equal(cornetto:hasHoloMadeOf, Relation)
	;   RelationName='HAS_MERO_MADEOF'
	->  rdf_equal(cornetto:hasMeroMadeOf, Relation) % inverse of holo made of
	;   RelationName='HAS_HOLONYM'
	->  rdf_equal(cornetto:hasHolonym, Relation)
	;   RelationName='HAS_MERONYM'
	->  rdf_equal(cornetto:hasMeronym, Relation) % inverse of has holonym

	;   RelationName='BE_IN_STATE'
	->  rdf_equal(cornetto:beInState, Relation)
	;   RelationName='CAUSES'
	->  rdf_equal(cornetto:causes, Relation)
	;   RelationName='IS_CAUSED_BY'
	->  rdf_equal(cornetto:isCausedBy, Relation) % inverse of causes
	;   RelationName='CO_ROLE'
	->  rdf_equal(cornetto:coRole, Relation)
	;   RelationName='CO_AGENT_INSTRUMENT'
	->  rdf_equal(cornetto:coAgentInstrument, Relation)
	;   RelationName='CO_AGENT_PATIENT'
	->  rdf_equal(cornetto:coAgentPatient, Relation)
	;   RelationName='CO_AGENT_RESULT'
	->  rdf_equal(cornetto:coAgentResult, Relation)
	;   RelationName='CO_INSTRUMENT_AGENT'
	->  rdf_equal(cornetto:coInstrumentAgent, Relation)
	;   RelationName='CO_INSTRUMENT_PATIENT'
	->  rdf_equal(cornetto:coInstrumentPatient, Relation)
	;   RelationName='CO_INSTRUMENT_RESULT'
	->  rdf_equal(cornetto:coInstrumentResult, Relation)
	;   RelationName='CO_PATIENT_AGENT'
	->  rdf_equal(cornetto:coPatientAgent, Relation)
	;   RelationName='CO_PATIENT_INSTRUMENT'
	->  rdf_equal(cornetto:coPatientInstrument, Relation)
	;   RelationName='CO_RESULT_AGENT'
	->  rdf_equal(cornetto:coResultAgent, Relation)
	;   RelationName='CO_RESULT_INSTRUMENT'
	->  rdf_equal(cornetto:coResultInstrument, Relation)
	;   RelationName='HAS_HOLO_LOCATION'
	->  rdf_equal(cornetto:locationMeronymOf, Relation)
	;   RelationName='HAS_MERO_LOCATION'
	->  rdf_equal(cornetto:hasMeroLocation, Relation) %  inverse of meronym
	;   RelationName='HAS_HOLO_PORTION'
	->  rdf_equal(cornetto:portionMeronymOf, Relation)
	;   RelationName='HAS_MERO_PORTION'
	->  rdf_equal(cornetto:hasMeroPortion, Relation) % inverse of meronym
	;   RelationName='HAS_XPOS_HYPERONYM'
	->  rdf_equal(cornetto:hasXposHyperonym, Relation)
	;   RelationName='HAS_XPOS_HYPONYM'
	->  rdf_equal(cornetto:hasXposHyponym, Relation)
	;   RelationName='IN_MANNER'
	->  rdf_equal(cornetto:inManner, Relation)
	;   RelationName='INVOLVED'
	->  rdf_equal(cornetto:involved, Relation)
	;   RelationName='INVOLVED_AGENT'
	->  rdf_equal(cornetto:involvedAgent, Relation)
	;   RelationName='INVOLVED_DIRECTION'
	->  rdf_equal(cornetto:involvedDirection, Relation)
	;   RelationName='INVOLVED_INSTRUMENT'
	->  rdf_equal(cornetto:involvedInstrument, Relation)
	;   RelationName='INVOLVED_LOCATION'
	->  rdf_equal(cornetto:involvedLocation, Relation)
	;   RelationName='INVOLVED_PATIENT'
	->  rdf_equal(cornetto:involvedPatient, Relation)
	;   RelationName='INVOLVED_RESULT'
	->  rdf_equal(cornetto:involvedResult, Relation)
	;   RelationName='INVOLVED_SOURCE_DIRECTION'
	->  rdf_equal(cornetto:involvedSourceDirection, Relation)
	;   RelationName='INVOLVED_TARGET_DIRECTION'
	->  rdf_equal(cornetto:involvedTargetDirection, Relation)


	;   RelationName='IS_SUBEVENT_OF'
	->  rdf_equal(cornetto:isSubEventOf, Relation)
	;   RelationName='HAS_SUBEVENT'
	->  rdf_equal(cornetto:hasSubEvent, Relation) % inverse of isSubEventOf ???

	;   RelationName='MANNER_OF'
	->  rdf_equal(cornetto:mannerOf, Relation)
	;   RelationName='NEAR_ANTONYM'
	->  rdf_equal(cornetto:nearAntonym, Relation)
	;   RelationName='NEAR_SYNONYM'
	->  rdf_equal(cornetto:nearSynonym, Relation)
	;   RelationName='ROLE'
	->  rdf_equal(cornetto:role, Relation)
	;   RelationName='ROLE_AGENT'
	->  rdf_equal(cornetto:roleAgent, Relation)
	;   RelationName='ROLE_DIRECTION'
	->  rdf_equal(cornetto:roleDirection, Relation)
	;   RelationName='ROLE_INSTRUMENT'
	->  rdf_equal(cornetto:roleInstrument, Relation)
	;   RelationName='ROLE_LOCATION'
	->  rdf_equal(cornetto:roleLocation, Relation)
	;   RelationName='ROLE_PATIENT'
	->  rdf_equal(cornetto:rolePatient, Relation)
	;   RelationName='ROLE_RESULT'
	->  rdf_equal(cornetto:roleResult, Relation)
	;   RelationName='ROLE_SOURCE_DIRECTION'
	->  rdf_equal(cornetto:roleSourceDirection, Relation)
	;   RelationName='ROLE_TARGET_DIRECTION'
	->  rdf_equal(cornetto:roleTargetDirection, Relation)
	;   RelationName='STATE_OF'
	->  rdf_equal(cornetto:stateOf, Relation)

	;   RelationName='FUZZYNYM'
	->  rdf_equal(cornetto:fuzzynym, Relation)
	;   RelationName='XPOS_FUZZYNYM'
	->  rdf_equal(cornetto:xposFuzzynym, Relation)
	;   RelationName='XPOS_NEAR_ANTONYM'
	->  rdf_equal(cornetto:xposNearAntonym, Relation)
	;   RelationName='XPOS_NEAR_SYNONYM'
	->  rdf_equal(cornetto:xposNearSynonym, Relation)
	;   debug(convert, 'internal relation ~w not mapped', RelationName),
	    fail
	),
	(   memberchk(target=Target, Attr)
	->  true
	;   debug(convert, 'Relation conversion failed for ~w', [Attr])
	).


%%	skip_property(+Property)
%
%	Succeeds if XML tag should be skipped

skip_property('dummy').

make_local_name(Preview, Pos, Id , Local) :-
	atomic_list_concat([synset, '-', Preview, '-', Pos], TheLabel),
        make_legal_xml_name(TheLabel, Legal),
        (   local_name_cache(Legal)
        ->  debug(convert, 'Oops, duplicate label for ~w (~w)', [Id, TheLabel]),
	    atomic_list_concat([Legal,'-',Id], Local)
	;   Legal = Local
        ),
        assert(local_name_cache(Local)).

make_legal_xml_name(PrefLabel, Local):-
        atom_chars(PrefLabel, CharList),
        make_legal_list(CharList, LegalCharList),
        LegalCharList = [First|_],
        (   (char_type(First, alpha) | First = '_')
        ->  Prefix=''
        ;   Prefix=aat_concept_
        ),
        atomic_list_concat([Prefix|LegalCharList], Local).

make_legal_list([],[]).
make_legal_list([H|T], [H|LegalTail]) :-
        (   char_type(H, alnum) | H = '-'),
        !,
        make_legal_list(T,LegalTail).
make_legal_list([':'|T], ['-'|LegalTail]) :-
        make_legal_list(T,LegalTail).

make_legal_list([_|T], ['_'|LegalTail]) :-
        make_legal_list(T,LegalTail).

