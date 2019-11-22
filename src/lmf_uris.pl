:- module(rewrite_lmf_uris,
	  [ lexicon/2,
	    target_lexicon/1,
	    instance_namespace/2,
	    ls_uri/2,
	    le_uri/2,
	    le_id_uri/2,
	    wn_uri/3,
	    ex_uri/2,
	    synset_uri/2,
	    irel_uri/2,
	    formType/2,
	    pos_type/2,
	    form_length_type/2,
	    source_of_prag/2,
	    prag_bnode/2,
	    adverbial_usage_map/2,
	    internal_relations_edoal/1,
	    wn_relations_edoal/2,
	    mint_domain/2,
	    clear_domain_cache/0
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(xmlrdf/rdf_convert_util)).

:- rdf_meta
	wn_uri(+, r, r),
	form_length_type(+, r),
	formType(+, r),
	irel_uri(+, r).

		 /*******************************
		 *	       URI		*
		 *******************************/
internal_relations_edoal(URI) :-
	rdf_global_id(corn21r:'synset_relations', URI).

wn_relations_edoal(Lex, URI) :-
	rdf_global_id(corn21r:Lex, URI).

target_lexicon(odwn13).
instance_namespace(odwn13, odwn13i).
instance_namespace(cornetto21, corn21i).
lexicon(cornetto21, 'http://purl.org/vocabularies/cornetto/21/').
lexicon(cornetto12, 'http://purl.org/vocabularies/cornetto').
lexicon(odwn13,     'http://purl.org/vocabularies/odwn13/').
lexicon(wn20,       'http://www.w3.org/2006/03/wn/wn20/').
lexicon(wn30, Lex) :- rdf_equal(wn30:'', Lex).

lmf_literal_to_id(List, Namespace, IRI) :-
	Options = [accents(disable), underscores(disable)],
	literal_to_id(List, Namespace, IRI, Options).

ls_uri(LU, URI) :-
	rdf(LU, lmf:id, literal(Id)),
	target_lexicon(Lexicon),
	instance_namespace(Lexicon, Namespace),
	lmf_literal_to_id(['sense-', Id], Namespace, URI).

ex_uri(Ex, URI) :-
	rdf(Ex, lmf:id, literal(Id)),
	target_lexicon(Lexicon),
	instance_namespace(Lexicon, Namespace),
	lmf_literal_to_id(['ex-', Id], Namespace, URI).

le_uri(LU, URI) :-
	rdf(LU, lmf:id, literal(Id)),
	le_id_uri(Id, URI).

le_id_uri(Id, URI) :-
	target_lexicon(Lexicon),
	instance_namespace(Lexicon, Namespace),
	lmf_literal_to_id(['entry-', Id], Namespace, URI).


synset_uri(unknown_000, _URI):-
	!,
	fail.

synset_uri(Id, URI):-
	target_lexicon(Lexicon),
	instance_namespace(Lexicon, Namespace),
	(Lexicon = cornetto21
	->      atomic_list_concat([_Lang, _Version|IdList], -, Id), % Strip of the nld-21- prefix
		atomic_list_concat(IdList, -, Id2)
	;	Id2 = Id
	),
	lmf_literal_to_id(['synset-', Id2], Namespace, URI).

wn_uri(ID, Scheme, Target_URI) :-
	atomic_list_concat([_Lang, _Version, Num ,PosTag], -, ID),
	pos2w3c(PosTag,Prefix),
	atom_concat(Prefix, Num, WNId),
	(   rdf(Target_URI, wn20s:synsetId, literal(WNId)),
	    rdf(Target_URI, skos:inScheme, Scheme) % ids are not unique...
	->  true
	;   debug(wordnet, 'Cannot find ~p URI for ~q (~q)', [Scheme, ID, WNId])
	).

pos2w3c(n,1).
pos2w3c(v,2).
pos2w3c(a,3).
pos2w3c(b,4).
pos2w3c(r,4).

% FIXME: check for PoS 'other'
pos_type(Form, URL) :-
	rdf_global_id(corn21s:Form, URL).

% pragmatics are defined on senses and sense examples:
source_of_prag(LMFPrag, SenseURI) :-
	rdf(LMFSense, lmf:pragmatics, LMFPrag),
	rdf(LmfLE, lmf:sense, LMFSense),!,
	ls_uri(LmfLE, SenseURI).
source_of_prag(LMFPrag, SenseExampleURI) :-
	rdf(LMFSenseExample, lmf:pragmatics, LMFPrag),
	ex_uri(LMFSenseExample, SenseExampleURI).

formType(vp,	corn21s:'VerbPhraseStructure').
formType(np,	corn21s:'NounPhraseStructure').
formType(s,	corn21s:'SentenceStructure').
formType(pp,	corn21s:'PPStructure').
formType('',	corn21s:'UndefinedStructure').
formType(ap,	corn21s:'APStructure').
formType(' vp ',corn21s:'VerbPhraseStructure').
formType(' np ',corn21s:'NounPhraseStructure').
formType(' s ',	corn21s:'SentenceStructure').
formType(ad,	corn21s:'ADStructure').
formType(' pp ',corn21s:'PPStructure').
formType(' ap ',corn21s:'APStructure').
formType('S',	corn21s:'SentenceStructure').
formType(pv,    corn21s:'PVStructure').

irel_uri('',		              corn21s:undefinedExtRelation). % FIXME
irel_uri('BE_IN_STATE',		      corn21s:beInState).
irel_uri('CAUSES',		      corn21s:causes).
irel_uri('CO_AGENT_INSTRUMENT',	      corn21s:coAgentInstrument).
irel_uri('CO_AGENT_PATIENT',	      corn21s:coAgentPatient).
irel_uri('CO_AGENT_RESULT',	      corn21s:coAgentResult).
irel_uri('CO_INSTRUMENT_AGENT',	      corn21s:coInstrumentAgent).
irel_uri('CO_INSTRUMENT_PATIENT',     corn21s:coInstrumentPatient).
irel_uri('CO_INSTRUMENT_RESULT',      corn21s:coInstrumentResult).
irel_uri('CO_PATIENT_AGENT',	      corn21s:coPatientAgent).
irel_uri('CO_PATIENT_INSTRUMENT',     corn21s:coPatientInstrument).
irel_uri('CO_PATIENT_RESULT',	      corn21s:coPatientResult).
irel_uri('CO_RESULT_AGENT',	      corn21s:coResultAgent).
irel_uri('CO_RESULT_INSTRUMENT',      corn21s:coResultInstrument).
irel_uri('CO_RESULT_PATIENT',	      corn21s:coResultPatient).
irel_uri('CO_ROLE',		      corn21s:coRole).
irel_uri('EQ_BE_IN_STATE',	      corn21s:eqBeInState).
irel_uri('EQ_CAUSES',		      corn21s:eqCauses).
irel_uri('EQ_CO_ROLE',		      corn21s:eqCoRole).
irel_uri('EQ_HAS_HOLONYM',	      corn21s:eqHasHolonym).
irel_uri('EQ_HAS_HYPERNYM',	      corn21s:eqHasHyperonym). % !!
irel_uri('EQ_HAS_HYPERONYM',	      corn21s:eqHasHyperonym).
irel_uri('EQ_HAS_HYPONYM',	      corn21s:eqHasHyponym).
irel_uri('EQ_HAS_MERONYM',	      corn21s:eqHasMeronym).
irel_uri('EQ_HAS_SUBEVENT',	      corn21s:eqHasSubEvent).
irel_uri('EQ_INVOLVED',		      corn21s:eqInvolved).
irel_uri('EQ_IS_CAUSED_BY',	      corn21s:eqIsCausedBy).
irel_uri('EQ_IS_STATE_OF',	      corn21s:eqIsStateOf).
irel_uri('EQ_IS_SUBEVENT_OF',	      corn21s:eqIsSubEventOf).
irel_uri('EQ_NEAR_SYNONYM',	      corn21s:eqNearSynonym).
irel_uri('EQ_ROLE',		      corn21s:eqRole).
irel_uri('EQ_SYNONYM',		      corn21s:eqSynonym).
irel_uri('FUZZYNYM',		      corn21s:fuzzynym).
irel_uri('HAS_HOLONYM',		      corn21s:hasHolonym).
irel_uri('HAS_HOLO_LOCATION',	      corn21s:hasHoloLocation).
irel_uri('HAS_HOLO_MADEOF',	      corn21s:hasHoloMadeOf).
irel_uri('HAS_HOLO_MEMBER',	      corn21s:hasHoloMember).
irel_uri('HAS_HOLO_PART',	      corn21s:hasHoloPart).
irel_uri('HAS_HOLO_PORTION',	      corn21s:hasHoloPortion).
irel_uri('HAS_HYPERONYM',	      corn21s:hasHyperonym).
irel_uri('HAS_MERONYM',		      corn21s:hasMeronym).
irel_uri('HAS_MERO_LOCATION',	      corn21s:hasMeroLocation).
irel_uri('HAS_MERO_MADEOF',	      corn21s:hasMeroMadeOf).
irel_uri('HAS_MERO_MEMBER',	      corn21s:hasMeroMember).
irel_uri('HAS_MERO_PART',	      corn21s:hasMeroPart).
irel_uri('HAS_MERO_PORTION',	      corn21s:hasMeroPortion).
irel_uri('HAS_SUBEVENT',	      corn21s:hasSubEvent).
irel_uri('HAS_XPOS_HYPERONYM',	      corn21s:hasXposHyperonym).
irel_uri('HAS_XPOS_HYPONYM',	      corn21s:hasXposHyponym).
irel_uri('INVOLVED',		      corn21s:involved).
irel_uri('INVOLVED_AGENT',	      corn21s:involvedAgent).
irel_uri('INVOLVED_DIRECTION',	      corn21s:involvedDirection).
irel_uri('INVOLVED_INSTRUMENT',	      corn21s:involvedInstrument).
irel_uri('INVOLVED_LOCATION',	      corn21s:involvedLocation).
irel_uri('INVOLVED_PATIENT',	      corn21s:involvedPatient).
irel_uri('INVOLVED_RESULT',	      corn21s:involvedResult).
irel_uri('INVOLVED_SOURCE_DIRECTION', corn21s:involvedSourceDirection).
irel_uri('INVOLVED_TARGET_DIRECTION', corn21s:involvedTargetDirection).
irel_uri('IN_MANNER',		      corn21s:inManner).
irel_uri('IS_CAUSED_BY',	      corn21s:isCausedBy).
irel_uri('IS_SUBEVENT_OF',	      corn21s:isSubEventOf).
irel_uri('MANNER_OF',		      corn21s:mannerOf).
irel_uri('NEAR_ANTONYM',	      corn21s:nearAntonym).
irel_uri('NEAR_SYNONYM',	      corn21s:nearSynonym).
irel_uri('ROLE',		      corn21s:role).
irel_uri('ROLE_AGENT',		      corn21s:roleAgent).
irel_uri('ROLE_DIRECTION',	      corn21s:roleDirection).
irel_uri('ROLE_INSTRUMENT',	      corn21s:roleInstrument).
irel_uri('ROLE_LOCATION',	      corn21s:roleLocation).
irel_uri('ROLE_PATIENT',	      corn21s:rolePatient).
irel_uri('ROLE_RESULT',		      corn21s:roleResult).
irel_uri('ROLE_SOURCE_DIRECTION',     corn21s:roleSourceDirection).
irel_uri('ROLE_TARGET_DIRECTION',     corn21s:roleTargetDirection).
irel_uri('STATE_OF',		      corn21s:stateOf).
irel_uri('XPOS_FUZZYNYM',	      corn21s:xposFuzzynym).
irel_uri('XPOS_NEAR_ANTONYM',	      corn21s:xposNearAntonym).
irel_uri('XPOS_NEAR_SYNONYM',	      corn21s:xposNearSynonym).
irel_uri('be_in_state',		      corn21s:beInState).
irel_uri('causes',		      corn21s:causes).
irel_uri('co_agent_instrument',	      corn21s:coAgentInstrument).
irel_uri('co_agent_patient',	      corn21s:coAgentPatient).
irel_uri('co_agent_result',	      corn21s:coAgentResult).
irel_uri('co_instrument_agent',	      corn21s:coInstrumentAgent).
irel_uri('co_instrument_patient',     corn21s:coInstrumentPatient).
irel_uri('co_instrument_result',      corn21s:coInstrumentResult).
irel_uri('co_patient_agent',	      corn21s:coPatientAgent).
irel_uri('co_patient_instrument',     corn21s:coPatientInstrument).
irel_uri('co_patient_result',	      corn21s:coPatientResult).
irel_uri('co_result_agent',	      corn21s:coResultAgent).
irel_uri('co_result_instrument',      corn21s:coResultInstrument).
irel_uri('co_result_patient',	      corn21s:coResultPatient).
irel_uri('co_role',		      corn21s:coRole).
irel_uri('eq_be_in_state',	      corn21s:eqBeInState).
irel_uri('eq_causes',		      corn21s:eqCauses).
irel_uri('eq_co_role',		      corn21s:eqCoRole).
irel_uri('eq_has_holonym',	      corn21s:eqHasHolonym).
irel_uri('eq_has_hypernym',	      corn21s:eqHasHyperonym). % !!
irel_uri('eq_has_hyperonym',	      corn21s:eqHasHyperonym).
irel_uri('eq_has_hyponym',	      corn21s:eqHasHyponym).
irel_uri('eq_has_meronym',	      corn21s:eqHasMeronym).
irel_uri('eq_has_subevent',	      corn21s:eqHasSubEvent).
irel_uri('eq_involved',		      corn21s:eqInvolved).
irel_uri('eq_is_caused_by',	      corn21s:eqIsCausedBy).
irel_uri('eq_is_state_of',	      corn21s:eqIsStateOf).
irel_uri('eq_is_subevent_of',	      corn21s:eqIsSubEventOf).
irel_uri('eq_near_synonym',	      corn21s:eqNearSynonym).
irel_uri('eq_role',		      corn21s:eqRole).
irel_uri('eq_synonym',		      corn21s:eqSynonym).
irel_uri('fuzzynym',		      corn21s:fuzzynym).
irel_uri('has_holonym',		      corn21s:hasHolonym).
irel_uri('has_holo_location',	      corn21s:hasHoloLocation).
irel_uri('has_holo_madeof',	      corn21s:hasHoloMadeOf).
irel_uri('has_holo_member',	      corn21s:hasHoloMember).
irel_uri('has_holo_part',	      corn21s:hasHoloPart).
irel_uri('has_holo_portion',	      corn21s:hasHoloPortion).
irel_uri('has_hyperonym',	      corn21s:hasHyperonym).
irel_uri('has_hyponym',   	      corn21s:hasHyponym).
irel_uri('has_meronym',		      corn21s:hasMeronym).
irel_uri('has_mero_location',	      corn21s:hasMeroLocation).
irel_uri('has_mero_madeof',	      corn21s:hasMeroMadeOf).
irel_uri('has_mero_member',	      corn21s:hasMeroMember).
irel_uri('has_mero_part',	      corn21s:hasMeroPart).
irel_uri('has_mero_portion',	      corn21s:hasMeroPortion).
irel_uri('has_subevent',	      corn21s:hasSubEvent).
irel_uri('has_xpos_hyperonym',	      corn21s:hasXposHyperonym).
irel_uri('has_xpos_hyponym',	      corn21s:hasXposHyponym).
irel_uri('involved',		      corn21s:involved).
irel_uri('involved_agent',	      corn21s:involvedAgent).
irel_uri('involved_direction',	      corn21s:involvedDirection).
irel_uri('involved_instrument',	      corn21s:involvedInstrument).
irel_uri('involved_location',	      corn21s:involvedLocation).
irel_uri('involved_patient',	      corn21s:involvedPatient).
irel_uri('involved_result',	      corn21s:involvedResult).
irel_uri('involved_source_direction', corn21s:involvedSourceDirection).
irel_uri('involved_target_direction', corn21s:involvedTargetDirection).
irel_uri('in_manner',		      corn21s:inManner).
irel_uri('is_caused_by',	      corn21s:isCausedBy).
irel_uri('is_subevent_of',	      corn21s:isSubEventOf).
irel_uri('manner_of',		      corn21s:mannerOf).
irel_uri('near_antonym',	      corn21s:nearAntonym).
irel_uri('near_synonym',	      corn21s:nearSynonym).
irel_uri('role',		      corn21s:role).
irel_uri('role_agent',		      corn21s:roleAgent).
irel_uri('role_direction',	      corn21s:roleDirection).
irel_uri('role_instrument',	      corn21s:roleInstrument).
irel_uri('role_location',	      corn21s:roleLocation).
irel_uri('role_patient',	      corn21s:rolePatient).
irel_uri('role_result',		      corn21s:roleResult).
irel_uri('role_source_direction',     corn21s:roleSourceDirection).
irel_uri('role_target_direction',     corn21s:roleTargetDirection).
irel_uri('state_of',		      corn21s:stateOf).
irel_uri('xpos_fuzzynym',	      corn21s:xposFuzzynym).
irel_uri('xpos_near_antonym',	      corn21s:xposNearAntonym).
irel_uri('xpos_near_synonym',	      corn21s:xposNearSynonym).
%irel_uri('[',			      corn21s:[).
%irel_uri(']',			      corn21s:]).
irel_uri('agent',		      corn21s:agent).
irel_uri('attribute',		      corn21s:attribute).
irel_uri('causes',		      corn21s:causes).
irel_uri('component',		      corn21s:component).
irel_uri('contains',		      corn21s:contains).
irel_uri('finishes',		      corn21s:finishes).
irel_uri('instance',		      corn21s:instance).
irel_uri('instrument',		      corn21s:instrument).
irel_uri('located',		      corn21s:located).
irel_uri('manner',		      corn21s:manner).
irel_uri('member',		      corn21s:member).
irel_uri('name',		      corn21s:name).
irel_uri('origin',		      corn21s:origin).
irel_uri('parallel',		      corn21s:parallel).
irel_uri('part',		      corn21s:part).
irel_uri('partlyLocated',	      corn21s:partlyLocated).
irel_uri('path',		      corn21s:path).
irel_uri('patient',		      corn21s:patient).
irel_uri('possesses',		      corn21s:possesses).
irel_uri('property',		      corn21s:property).
irel_uri('resource',		      corn21s:resource).
irel_uri('result',		      corn21s:result).
irel_uri('sibling',		      corn21s:sibling).
irel_uri('subname',		      corn21s:subname).
irel_uri('time',		      corn21s:time).
irel_uri('uses',		      corn21s:uses).

form_length_type(full,         corn21s:'Full').
form_length_type(shortening,   corn21s:'Shortening').
form_length_type(abbreviation, corn21s:'Abbreviation').
form_length_type(acronym,      corn21s:'Acronym').
form_length_type(contraction,  corn21s:'Contraction').
form_length_type(origshort,    corn21s:'Origshort').
form_length_type('',           corn21s:'UnknownLengthType').

:- dynamic
	domain_cache/2.

clear_domain_cache :-
	retractall(domain_cache(_,_)).

mint_domain(Domain, URI) :-
	domain_cache(Domain, URI),!.

mint_domain(Domain, URI) :-
	lmf_literal_to_id(['domain-', Domain], corn21s, URI),
	rdf_assert(URI, rdf:type, corn21s:'WordnetDomain', domainspec),
	rdf_assert(URI, skos:prefLabel, literal(lang(en, Domain)), domainspec),
	assert(domain_cache(Domain, URI)), !.

adverbial_usage_map(yes, true).
adverbial_usage_map(no,  false).

prag_bnode(LmfBnode, LemonBnode) :-
	atom_concat(LmfBnode, '_pragmatics', LemonBnode).
