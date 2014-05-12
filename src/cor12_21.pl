:- module(cornetto12to21, [
			   replacedBy/0,
			   newhubs/0
			  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_library)).

:- use_module(lmf_uris).

:- rdf_meta
	pos2code(r, ?).

:- debug(replacedBy).

run :-
	load_synsets,
	replacedBy,
	newhubs.

load_synsets:-
	rdf_attach_library('..'),
	rdf_load_library(cornetto12),
	rdf_load_library(cornetto21Synsets),
	rdf_load_library(cornetto21SkosMapping).

replacedBy :-
	findall(S, synset(cornetto12, S), Old),
	forall(member(S, Old), map_old_to_new(S)).

newhubs :-
	findall(S, synset(cornetto21, S), New),
	forall(member(S, New), map_hub_to_new(S)).

synset(Lex, S) :-
	lexicon(Lex, Scheme),
	rdf(S, skos:inScheme, Scheme).

map_old_to_new(OldSynset) :-
	rdf(OldSynset, rdf:type, Type),
	rdf(OldSynset, cornetto:id, literal(OldId)),
	pos2code(Type, PosCode),
	atomic_list_concat(['nld-21-', OldId, '-', PosCode], NewId),
	(   rdf(NewSynset, corn21s:id, literal(NewId))
	->  rdf_global_id(corn21i:NewLocal,  NewSynset),
	    rdf_global_id(cornetto:NewLocal, HubSynset),
	    rdf_assert(OldSynset, dcterms:isReplacedBy, NewSynset, replacedBy),
	    rdf_assert(HubSynset, dcterms:hasVersion,   OldSynset, hub_old)
	;   debug(replacedBy, 'No replacement for ~p ~w', [OldSynset, OldId])
	).

map_hub_to_new(NewSynset) :-
	rdf_global_id(corn21i:NewLocal, NewSynset),
	rdf_global_id(cornetto:NewLocal, HubSynset),
	copy_new_to_hub(NewSynset, HubSynset),
	rdf_assert(HubSynset, dcterms:hasVersion,  NewSynset, hub_new).

copy_new_to_hub(New, Hub):-
	rdf_transaction(forall(rdf(New,P,O),
			       rdf_assert(Hub, P, O, hub_cp)
			      )
		       ).

pos2code(cornetto:'AdjectiveSynset', a).
pos2code(cornetto:'AdverbSynset',    r).
pos2code(cornetto:'NounSynset',      n).
pos2code(cornetto:'VerbSynset',      v).
pos2code(cornetto:'Synset',          o).
