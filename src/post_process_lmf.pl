:- module(post_process_lmf,
	  [
	      post_process_lmf/0,
	      isocat_usage/0
	  ]).

:- debug(synset).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(lmf_uris).
:- use_module(cor12_21).

:- rdf_meta
	le_sy_pos(r,r),
	code_sy_pos(r,r).


post_process_lmf :-
	complete_edoal_graphs,
	enrich_synsets,
	replacedBy,
	newhubs.

complete_edoal_graphs :-
	complete_wn_edoal_graphs(edoal_wn20, wn20),
	complete_wn_edoal_graphs(edoal_wn30, wn30),
	complete_internal_edoal_graphs.

complete_internal_edoal_graphs :-
	internal_relations_edoal(Alignment),
	lexicon(cornetto21, Lexicon),
	Graph = edoal_internal,
	rdf_assert(Alignment, rdf:type,    align:'Alignment', Graph),
	rdf_assert(Alignment, align:onto1, Lexicon,           Graph),
	rdf_assert(Alignment, align:onto2, Lexicon,           Graph),
	rdf_assert(Alignment, align:level, literal('0'),      Graph),
	rdf_assert(Alignment, align:xml,   literal('no'),     Graph),
	rdf_assert(Alignment, align:type,  literal('**'),     Graph),
	true.

complete_wn_edoal_graphs(Graph, Lex) :-
	lexicon(Lex, WordNet),
	lexicon(cornetto21,Lexicon),
	wn_relations_edoal(Lex, Alignment),
	rdf_assert(Alignment, rdf:type,    align:'Alignment', Graph),
	rdf_assert(Alignment, align:onto1, Lexicon,           Graph),
	rdf_assert(Alignment, align:onto2, WordNet,           Graph),
	rdf_assert(Alignment, align:level, literal('0'),      Graph),
	rdf_assert(Alignment, align:xml,   literal('no'),     Graph),
	rdf_assert(Alignment, align:type,  literal('**'),     Graph),
	true.

enrich_synsets:-
	findall(Syn, is_synset(Syn), Syns),
	assign_synset_defs(Syns),
	assign_synset_labels(Syns),
	assign_synset_types(Syns).

is_synset(Synset) :-
	rdf(Synset, lemon:isReferenceOf, _LS, synset).

assign_synset_types([]).
assign_synset_types([Synset|Tail]) :-
	rdf_equal(corn21s:'synset-unknown_000', Synset),
	assign_synset_types(Tail).
assign_synset_types([Synset|Tail]) :-
	findall(Type, find_synset_type(Synset, Type), Types),
	sort(Types, SortedTypes),
	length(SortedTypes, NTypes),
	(   NTypes \== 1
	->  debug(synset, 'Type Error for synset ~p ~p', [Synset, SortedTypes]),
	    find_synset_type_from_id(Synset, Type)
	;   Types = [Type|_]
	),
	rdf_assert(Synset, rdf:type, Type, synset_types),
	assign_synset_types(Tail).

assign_synset_labels([]).
assign_synset_labels([Synset|Tail]) :-
	rdf_equal(corn21s:'synset-unknown_000', Synset),
	assign_synset_labels(Tail).
assign_synset_labels([Synset|Tail]) :-
	findall(Label, find_synset_label(Synset, Label), Labels),
	forall(member(Label, Labels),
	       rdf_assert(Synset, skos:altLabel, Label, synset_labels)
	      ),
	lexicon(cornetto21,Scheme),
	rdf_assert(Synset, skos:inScheme, Scheme, inscheme),
	assign_synset_labels(Tail).

assign_synset_defs([]).
assign_synset_defs([Synset|Tail]) :-
	rdf_equal(corn21s:'synset-unknown_000', Synset),
	assign_synset_defs(Tail).

assign_synset_defs([Synset|Tail]) :-
	findall(Label, find_synset_defs(Synset, Label), Labels),
	forall(member(Label, Labels),
	       rdf_assert(Synset, skos:definition, Label, synset_defs)
	      ),
	assign_synset_defs(Tail).

find_synset_label(Synset, Label) :-
	rdf(LS, lemon:reference, Synset),
	rdf(LE, lemon:sense, LS),
	rdf(LE, lemon:canonicalForm, WF),
	rdf(WF, lemon:writtenRep, Label).

find_synset_type(Synset, Type) :-
	rdf(LS, lemon:reference, Synset),
	rdf(LE, lemon:sense, LS),
	rdf(LE, corn21s:partOfSpeech, PoS),
	le_sy_pos(PoS, Type).

find_synset_defs(Synset, literal(lang(nl,DefLabels))) :-
	findall(L, find_synset_def(Synset, L), Labels),
	Labels \= [],
	sort(Labels, Unique),
	atomic_list_concat(Unique, '; ', DefLabels).

find_synset_def(Synset, DefLabelTxt) :-
	rdf(LS,  lemon:reference, Synset),
	rdf(LS,  lemon:definition, Def),
	rdf(Def, lemon:value, DefLabel),
	literal_text(DefLabel, DefLabelTxt).

find_synset_type_from_id(Synset, Type) :-
	rdf(Synset, corn21s:id, literal(Id)),
	atomic_list_concat(IdList, '-', Id),
	last(IdList, Pos),
	code_sy_pos(Pos, Type).

le_sy_pos(corn21s:adjective, corn21s:'AdjectiveSynset').
le_sy_pos(corn21s:adverb, corn21s:'AdverbSynset').
le_sy_pos(corn21s:noun, corn21s:'NounSynset').
le_sy_pos(corn21s:verb, corn21s:'VerbSynset').
le_sy_pos(corn21s:other, corn21s:'Synset').

code_sy_pos(a, corn21s:'AdjectiveSynset').
code_sy_pos(r, corn21s:'AdverbSynset').
code_sy_pos(n, corn21s:'NounSynset').
code_sy_pos(v, corn21s:'VerbSynset').
code_sy_pos(o, corn21s:'Synset').
code_sy_pos(unknown_000, corn21s:'Synset').

isocat_usage :-
	findall(I-C, rdf(C, 'http://isocat.org/ns/dcr.rdf#datcat', I), Pairs),
	keysort(Pairs, Sorted),
	open('isocat_usage.txt', write, S),
	isocat_usage(S, Sorted),
	close(S).

isocat_usage(_, []) :- !.
isocat_usage(S, [K-V| Tail]) :-
	format(S, '~p ~p~n', [K, V]),
	isocat_usage(S, Tail).
