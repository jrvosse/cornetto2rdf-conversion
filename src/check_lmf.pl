:- module(check_lmf_conversion, [check_lmf/0]).

:- use_module(library(semweb/rdf_label)).

check_lmf :-
	check_pos_counts.

check_pos_counts:-
	find_pos_entries(xml, XMLEntries),
	find_pos_entries(rdf, RDFEntries),
	compare_entries(XMLEntries, RDFEntries).

find_pos_entries(Type, Grouped) :-
	findall(Pos-Entry, entry(Type, Pos, Entry), Pairs),
	keysort(Pairs, Sorted),
	group_pairs_by_key(Sorted, Grouped),
	report_pos_counts(Type, Grouped).

entry(xml, Pos, (URI-Entry)) :-
	rdf(Entry, rdf:type, lmf:'LexicalEntry'),
	rdf(Entry, lmf:part_of_speech, literal(Pos)),
	rdf(Entry, lmf:id, literal(Id)),
	rewrite_lmf:le_id_uri(Id, URI).

entry(rdf, Pos, Entry) :-
	rdf(Entry, dcpos:partOfSpeech, PosResource),
	rdf_display_label(PosResource, Pos).

report_pos_counts(_, []).
report_pos_counts(Report, [H|T]) :-
	H = Id-List,
	length(List, Count),
	format('~w ~p: ~d ~n', [Report, Id, Count]),
	report_pos_counts(Report, T).

compare_entries([],_).
compare_entries([H1|T1], [H2|T2]) :-
	H1=Id-List1,
	H2=Id-List2,
	length(List1, Length1),
	length(List2, Length2),
	(   Length1 == Length2
	->  format('Equal counts form ~w: ~d~n', [Id, Length1])
	;   sort(List1, List1S),
	    sort(List2, List2S),
	    compare_pos(List1S, List2S)
	),
	compare_entries(T1, T2).

compare_pos([],_).
compare_pos([H1|T1], [H2|T2]) :-
	H1 = (URI1-Entry1),
	(   URI1 == H2
	->  compare_pos(T1, T2)
	;   format('Missing: ~p bnode: ~p~n', [URI1, Entry1]),
	    compare_pos(T1, [H2|T2])
	).

count_mappings:-
	findall(M, mapping(M), MUs),
	msort(MUs, Ms),
	length(Ms, N),
	format('Mapping count ~d~n', N),
	find_duplicates(Ms).

mapping(SourceId-Name-WN30Id) :-
	rdf(Source, lmf:id, literal(SourceId)),
	rdf(Source, lmf:monolingual_external_refs, MerList),
	rdf(MerList, lmf:monolingual_external_ref, MER),
	rdf(MER, lmf:external_system, literal('pwn-30')),
	rdf(MER, lmf:rel_type, literal(Name)),
	rdf(MER, lmf:external_reference, literal(WN30Id)).


find_duplicates([]).
find_duplicates([_]).
find_duplicates([H1,H2|Tail]) :-
	(   H1 == H2
	->  format('duplicate found: ~w~n', [H1])
	;   true
	),
	find_duplicates([H2|Tail]).



