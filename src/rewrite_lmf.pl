:- module(rewrite_lmf,
	  [ rewrite_lmf/0,
	    rewrite_lmf/1,
	    rewrite_lmf/2,
	    list_rules_lmf/0,

	    save_lmf/0,
	    clean_lmf/0
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(xmlrdf/rdf_rewrite)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(lmf_uris).

:- debug(rdf_rewrite).
:- debug(wordnet).

result_directory(rdf).

rewrite_lmf :-rdf_rewrite(lmf).
rewrite_lmf(Rule) :- rdf_rewrite(lmf, Rule).
rewrite_lmf(Graph, Rule) :- rdf_rewrite(Graph, Rule).
list_rules_lmf :-rdf_rewrite_rules.

:- discontiguous
	rdf_mapping_rule/5.

:- multifile
	rdf_mapping_rule/5.
:- [rewrite_lmf_form].
:- [rewrite_lmf_entry].
:- [rewrite_lmf_sense].
:- [rewrite_lmf_sense_pragmatics].
:- [rewrite_lmf_sense_examples].
:- [rewrite_lmf_synset_relations].
:- [rewrite_lmf_wn_mapping].

clean_lmf:-
	clear_domain_cache,
	rdf_unload_graph(form),
	rdf_unload_graph(entry),
	rdf_unload_graph(sense),
	rdf_unload_graph(synset),
	rdf_unload_graph(synset_relations),
	rdf_unload_graph(sense_examples),
	rdf_unload_graph(pragmatics),

	rdf_unload_graph(replacedBy),
	rdf_unload_graph(hub_new),
	rdf_unload_graph(hub_old),
	rdf_unload_graph(hub_cp),

	rdf_unload_graph(wn20),
	rdf_unload_graph(wn30),
	rdf_unload_graph(domainspec),
	rdf_unload_graph(domains),

	rdf_unload_graph(internal_edoal),
	rdf_unload_graph(edoal_internal),
	rdf_unload_graph(edoal_wn30),
	rdf_unload_graph(edoal_wn20),

	rdf_unload_graph(synset_types),
	rdf_unload_graph(synset_labels),
	rdf_unload_graph(inscheme),
	rdf_unload_graph(synset_defs),

	rdf_unload_graph(entry_sense),
	rdf_unload_graph(entry_form),
	rdf_unload_graph(sense_entry),
	rdf_unload_graph(sense_label),
	rdf_unload_graph(sense_id),
	rdf_unload_graph(sense_synset),
	rdf_unload_graph(synset_sense),
	rdf_unload_graph(sumo),
	rdf_unload_graph(lexical_unit),
	rdf_unload_graph(lexical_form),
	true.

save_lmf:-
	% save(sumo, 'cornetto-sumo-mapping.ttl'),
	save(replacedBy, 'cornetto21-cornetto12-mapping.ttl'),
	save(hub_new,    'cornetto21-hub-mapping.ttl'),
	save(hub_old,    'cornetto12-hub-mapping.ttl'),
	save(hub_cp,    'cornetto-hub-synsets.ttl'),

	save(edoal_internal, 'cornetto21-edoal-synset-relations.ttl'),
	save(edoal_wn20,     'cornetto21-edoal-wn20-mapping.ttl'),
	save(edoal_wn30,     'cornetto21-edoal-wn30-mapping.ttl'),

	save(wn20, 'cornetto21-wn20-mapping.ttl'),
	save(wn30, 'cornetto21-wn30-mapping.ttl'),

	save(domainspec, 'cornetto21-wordnet-domainspec.ttl'),
	save(domains,    'cornetto21-wordnet-domains.ttl'),

	save(synset,     'cornetto21-synsets.ttl'),
	save(entry,      'cornetto21-entries.ttl'),
	save(form,       'cornetto21-lexforms.ttl'),
	save(sense,      'cornetto21-senses.ttl'),

	save(pragmatics,       'cornetto21-sense-pragmatics.ttl'),
	save(sense_examples,   'cornetto21-sense-examples.ttl'),
	save(synset_relations, 'cornetto21-synset-relations.ttl'),

	save(synset_types, 'cornetto21-synset-types.ttl'),
	save(synset_labels,'cornetto21-synset-skos-labels.ttl'),
	save(synset_defs,  'cornetto21-synset-skos-definitions.ttl'),
	save(inscheme,	   'cornetto21-synset-skos-inscheme.ttl'),


	true.

save(Graph, File) :-
	result_directory(Directory),
	directory_file_path(Directory, File, Path),
	rdf_save_canonical_turtle(Path, [graph(Graph)]).

