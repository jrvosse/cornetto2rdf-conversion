wn30_mapping @@
 { MER, lmf:external_system, literal('pwn-30') },
 { MER, lmf:rel_type, literal(Name) },
 { MER, lmf:external_reference, literal(WN30Id) },
 { MerList, lmf:monolingual_external_ref, MER },
 { Source, lmf:monolingual_external_refs, MerList },
 { Source, lmf:id, literal(SourceId) }
 ==>
 synset_uri(SourceId, SS_URI),
 irel_uri(Name, Pred),
 wn_uri(WN30Id, wn30:'', Target_URI),
 { SS_URI, Pred, Target_URI } >> wn30.

wn20_mapping @@
 { MER, lmf:external_system, literal('pwn-20') },
 { MER, lmf:rel_type, literal(Name) },
 { MER, lmf:external_reference, literal(WN20Id) } ,
 { MerList, lmf:monolingual_external_ref, MER } ,
 { Source, lmf:monolingual_external_refs, MerList } ,
 { Source, lmf:id, literal(SourceId) } 
 ==>
 irel_uri(Name, Pred),
 synset_uri(SourceId, SS_URI),
 wn_uri(WN20Id, 'http://www.w3.org/2006/03/wn/wn20/', Target_URI),
 { SS_URI, Pred, Target_URI } >> wn20.

domains @@
 { MER, lmf:external_system, literal('wordnet_domain') },
 { MerList, lmf:monolingual_external_ref, MER },
 { Source, lmf:monolingual_external_refs, MerList },
 { Source, lmf:id, literal(SourceId) },
 { MER, lmf:external_reference, literal(Term) }
 ==>
 Term \== '',
 mint_domain(Term, DomainURI),
 synset_uri(SourceId, SS_URI),
 { SS_URI, lemon:propertyDomain, DomainURI } >> domains.

wn30_mapping_edoal @@
 { MER, lmf:external_system, literal('pwn-30') },
 { MerList, lmf:monolingual_external_ref, MER },
 { Source,  lmf:monolingual_external_refs, MerList },
 { Source,  lmf:id, literal(SourceId) },
 { MER,  lmf:meta, Meta },
 { Meta, lmf:author, literal(Author) },
 { Meta, lmf:date, literal(Date) },
 { Meta, lmf:confidence, literal(Confidence) },
 { MER, lmf:rel_type, literal(Name) },
 { MER, lmf:external_reference, literal(WN30Id) }
 ==>
 internal_relations_edoal(Alignment) ,
 synset_uri(SourceId, Source_URI),
 wn_uri(WN30Id, wn30:'', Target_URI),
 rdf_bnode(Cell),
 { Alignment, align:map, Cell } >> edoal_wn30 ,
 { Cell, align:entity1, Source_URI } >> edoal_wn30 ,
 { Cell, align:entity2, Target_URI } >> edoal_wn30 ,
 { Cell, align:measure, Confidence^^xsd:integer } >> edoal_wn30 ,
 { Cell, align:relation, literal(Name) } >> edoal_wn30 ,
 { Cell, dc:creator, literal(Author) } >> edoal_wn30 ,
 { Cell, dc:date, literal(Date) } >> edoal_wn30 .

wn20_mapping_edoal @@
 { MER, lmf:external_system, literal('pwn-20') },
 { MerList, lmf:monolingual_external_ref, MER },
 { Source,  lmf:monolingual_external_refs, MerList },
 { Source,  lmf:id, literal(SourceId) },
 { MER,  lmf:meta, Meta },
 { Meta, lmf:author, literal(Author) },
 { Meta, lmf:date, literal(Date) },
 { Meta, lmf:confidence, literal(Confidence) },
 { MER, lmf:rel_type, literal(Name) },
 { MER, lmf:external_reference, literal(WN20Id) }
 ==>
 internal_relations_edoal(Alignment) ,
 synset_uri(SourceId, Source_URI),
 wn_uri(WN20Id, 'http://www.w3.org/2006/03/wn/wn20/', Target_URI),
 rdf_bnode(Cell),
 { Alignment, align:map, Cell } >> edoal_wn20 ,
 { Cell, align:entity1, Source_URI } >> edoal_wn20 ,
 { Cell, align:entity2, Target_URI } >> edoal_wn20 ,
 { Cell, align:measure, Confidence^^xsd:integer } >> edoal_wn20 ,
 { Cell, align:relation, literal(Name) } >> edoal_wn20 ,
 { Cell, dc:creator, literal(Author) } >> edoal_wn20 ,
 { Cell, dc:date, literal(Date) } >> edoal_wn20 .
