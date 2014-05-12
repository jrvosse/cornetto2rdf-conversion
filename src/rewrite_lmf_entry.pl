% All lexical entries have Id and sense information:
entryBasics @@
 { LmfLE, lmf:id, Id},
 { LmfLE, lmf:sense, _ }
 ==>
 ls_uri(LmfLE, LS),
 le_uri(LmfLE, LE),
 { LE, lemon:sense, LS } >> entry,
 { LS, rdf:type, lemon:'LexicalSense' } >>  sense,
 { LE, corn21s:id, Id } >>  entry.

% Rules for info not present on all entries:
partOfSpeech @@
 { LmfLE, lmf:part_of_speech, literal(PoS) }
 ==>
 le_uri(LmfLE, LE),
 pos_type(PoS, PosType),
 { LE, corn21s:partOfSpeech, PosType } >>  entry.

gender @@
 { LmfLE, lmf:morpho_syntax, MS },
 { MS, lmf:pronominal_and_grammatical_gender, literal(Gender)}
 ==>
 rdf_global_id(corn21s:Gender, GenderType),
 le_uri(LmfLE, URI),
 { URI, corn21s:gender, GenderType } >> entry.

auxiliary @@
 { LmfLE, lmf:morpho_syntax, MS },
 { MS, lmf:auxiliaries, Auxs} ,
 { Auxs, lmf:auxiliary , literal(Aux) }
 ==>
 le_uri(LmfLE, URI),
 { URI, corn21s:auxilary, Aux@nl } >> entry.

adverbial_usage @@
 { MS, lmf:adverbial_usage, literal(AU)},
 { LmfLE, lmf:morpho_syntax, MS }
 ==>
 adverbial_usage_map(AU, Boolean),
 le_uri(LmfLE, URI),
 { URI, corn21s:adverbialUsage, Boolean^^xsd:boolean } >> entry.

position @@
 { MS, lmf:position, literal(L)},
 { LmfLE, lmf:morpho_syntax, MS }
 ==>
 le_uri(LmfLE, URI),
 rdf_global_id(corn21s:L, Object),
 { URI, corn21s:position, Object } >> entry.

le_form_length_type @@
 { LmfLE, lmf:form_type, literal(Cat) }
 ==>
 form_length_type(Cat, FormType),
 le_uri(LmfLE, URI),
 { URI, corn21s:abbreviatedForm, FormType } >> entry.
