senseId @@
 { LmfLE, lmf:sense, Sense } ,
  { Sense, lmf:id, LUId },	 % e.g. r_v-184 (cornetto lex unit id)
 { Sense, lmf:sense_id, SenseId }	 % e.g. 1,2)
 ==>
 ls_uri(LmfLE, LS),
 { LS, corn21s:id, LUId } >> sense,
 { LS, corn21s:senseId, SenseId } >> sense.

sense_synset @@
 { LmfLE, lmf:sense, Sense } ,
 { Sense, lmf:synset, literal(SynId) }   % e.g. d_v-145 (synset id)
 ==>
 ls_uri(LmfLE, LS),
 synset_uri(SynId, Synset),
 { LS, lemon:reference, Synset } >> sense,
 { Synset, corn21s:id, literal(SynId) } >> synset,
 { Synset, lemon:isReferenceOf, LS } >> synset.

senseDef @@
 { LmfLE, lmf:sense, Sense } ,
 { Sense, lmf:definition, literal(Def) }
 ==>
 Def \== '',
 ls_uri(LmfLE, LS),
 rdf_bnode(Bnode),
 { LS, lemon:definition, Bnode } >> sense,
 { Bnode, rdf:type, lemon:'SenseDefinition' } >> sense,
 { Bnode, lemon:value, Def@nl } >> sense.

commonProper @@
 { LmfLE, lmf:sense, Sense } ,
 { Sense, lmf:semantics_noun, SN } ,
 { SN, lmf:reference, literal(Ref) }
 ==>
 ls_uri(LmfLE, LS),
 rdf_global_id(corn21s:Ref, NounType),
 { LS, corn21s:commonProper, NounType } >> sense.

countability @@
 { SN, lmf:countability, literal(Ref) } ,
 { Sense, lmf:semantics_noun, SN } ,
 { LmfLE, lmf:sense, Sense }
 ==>
 ls_uri(LmfLE, LS),
 rdf_global_id(corn21s:Ref, CountType),
 { LS, corn21s:countability, CountType } >> sense.

semanticNounType @@
 { SN, lmf:semantic_type, literal(Ref) } ,
 { Sense, lmf:semantics_noun, SN } ,
 { LmfLE, lmf:sense, Sense }
 ==>
 ls_uri(LmfLE, LS),
 rdf_global_id(corn21s:Ref, SType),
 { LS, corn21s:semanticNounType, SType } >> sense.

semanticAdjType @@
 { SN, lmf:semantic_type, literal(Ref) } ,
 { Sense, lmf:semantics_adjective, SN } ,
 { LmfLE, lmf:sense, Sense }
 ==>
 ls_uri(LmfLE, LS),
 rdf_global_id(corn21s:Ref, SType),
 { LS, corn21s:semanticAdjectiveType, SType } >> sense.

semanticVerbType @@
 { ST, lmf:semantic_type, literal(Ref) } ,
 { SV, lmf:semantic_types, ST } ,
 { Sense, lmf:semantics_verb, SV } ,
 { LmfLE, lmf:sense, Sense }
 ==>
 ls_uri(LmfLE, LS),
 rdf_global_id(corn21s:Ref, SType),
 { LS, corn21s:semanticVerbType, SType } >> sense.

polarity @@
 { Sent, lmf:polarity, literal(PosNeg) } ,
 { Sense, lmf:sentiment, Sent } ,
 { LmfLE, lmf:sense, Sense }
 ==>
 ls_uri(LmfLE, LS),
 rdf_global_id(corn21s:PosNeg, Object),
 { LS, corn21s:polarity, Object } >> sense.

sense_relation @@
 { Rlist, lmf:sense_group, Group } ,
 { Group, lmf:relation_type, literal(Rtype) },
 { Group, lmf:target_sense_id, literal(TargetId) } ,
 { Sense, lmf:sense_relations, Rlist } ,
 { LmfLE, lmf:sense, Sense }
 ==>
 ls_uri(LmfLE, LS),
 rdf(Target_URI, corn21s:id, literal(TargetId)),
 rdf_global_id(corn21s:Rtype, Pred),
 { LS, Pred, Target_URI} >> sense.
