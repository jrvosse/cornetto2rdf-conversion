internal_relation @@
 { Source, lmf:synset_relations, RList },
 { Source, lmf:id, literal(SourceId) },
 { RList, lmf:synset_relation, Rel },
 { Rel, lmf:rel_type, literal(Name) },
 { Rel, lmf:target, literal(TargetId) }
 ==>
 irel_uri(Name, Pred),
 synset_uri(SourceId, Source_URI),
 synset_uri(TargetId, Target_URI),
 { Source_URI, Pred, Target_URI } >> synset_relations.

internal_relation_edoal @@
{ Source, lmf:synset_relations, RList },
{ Source, lmf:id, literal(SourceId) },
{ RList, lmf:synset_relation, Rel },
{ Rel,  lmf:meta, Meta },
{ Meta, lmf:author, literal(Author) },
{ Meta, lmf:date, literal(Date) },
{ Meta, lmf:confidence, literal(Confidence) },
{ Rel,  lmf:rel_type, literal(Name) },
{ Rel,  lmf:target, literal(TargetId) }
==>
internal_relations_edoal(Alignment) ,
synset_uri(SourceId, Source_URI),
synset_uri(TargetId, Target_URI),
rdf_bnode(Cell),
{ Alignment, align:map, Cell } >> edoal_internal ,
{ Cell, align:entity1, Source_URI } >> edoal_internal ,
{ Cell, align:entity2, Target_URI } >> edoal_internal ,
{ Cell, align:measure, Confidence^^xsd:integer } >> edoal_internal ,
{ Cell, align:relation, literal(Name) } >> edoal_internal ,
{ Cell, dc:creator, literal(Author) } >> edoal_internal ,
{ Cell, dc:date, literal(Date) } >> edoal_internal .
