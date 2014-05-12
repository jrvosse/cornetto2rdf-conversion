prag_domain @@
 { LMFPrag, lmf:domains, Dom },
 { Dom, lmf:domain, literal(Domain) }
 ==>
 source_of_prag(LMFPrag, SenseURI),
 prag_bnode(LMFPrag, P),
 mint_domain(Domain, URI),
 { SenseURI, corn21s:pragmatics, P } >> pragmatics,
 { P, rdf:type, corn21s:'Pragmatics' } >> pragmatics,
 { P, corn21s:domain, URI } >> pragmatics.

geography @@
 { LMFPrag, lmf:geography, literal(Geo) }
 ==>
 source_of_prag(LMFPrag, SenseURI),
 prag_bnode(LMFPrag, P),
 rdf_global_id(corn21s:Geo, Object), 
 { SenseURI, corn21s:pragmatics, P } >> pragmatics,
 { P, rdf:type, corn21s:'Pragmatics' } >> pragmatics,
 { P, corn21s:geography, Object } >> pragmatics.

chronology @@
 { LMFPrag, lmf:chronology, literal(Chronology) }
 ==>
 source_of_prag(LMFPrag, SenseURI),
 rdf_global_id(corn21s:Chronology, Object), 
 prag_bnode(LMFPrag, P),
 { SenseURI, corn21s:pragmatics, P } >> pragmatics,
 { P, rdf:type, corn21s:'Pragmatics' } >> pragmatics,
 { P, corn21s:chronology, Object } >> pragmatics.

connotation @@
 { LMFPrag, lmf:connotation, literal(Connotation) }
 ==>
 source_of_prag(LMFPrag, SenseURI),
 prag_bnode(LMFPrag, P),
 rdf_global_id(corn21s:Connotation, Object), 
  { SenseURI, corn21s:pragmatics, P } >> pragmatics,
 { P, rdf:type, corn21s:'Pragmatics' } >> pragmatics,
 { P, corn21s:connotation, Object } >> pragmatics.

register @@
 { LMFPrag, lmf:register, literal(Register) }
 ==>
 source_of_prag(LMFPrag, SenseURI),
 prag_bnode(LMFPrag, P),
 rdf_global_id(corn21s:Register, Object), 
 { SenseURI, corn21s:pragmatics, P } >> pragmatics,
 { P, rdf:type, corn21s:'Pragmatics' } >> pragmatics,
 { P, corn21s:register, Object } >> pragmatics.
