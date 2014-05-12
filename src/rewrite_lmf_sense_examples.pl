sense_example @@
 { ExList, lmf:sense_example, SenseExample },
 { SenseExample, lmf:canonical_form, CanonicalNode },
 { CanonicalNode, lmf:canonicalform, CanonicalForm },
 { CanonicalNode, lmf:expression_type, literal(ExpressionId) },
 { CanonicalNode, lmf:phrase_type, literal(FormCategory) },
 { Sense, lmf:sense_examples, ExList },
 { LmfLE, lmf:sense, Sense }
 ==>
 ls_uri(LmfLE, SenseURI),
 ex_uri(SenseExample, ExURI),
 formType(FormCategory, FormType),
 rdf_global_id(corn21s:ExpressionId, ExpressionType),
 { SenseURI, lemon:example, ExURI } >> sense_examples,
 { ExURI, corn21s:phraseType, FormType } >> sense_examples,
 { ExURI, corn21s:senseExpressionType, ExpressionType } >> sense_examples,
 { ExURI, rdf:type, lemon:'UsageExample' } >> sense_examples,
 { ExURI, lemon:value, CanonicalForm } >> sense_examples.

sense_example_def @@
 { SemEx, lmf:definition, Def } ,
 { SenseExample, lmf:semantics_ex, SemEx }
 ==>
 ex_uri(SenseExample, ExURI),
 { ExURI, corn21s:definition, Def@nl } >> sense_examples.

sense_example_text @@
 { TF, lmf:textualform, literal(Lit) } ,
 { SenseExample, lmf:textual_form, TF } ,
 { ExList, lmf:sense_example, SenseExample },
 { Sense, lmf:sense_examples, ExList },
 { LmfLE, lmf:sense, Sense }
 ==>
 ls_uri(LmfLE, SenseURI),
 ex_uri(SenseExample, ExURI),
 { SenseURI, lemon:example, ExURI } >> sense_examples,
 { ExURI, corn21s:textualForm, Lit@nl } >> sense_examples.
