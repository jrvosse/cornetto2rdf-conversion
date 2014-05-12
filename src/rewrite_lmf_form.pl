% LexicalForm information is usually in LMF WordForms, but sometimes
% these are empty and all the Form info is in the Lemma:
lemmaWithEmptyWordForms @@
 { LmfLE, lmf:word_forms, literal('') },
 { LmfLE, lmf:lemma, Lemma } ,
 { Lemma, lmf:written_form, literal(LFormLiteral) } ==>
 le_uri(LmfLE, LE),
 rdf_bnode(LF),
 { LE, rdf:type, lemon:'LexicalEntry' } >> entry,
 { LF, rdf:type, lemon:'Form' } >> form,
 { LF, lemon:writtenRep, LFormLiteral@nl } >> form,
 { LE, lemon:canonicalForm, LF } >> form.

% Typical case with non-empty LMF WordForms. If the form is equal to the
% lemma, we model the Entry/Form relation as canonical:
lemmaWithWordForms @@
 { WF, lmf:written_form, literal(WFormLiteral) } ,
 { WFList, lmf:word_form, WF },
 { LmfLE, lmf:word_forms, WFList },
 { LmfLE, lmf:lemma, Lemma } ,
 { Lemma, lmf:written_form, literal(LFormLiteral) } ==>
 WFormLiteral \== '',
 (    LFormLiteral \== WFormLiteral
 ->  rdf_equal(FormPred, lemon:otherForm)
 ;   rdf_equal(FormPred, lemon:canonicalForm)
 ),
 le_uri(LmfLE, LE),
 rdf_bnode(LF),
 { LE, rdf:type, lemon:'LexicalEntry' } >> entry,
 { LF, rdf:type, lemon:'Form' } >> form,
 { LF, lemon:writtenRep, WFormLiteral@nl } >> form,
 { LE, FormPred, LF } >> form.

% Verb with lemma never equal to wordform ...
verbWithWordForms @@
 { LmfLE, lmf:part_of_speech, literal(verb) } ,
 { LmfLE, lmf:lemma, Lemma } ,
 { Lemma, lmf:written_form, literal(LFormLiteral) } ==>
 \+ rdf(LmfLE, lmf:word_forms, literal('')),
 LFormLiteral \== '',
 le_uri(LmfLE, LE),
 rdf_bnode(LF),
 { LE, rdf:type, lemon:'LexicalEntry' } >> entry,
 { LF, rdf:type, lemon:'Form' } >> form,
 { LF, lemon:writtenRep, LFormLiteral@nl } >> form,
 { LE, lemon:canonicalForm, LF } >> form.

% Multi-word Expressions (MWEs) have no WordForm either.
% We model the corresponding MWE Entry has a lemon:Phrase.
multiWordExpressions @@
 { LmfLE, lmf:multiword_expression, MWE },
 { MWE, lmf:expression_type, literal(Etype) },
 { MWE, lmf:written_form, literal(MWELiteral) } ==>
 le_uri(LmfLE, LE),
 rdf_bnode(LF),
 rdf_global_id(corn21s:Etype, T),
 { LE, rdf:type, lemon:'Phrase'} >> entry,
 { LF, rdf:type, lemon:'Form' } >> form,
 { LF, lemon:writtenRep, MWELiteral@nl } >> form,
 { LF, corn21s:mweExpressionType, T } >> form,
 { LE, lemon:canonicalForm, LF } >> form.

% Not all Forms have a the same properties, so put these in specific
% rules.  
relatedForms @@
 { RF, lmf:written_form, literal(RFormLiteral) } ,
 { RF, lmf:variant_type, literal(RFormType) } ,
 { RFList, lmf:related_form, RF },
 { LmfLE, lmf:related_forms, RFList } ==>
 le_uri(LmfLE, LE),
 rdf_global_id(corn21s:RFormType, Type),
 rdf_bnode(LF),
 { LF, lemon:writtenRep, RFormLiteral@nl } >> form,
 { LF, rdf:type, Type } >> form,
 { LE, corn21s:hasRelatedForm, LF } >> form.

number@@
 { WF, lmf:grammatical_number, literal(Number) } ,
 { WF, lmf:written_form, literal(WFormLiteral) } ,
 { WFList, lmf:word_form, WF },
 { LmfLE, lmf:word_forms, WFList } ==>
 Number \= '',
 le_uri(LmfLE, LE),
 rdf_has(LF, lemon:writtenRep, literal(WFormLiteral)),
 rdf_has(LE, lemon:lexicalForm, LF),
 rdf_global_id(corn21s:Number, Object),
 { LF, corn21s:grammaticalNumber, Object } >>  form.

article @@
 { WF, lmf:article, literal(Article) },
 { WF, lmf:written_form, literal(WFormLiteral) } ,
 { WFList, lmf:word_form, WF },
 { LmfLE, lmf:word_forms, WFList } ==>
 Article \= '',
 le_uri(LmfLE, LE),
 rdf_has(LF, lemon:writtenRep, literal(WFormLiteral)),
 rdf_has(LE, lemon:lexicalForm, LF),
 { LF, corn21s:article, Article@nl } >> form.

comparison @@
 { WF, lmf:comparison, literal(Comparison) },
 { WF, lmf:written_form, literal(WFormLiteral) } ,
 { WFList, lmf:word_form, WF },
 { LmfLE, lmf:word_forms, WFList } ==>
 le_uri(LmfLE, LE),
 rdf_has(LF, lemon:writtenRep, literal(WFormLiteral)),
 rdf_has(LE, lemon:lexicalForm, LF),
 rdf_global_id(corn21s:Comparison, Object),
 { LF, corn21s:comparison, Object } >> form.

tense @@
 { WF, lmf:tense, literal(Tense) },
 { WF, lmf:written_form, literal(WFormLiteral) } ,
 { WFList, lmf:word_form, WF },
 { LmfLE, lmf:word_forms, WFList } ==>
 le_uri(LmfLE, LE),
 rdf_has(LF, lemon:writtenRep, literal(WFormLiteral)),
 rdf_has(LE, lemon:lexicalForm, LF),
 rdf_global_id(corn21s:Tense, Object),
 { LF, corn21s:tense, Object } >> form.
