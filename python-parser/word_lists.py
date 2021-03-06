singular = [
    'this','as','is','thesis','hypothesis','less','obvious','us','yes'
    ]

invariable = [  #frozen_list - cannot be given a synonym
    'a','an','all','and','any','are','as','assume','be','by',
    'case','classifier',
    'coercion','conjecture','contradiction','contrary','corollary','declare',
    'def',
    'define','defined','definition','denote','division','do','document',
    'does','dump','each','else','end','enddivision','endsection',
    'endsubdivision','endsubsection','endsubsubsection','equal',
    'equation','error','enter','every','exhaustive','exist','exit',
    'false','fix','fixed','for','forall','formula','fun','function','has','have',
    'having','hence','holding','hypothesis','if','iff','in','inferring',
    'indeed','induction','inductive','introduce','is','it','left','lemma',
    'let','library','make','map','match','moreover','mutual','namespace',
    'no','not','notational','notation',
    'notationless','obvious','of','off','on','only','ontored','or','over',
    'pairwise','parameter','precedence','predicate','printgoal',
    'proof','prop','property','prove','proposition',
    'propped','qed','quotient','read','record','register','recursion','right',
    'said','say','section','show','some','stand','structure','subdivision',
    'subsection','subsubsection','such','suppose','synonym','take','that',
    'the','then','theorem','there','therefore','thesis','this','timelimit',
    'to','total','trivial','true','type','unique','us',
    'warning','we','well','welldefined','well_defined','well_propped',
    'where','with','write','wrong','yes',

#(* plural handled by sing 'classifiers', 'exists','implement',
#   'parameters','properties','propositions','synonyms','types',

]

transition = [ #phrase_list_transition_words
    'a basic fact is','accordingly','additionally','again','also','and yet','as a result',
    'as usual','as we have seen','as we see','at the same time','besides','but',
    'by definition','certainly','clearly','computations show','consequently',
    'conversely','equally important','explicitly','finally','first','for example',
    'for instance','for simplicity','for that reason','for this purpose','further',
    'furthermore','generally','hence','here','however','importantly','in addition',
    'in any event','in brief','in consequence','in contrast','in contrast to this',
    'in each case','in fact','in general','in other words','in particular','in short',
    'in sum','in summary','in the present case','in the same way','in this computation',
    'in this sense','indeed','it follows','it is clear','it is enough to show',
    'it is known','it is routine','it is trivial to see','it is understood',
    'it turns out','last','likewise','more precisely','moreover','most importantly',
    'neverthess','next','nonetheless','note',
    'notice','now','observe','obviously','of course','on the contrary','on the other hand',
    'on the whole','otherwise','second','similarly','so','specifically','still',
    'that is','the point is','then','therefore','third','this gives','this implies',
    'this means','this yields','thus','thus far','to begin with','to this end',
    'trivially','we claim','we emphasize','we first show','we get','we have seen',
    'we have','we know','we check','we may check','we obtain','we remark','we say','we see',
    'we show','we understand','we write','recall','we recall',
    'without loss of generality','yet'
]

preposition_list = [
    'aboard','about','above','according to', 'across', 'against', 'ahead of',
    'along','alongside','amid','amidst','among','around','at','atop','away from',
    'before',
    'behind','below','beneath','beside','between','beyond','by','concerning','despite',
    'except','except at','excluding','following',
    'from','in','in addition to','in place of','in regard to',
    'inside','instead of','into','near','next to','of',
    'off','on','on behalf of','on top of','onto','opposite','out','out of',
    'outside','outside of',
    'over','owing to','per','prior to','regarding','save','through',
    'throughout','till','to','towards','under','until',
    'up','up to','upon','with','with respect to','wrt','within','without'
#   'for', 'as', 'like', 'after', 'round', 'plus', 'since', 'than', 'past', 
#   'during', 
#   synonyms with\~respect\~to/wrt 
]

prim_list = [
    'prim_classifier',
    'prim_term_op_controlseq',
    'prim_binary_relation_controlseq',
    'prim_propositional_op_controlseq',
    'prim_type_op_controlseq',
    'prim_term_controlseq',
    'prim_type_controlseq',
    'prim_lambda_binder',
    'prim_pi_binder',
    'prim_binder_prop',
    'prim_typed_name',
    'prim_adjective',
    'prim_adjective_multisubject',
    'prim_simple_adjective',
    'prim_simple_adjective_multisubject',
    'prim_field_term_accessor',
    'prim_field_type_accessor',
    'prim_field_prop_accessor',
    'prim_definite_noun',
    'prim_identifier_term',
    'prim_identifier_type',
    'prim_possessed_noun',
    'prim_verb',
    'prim_verb_multisubject',
    'prim_structure',
    'prim_type_op',
    'prim_type_word',
    'prim_term_op',
    'prim_binary_relation_op',
    'prim_propositional_op',
    'prim_relation'
  ]
