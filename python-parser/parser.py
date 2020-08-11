# parser combinators 

import msg
import lib 
import lexer
import word_lists
import copy

# input stream should have next:stream -> (result,stream)

class ParseError(Exception):
    """Standard parse error"""
    pass

class ParseNoCatch(Exception):
    """Exception not caught by other parsers"""
    
    def __init__(self,msg=''):
        self.msg = msg
        
def can_eval(f,x):
    try:
        f(x)
        return True
    except ParseError:
        return False
    
def raise_false(b):
    if not(b):
        raise ParseError
    return b

class ParseCell:
    """base class for parsed data"""
    
    def __init__(self,toks,cells,name,celltyp):
        self.toks = toks
        self.cells = cells
        self.name = name 
        self.celltyp = celltyp
        
    def start_index(self):
        lexer.token_len(self.toks[0])
        
    def end_index(self):
        t = self.toks[-1]
        lexer.token_len(t)+t.lexpos 
        
class Parse:
    """base class for parsers"""
    def __init__(self,repr,r):
        self.read = r
        self.repr = repr 
        
    def __repr__(self):
        """Description of production rule"""
        return 'Parse(%s)' % self.repr 
    
    def set_repr(self,rep):
        self.repr = rep
        
    def next_token(): # constructor for next token
        return Parse('next_token',next)
        
    def __call__(self,input):
        self.read(input)
    
    def __add__(self,other):
        """combine two parsers in succession"""
        def f(input):
            (result,input1) = self(input)
            (result2,input2) = other(input1)
            return ((result,result2),input2)
        return Parse(self.repr()+'+'+other.repr(),f)

    def __or__(self,other):
        """try first parser then next"""
        def f(input):
            try:
                return self(input)
            except ParseError:
                return other(input)
            
        return Parse(self.repr()+'|'+other.repr(),f)
    
    def __rshift__(self,treatment):
        """apply treatment to parser output"""
        def f(input):
            (result,input1) = self(input)
            return (treatment(result),input1)
        
    def many(self):
        """parse zero or more times"""
        def f(input):
            try:
                result,input1 = self(input)
                results,input2 = self.many()(input1)
                return ([result]+results,input2)
            except ParseError:
                return ([],input)
        return Parse('many(%s)' % self.repr(),f)
    
    def nocatch(self,msg): #was fix
        """No catch error if failure"""
        def f(input):
            try:
                return self(input)
            except ParseError:
                raise ParseNoCatch(msg)
        return Parse(self.repr(),f)
    
    def separated_nonempty_list(self,sep):
        """Sequence of at least one parse with separation sep"""
        def treat(l,ls):
            [l]+ls
        return self + (sep + self >> lib.snd).many() >> treat
            
    def nothing():
        """Does no parsing, empty list as output"""
        def f(input):
            return ([],input)
        return Parse("nothing",f)
    
    def separated_list(self,sep):
        """sequence of parses with separation sep"""
        return (self.separated_nonempty_list(sep) | Parse.nothing()).set_repr("sep list")
                
    def possibly(self):
        """zero or one parses returned in a list"""
        def f(input):
            try:    
                result,input1 = self(input)
                return [result],input1
            except ParseError:
                return [],input
        return Parse('possibly(%s)' % self.repr(),f)
    
    def if_test(self,p): #was some
        """Next passes boolean test or fail"""
        def f(input):
            (result,input1) = self(input)
            if p(result):
                return (result,input1)
            else:
                raise ParseError
        return Parse('if_test',f)
    
    def if_test_treat(self,p): #was someX
        """Next passes test and evaluates or fail"""
        def f(input):
            (result,input1) = self(input)
            b,treat = p(result)
            if b:
                return treat,input1
            else:
                raise ParseError
        return Parse('if_test_delay',f)
    
    def atleast(self,n):
        """parse at least n times"""
        def treat(t):
            (a,b) = t
            return [a]+b
        def f(input):
            if n < 1:
                self.many()
            else:
                self + Parse.atleast(self,n-1) >> treat
        return Parse('at least {n}'.format(n=n),f)
    
    def plus(self):
        """parse at least once"""
        return self.atleast(1)
    
    def compose(self,other): #was dependent plus
        """compose parsers"""
        def f(input):
            (result,input1) = self(input)
            return other(result,input1)
        return Parse('compose',f)
    
    def next_value(v): #was a
        """parse if next token has value v or fail"""
        def p(tok):
            return tok.value == v
        return Parse.next_token().if_test(p)
    
    def next_type(ts): 
        """parse if next type is in ts or fail"""
        def p(tok):
            return tok.type in ts
        return Parse.next_token().if_test(p)
    
    def parse_all(prs):
        """sequentially parse a list of parsers and return list of results"""
        def f(input):
            if len(prs) == 0:
                return ([],input)
            else:
                (result,input1) = prs[0](input)
                (result2,input2) = Parse.parse_all(prs[1:],input1)
                return [result]+result2,input2
        return Parse('parse_all',f)
    
    def parse_first(prs): #was parse_some 
        """parse first in a list that does not fail"""
        def f(input):
            if len(prs) == 0:
                raise ParseError
            else:
                try: 
                    return prs[0](input)
                except:
                    return Parse.parse_first(prs[1:])(input)
        return Parse('parse_first',f)
    
def wordify(tok):
    # need to (shallow) clone because of backtracking.
    if tok.typ == 'WORD':
        return tok
    clone = copy.copy(tok)
    clone.type = 'WORD'
    clone.value = lexer.singularize(tok.value)
    return clone

def can_wordify(tok):
    return tok.typ == 'WORD' or (tok.type == 'VAR' and len(tok.value)==1 and tok.value.isalpha())
    
def next_word(s:str) -> Parse:
    """parser constructor that matches next with word string s"""
    def p(tok):
        if can_wordify(tok):
            return (True,wordify(tok))
        else:
            return (False,None)
    return Parse.next_token().if_test(p).set_repr('word(%s)' % s)

# synonym handling uses a global dictionary, must be single words.

synonym = { key: key for key in word_lists.invariable }
minsynlen = 4

def synonym_add(ts):
    """add synonym list to dictionary"""
    #XX Debug: should check that at most one variant in ts is defined anywhere.
    for s in ts:
        if len(s.split(' '))> 1:
            return msg.error('synonym entries must be single words:'+s)
        if lexer.singularize(s) in synonym:
            return msg.error('synonym already declared: %s' % s)
        # len restriction prevents VAR from being added to dict.
        if len(s) < minsynlen:
            return msg.error('synonyms must have at least {m} chars: {s}'.format(m=minsynlen,s=s))
    ls = [lexer.singularize(s) for s in ts]
    ls.sort()
    js = ' '.join(ls)
    for s in ls:
        synonym[s] = js
        
def synonymize(s:str):
    """get canonical synonymized form of s. Input assumed lower case singular."""
    return synonym.get(s,s)

#debug 
#synonym_add(['world','andulux','awayto'])
#synonym_add(['Real','Worldly','crypto'])
#print(synonymize('Andulux'))

def synw(tok):
    """synonymize a word"""
    s = tok.value 
    if tok.type == 'VAR':
        s = s.lower()
    return synonymize(s)

def next_wordsyn(s:str) -> Parse:
    """parser constructor that matches next word s, up to synonym"""
    if len(s) < minsynlen:
        return next_word(s)
    syn = synonymize(lexer.singularize(s))
    def p(tok):
        return tok.type == 'WORD' and synw(tok)==syn
    return Parse.next_token().if_test(p).set_repr('wordsyn(%s)' % s)

def next_any_word() -> Parse: #was anyword
    """parser constructor that matches any next word"""
    return Parse.next_token().if_test(can_wordify)

def next_any_word_except(banned) -> Parse:
    """parser constructor that matches any next word except banned.
    Matching on banned words is up to synonym."""
    bansyn = [synonymize(lexer.singularize(b)) for b in banned]
    def p(tok):
        return can_wordify(tok) and not(synw(tok) in bansyn)
    return Parse.next_token().if_test(p)

def next_phrase(s:str)-> Parse:
    """parser constructor that matches phrase up to white space and synonyms."""
    phrase = [next_wordsyn(t) for t in s.split()]
    return Parse.parse_all(phrase)

def first_phrase(phs)-> Parse: #was somephrase
    """parser constructor for the first matching phrase up to white space and synonyms"""
    return Parse.parse_first([next_phrase(ph) for ph in phs])

def first_word(ss:str) -> Parse: #was someword
    """parser constructor for the first matching word up to white space and syns"""
    return Parse.parse_first([next_wordsyn(s) for s in ss.split()])

#repeat
#def nocatch(msg,pr:Parse) -> Parse:
#    """make a parser raise ParseNoCatch on failure"""
#    def f(tok):
#        try:    
#            return pr(tok)
#        except ParseError:
#            raise ParseNoCatch(msg)
#    return Parse('nocatch',f)

def commit(msg:str,trial_parse:Parse,pr:Parse) -> Parse:
    """if trial_parse does not fail, discard, then apply pr without catching"""
    def f(input):
        (_,_)= trial_parse(input)
        return pr.nocatch(msg)(input)
    return Parse('commit',f)
        
def commit_head(msg:str,head:Parse,pr2) -> Parse:
    """compose parsers applying head, then pr2(headresult) with nocatch"""
    def f(input):
        (headresult,input1) = head(input)
        return pr2(headresult).nocatch(msg)(input1)
    return Parse('commit_head',f)

def until(pr1:Parse,pr2:Parse) -> Parse:
    """accumulate pr1's in a list until parse2 succeeds"""
    def f(input):
        try:
            return pr2(input)
        except:
            (result,input1)=pr1(input)
            ((result1,result2),input2)= until(pr1,pr2)(input1)
            return (([result]+result1,result2),input2)
    return Parse('until',f)

def delimit(pr:Parse,left:str,right:str) -> Parse:
    """delimit a parser"""
    return (Parse.next_value(left)+pr+Parse.next_value(right))

def delimit_strip(pr:Parse,left:str,right:str) -> Parse:
    """delimit a parser, discarding delimiters"""
    def treat(t):
        (_,(b,_))= t
        return b
    return delimit(pr,left,right) >> treat

def paren(pr): 
    return delimit_strip(pr,'(',')')
    
def bracket(pr): 
    return delimit_strip(pr,'[',']')
        
def brace(pr):
    return delimit_strip(pr,'{','}')

def option_paren(pr):
    return paren(pr) | pr 

def balanced_test(test) -> Parse:  #was balanced B
    """get list of balanced delimited tokens, applying test at outermost level"""
    def p(token):
        not(token.value in '()[]{}') and test(p)
    return (Parse.next_token().if_test(p).plus() | 
         delimit(balanced(),'(',')') |
         delimit(balanced(),'{','}') |
         delimit(balanced(),'[',']')
         ).many() >> lib.flatten

def balanced():
    return balanced_test(lambda _: True)

def brace_semi():
    """construct parser for brace-delimited delimiter-balanced semicolon separated list"""
    semisep = balanced_test(lambda tok: tok.value != ';')
    return brace(Parse.separated_nonempty_list(semisep,Parse.next_value(';')))
    
def comma_nonempty_list(pr:Parse) -> Parse:
    """construct parser for comma-separated list"""
    return Parse.separated_nonempty_list(pr,Parse.next_value(';'))

def andcomma():
    """parser for 'and' or ','"""
    return Parse.next_value(',') | Parse.next_value('and')

def andcomma_nonempty_list(pr:Parse) -> Parse:
    """construct parser for and/comma separated list"""
    return Parse.separated_nonempty_list(pr,andcomma)

def or_nonempty_list(pr:Parse) -> Parse:
    """construct parser for 'or' separated list"""
    return Parse.separated_nonempty_list(pr,Parse.next_value('or'))

def cs_brace(cs_parse:Parse,brace_parse:Parse) -> Parse:
    """control sequence parser"""
    return cs_parse + brace(brace_parse).many()

def phrase_list_transition():
    """parser for transition phrases"""
    prs = [Parse.phrase(s) for s in word_lists.transition]
    return Parse.parse_first(prs) + Parse.word('that').possibly() >> (lambda _: [])

def phrase_list_filler():
    """parser for filler words"""
    return (Parse.word('we').possibly() + first_word('put write have know see') + 
            Parse.word('that').possibly()) >> (lambda _ : [])

def phrase_list_proof_statement():
    """parser for canned proof statements"""
    return (Parse.phrase("we proceed as follows") |
            (Parse.word('the') + 
             first_word('result lemma theorem proposition corollary') +
             Parse.word('now').possibly() +
             Parse.word('follows')) |
            Parse.phrase('the other cases are similar') |
            (Parse.phrase('the proof is')+ first_word('obvious trivial easy routine')) >>
            (lambda _ : []))

# case_sensitive_word -> use next_value(s)

# Atomic identifiers cannot be a single letter (a short var)
# wordlike atomic identifiers are case insensitive and can have synonym.
#  but hierarchical identifiers are always case sensitive.

def atomic():
    #I forget why I am converting integers.
    """parser for atomic identifiers, converting words and integers as needed"""
    def f(input):
        (result,input1) = Parse.next_token()(input)
        if result.type == 'INTEGER' or result.type == 'WORD':
            tok = copy.copy(result)
            if tok.type == 'WORD':
                tok.value = synonymize(tok.value)
            tok.type = 'ATOMIC_IDENTIFIER'
            return (tok,input1)
        if result.type == 'ATOMIC_IDENTIFIER':
            return result
        raise ParseError
    return Parse('atomic',f)

def var():
    """parser for variables"""
    return Parse.next_token().type(['VAR'])

def var_or_atomic():
    """parser for a var or atomic"""
    return var() | atomic()

def var_or_atomics():
    """parser for a sequence of one or more var or atomics"""
    return Parser.plus(var_or_atomic())

def hierarchical_identifier():
    """parser for hierarchical identifiers"""
    return Parse.next_token().type(['HIERARCHICAL_IDENTIFIER'])

def identifier():
    """parser for hierarchical or atomic identifier"""
    return atomic() | hierarchical_identifier()

# literals are stock phrases that have small variants

#def lits = {
#        'a':
#        }
    

def indefinite(): #lit_a
    """parser for 'a' or 'an'"""
    return Parse.word('a') | Parse.word('an')

def article(): 
    """parser for 'a' or 'an' or 'the'"""
    return indefinite() | Parse.word('the')

def lit_defined_as():
    """parser for 'defined_as'-like phrases"""
    return first_phrase(['said to be','defined as','defined to be'])

def lit_is():
    """parser for 'is'-like words"""
    return first_phrase(['is','area','be','to be'])

def lit_iff():
    """parser for 'iff'-like phrases"""
    return (first_phrase(['iff','if and only if']) |
            (lit_is() + Parse.word('the').possibly() + Parse.word('predicate')))

def lit_denote():
    """parser for 'denote'-like phrases"""
    return first_phrase('denote','stand for')

def lit_do():
    """parser for 'do'-like phrases"""
    return Parse.first_word('do does')

def lit_equal():
    """parser for 'equal'-like phrases"""
    return Parse.phrase('equal to')

def lit_has():
    """parser for 'has'-like phrases"""
    return first_word('has have')

def lit_with():
    """parser for 'with'-like phrases"""
    return first_word('with of having')

def lit_true():
    """parser for 'true'-like words"""
    return first_word('on true yes')

def lit_false():
    """parser for 'false'-like words"""
    return first_word('off false no')

def lit_its_wrong():
    """parser for 'it is wrong that'"""
    return Parse.phrase('it is wrong that')

def lit_record():
    """parser for 'record'-type phrases"""
    return (Parse.word('we').possibly() +
            first_word('record register') +
            Parse.word('identification').possibly() +
            Parse.word('that').possibly())

def lit_exist():
    """parser for 'exist'-type phrases"""
    return Parse.word('exist')

def lit_lets():
    """parser for 'lets'-type phrases"""
    return first_phrase(['let','let us','we','we can'])

def lit_fix():
    """parser for 'fix'-type phrases"""
    return first_word('fix let')

def lit_assume():
    """parser for 'assume'-type words"""
    return first_word('assume suppose')

def lit_then():
    """parser for 'then'-type words"""
    return first_word('then therefore hence')

def lit_choose():
    """parser for 'choose'-type words"""
    return first_word('take choose pick')

def lit_prove():
    """parser for 'prove'-like words"""
    return first_word('prove show')

def lit_say():
    """parser for 'say'-like words"""
    return first_word('say write')

def lit_we_say():
    """parser for 'we say'-like phrases"""
    return (Parse.word('we').possibly() +
            lit_say() +
            Parse.word('that').possibly()
            )

def lit_assoc(): #lit_left
    """parser for associativity left/right/no"""
    return first_word('left right no')

#lit_type Parse.word('type')
#lit_proposition Parse.word('proposition')
#lit_property Parse.word('property')
#lit_classifier Parse.word('classifier')
#label = atomic
#period Parse.value('.')
#renamed map -> call

def lit_field_key():
    """parser for structure field keywords"""
    return first_word('coercion notationless notation parameter type call')

def lit_qed():
    """parser for proof end marker"""
    return first_word('end qed obvious literal')

def lit_document():
    """parser for section markers"""
    return first_word('document article section subsection subsubsection subdivision division')

def lit_enddocument():
    """parser for section end markers"""
    return first_word("endsection endsubsection endsubsubsection enddivision endsubdivision")

def lit_doc() -> Parse: #section_tag
    """parser for section start or end markers"""
    return lit_document() | lit_enddocument()

def lit_def() -> Parse:
    """parser for def keyword"""
    return first_word("def definition")

def lit_axiom() -> Parse:
    """parser for axiom-like keywords"""
    return first_word('axiom conjecture hypothesis equation formula')

def lit_with_property() -> Parse:
    """parser for 'with property'-like phrases"""
    return Parse.phrase('with property')

def lit_param() -> Parse:
    return Parse.phrase('with parameter')

def lit_theorem() -> Parse:
    """parser for theorem-like keywords"""
    return first_word('proposition theorem lemma corollary')

def lit_location() -> Parse:
    """parser for cross-reference document locations"""
    return Parse.first([lit_document(),lit_theorem(),lit_axiom()])






















         
            
    
            
        
    
    


    


    
        
    


    
    


    
        
        
        


        
            
                                  
    
    
                
    
    
            
            