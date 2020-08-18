# parser combinators 

import msg
import lib 
import lexer
import word_lists
import copy
from collections import namedtuple

# An item is a token embedded at a particular position of the tuple of tokens.
# The stream and individual tokens remain immutable.  
# pos changes.
# acc is the accumulator holding the parsed data
Item = namedtuple('Item','stream pos acc')

def init_item(s) -> Item:
    """Initialize item stream with a tuple of tokens"""
    
    return Item(pos=0,stream=s,acc=None)

def next_item(item:Item) -> Item:
    """Advance to the next item of the stream.
    The stream is left unchanged."""
    if item.pos >= len(item.stream):
        raise StopIteration
    return Item(pos=item.pos +1,stream=item.stream,acc=item.stream[item.pos])

def update(acc,item:Item) -> Item:
    """Create a new item with replaced accumulator"""
    return Item(pos=item.pos,stream=item.stream,acc=acc)

#exceptions

class ParseError(BaseException):
    """Standard parse error"""
    pass

class ParseNoCatch(BaseException):
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

#not yet used...
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
     
#repr not yet used...

class Parse:
    """base class for parsers.
    f:Item->Item processes one or more tokens from the item stream.
    """
    def __init__(self,repr,f,errmsg=''):
        """r:Item->Item, repr:str"""
        self.process = f
        self.repr = repr 
        self.err = errmsg
        
    def __repr__(self):
        """Description of production rule"""
        return f'Parse({self.repr})'
    
    def set_repr(self,rep):
        self.repr = rep
        return self
        
    def next_token(): # constructor for next token
        return Parse('next_token',next_item)
        
    #def __call__(self,item):
    #    return self.process(item)
    
    def __add__(self,other):
        """combine two parsers in succession, returning pair of results."""
        def f(item:Item):
            item1 = self.process(item)
            item2 = other.process(item1)
            return update((item1.acc,item2.acc),item2)
        return Parse(f'{self}+{other}',f)

    def __or__(self,other):
        """try first parser then next. Lower precedence than +"""
        def f(item):
            try:
                return self.process(item)
            except ParseError:
                return other.process(item)       
        return Parse(f'{self} | {other}',f)
    
    def compose(self,other): #was dependent plus
        """compose parsers"""
        def f(item):
            return other.process(self.process(item))
        return Parse('compose',f)
    
    def nocatch(self,msg): #was fix
        """No catch error if failure"""
        def f(item):
            try:
                return self.process(item)
            except ParseError:
                raise ParseNoCatch(msg)
        return Parse(self,f)
    
    # was __rshift__ but Python gives it higher precedence than | +, which isn't helpful.
    def treat(self,treatment):
        """apply treatment to parser output."""
        def f(item):
            item1 = self.process(item)
            return update(treatment(item1.acc),item1)
        return Parse(self,f)
        
    def many(self):
        """parse zero or more times"""
        def f(item):
            try:
                item1 = self.process(item)
                item2 = self.many().process(item1) #this doesn't fail
                return update([item1.acc]+item2.acc,item2)
            except (ParseError, StopIteration):
                return update([],item)
        return Parse(f'many{self.repr}',f)
    
    def atleast(self,n):
        """parse at least n times"""
        def f(item):
            if n < 1:
                return self.many().process(item)
            else:
                return (self + Parse.atleast(self,n-1)).treat(lib.prepend).process(item)
        return Parse(f'at least {n}',f)
    
    def plus(self):
        """parse at least once"""
        return self.atleast(1).set_repr('plus')
    
    def possibly(self):
        """zero or one parses returned in a list"""
        def f(item):
            try:    
                item1 = self.process(item)
                return update([item1.acc],item1)
            except ParseError:
                return update([],item)
        return Parse(f'possibly({self})',f)
    
    def identity(): #was nothing
        """Does no parsing, identity parser"""
        return Parse("identity",lambda item:item)
    
    def nil(self):
        """replaces output with nil list"""
        return self.treat(lambda _:[])
 
    def separated_nonempty_list(self,sep):
        """Sequence of at least one parse with separation sep"""
        return (self + (sep + self).treat(lib.snd).many()).treat(lib.prepend)
              
    def separated_list(self,sep):
        """sequence of parses with separation sep"""
        return (self.separated_nonempty_list(sep) | Parse.identity().nil()).set_repr("sep list")
                
    def if_test(self,p): #was some
        """Next passes boolean test or fail"""
        def f(item):
            item1 = self.process(item)
            if p(item1.acc):
                return item1
            else:
                raise ParseError
        return Parse('if_test',f)
    
#    def if_test_treat(self,p): #was someX
#        """Next passes test and evaluates, or fail"""
#        def f(item):
#            item1 = self.process(item)
#            b,treat = p(item1.acc)
#            if b:
#                return update(treat,item1)
#            else:
#                raise ParseError
#        return Parse('if_test_delay',f)
    
    def if_value(self,v): #was a
        """parse if next token has value v or fail"""
        def p(tok):
            return tok.value == v
        return self.if_test(p)
    
    def if_type(self,ts): 
        """parse if next type is in ts or fail"""
        def p(tok):
            return tok.type in ts
        return self.if_test(p)
 
    # class methods
    def all(prs):
        """sequentially parse a list of parsers and return list of results"""
        def f(item):
            if len(prs) == 0:
                return update([],item)
            else:
                item1 = prs[0].process(item)
                item2 = Parse.all(prs[1:]).process(item1)
                return update([item1.acc]+item2.acc,item2)
        return Parse('all',f)
    
    def first(prs): #was parse_some 
        """parse first in a list that does not fail"""
        def f(item):
            if len(prs) == 0:
                raise ParseError
            else:
                try: 
                    return prs[0].process(item)
                except:
                    return Parse.first(prs[1:]).process(item)
        return Parse('first',f)
    
    def gen_first(prs_gen,args):
        """Repeat (lazy) parse generator until first non-failure.
        Yields of generator function prs_gen should be a parser.
        Generator formed by prs_gen(*args)."""
        def f(item):
            gen = prs_gen(*args) 
            #print(f'\nentering first on {item.stream[item.pos].value}\n')
            while True:
                try:
                    prs = next(gen)
                    #print(f'{prs}--start on {item.stream[item.pos].value}')
                    item1 = prs.process(item)
                    del gen
                    #print(f'{prs}--works')
                    return item1
                except ParseError:
                    #print(f'{prs}--fails')
                    pass
                except StopIteration:
                    #print(f'{prs}--stop')
                    del gen
                    raise ParseError
        return Parse('gen_first',f)
    
#functions outside class.

# synonym handling uses a global dictionary, must be single words.

synonym = { key: key for key in word_lists.invariable }

MIN_LEN_SYNONYM = 4

def synonym_add(ts):
    """add synonym list to dictionary"""
    #XX Debug: should check that at most one variant in ts is defined anywhere.
    for s in ts:
        if len(s.split(' '))> 1:
            return msg.error(f'synonym entries must be single words:{s}')
        if lexer.singularize(s) in synonym:
            return msg.error(f'synonym already declared: {s}')
        # len restriction prevents VAR from being added to dict.
        if len(s) < MIN_LEN_SYNONYM:
            return msg.error(f'synonyms must have at least {MIN_LEN_SYNONYM} chars: {s}')
        if not(s.isalpha()):
            return msg.error(f'synonyms must be words: {s}')
    ls = [lexer.singularize(s) for s in ts]
    ls.sort()
    js = ' '.join(ls)
    for s in ls:
        synonym[s] = js
        
def synonymize(s:str) -> str:
    """get canonical synonymized form of s. item assumed lower case singular."""
    if len(s) < MIN_LEN_SYNONYM:
        return s
    return synonym.get(s,s)

#debug 
#synonym_add(['world','andulux','awayto'])
#synonym_add(['Real','Worldly','crypto'])
#print(synonymize('Andulux'))

def synw(tok) -> str:
    """get synonym of a word token"""
    s = tok.value 
    if tok.type == 'VAR':
        s = s.lower()
    return synonymize(s)

def can_wordify(tok) -> bool:
    """True if token can be converted to a word token"""
    return tok.type == 'WORD' or (tok.type == 'VAR' and len(tok.value)==1 and tok.value.isalpha())

def wordify(tok):
    """convert a var/word token to word token up to synonym"""
    # need to (shallow) clone because of backtracking.
    value = synw(tok)
    if tok.type == 'WORD' and tok.value == value:
        return tok
    clone = copy.copy(tok)
    clone.type = 'WORD'
    clone.value = value
    return clone

#def wordify_exact(tok):
#    """convert a var/word token to a word exactly."""

def word(p:Parse) -> Parse:
    """Parser treatment attempts to coerce token to a word token up to synonym."""
    return p.if_test(can_wordify).treat(wordify)

def next_any_word() -> Parse: #was anyword
    """parser constructor that matches any next word"""
    return word(Parse.next_token())

def next_value(v):
    """Parser constructor that accepts a token with given value."""
    return Parse.next_token().if_value(v)

#def next_word_exact(s:str) -> Parse: #was next_word
#    """parser constructor that matches next with word string s"""
    #def p(tok):
    #    if can_wordify(tok):
    #        return (True,wordify(tok))
    #    else:
    #        return (False,None)
#    return word(Parse.next_token()).if_value(s)

def next_word(s:str) -> Parse: #was_next_word_syn
    """parser constructor that matches next word s, up to synonym"""
    #if len(s) < MIN_LEN_SYNONYM:
    #    return next_word_exact(s)
    syn = synonymize(lexer.singularize(s))
    #def p(tok):
    #    return tok.type == 'WORD' and synw(tok)==syn
    return next_any_word().if_value(syn)
    #Parse.next_token().if_test(p).treat(wordify).set_repr(f'wordsyn({s})')

def next_any_word_except(banned) -> Parse:
    """parser constructor that matches any next word except banned.
    Matching on banned words is up to synonym."""
    bansyn = [synonymize(lexer.singularize(b)) for b in banned]
    def p(tok):
        return not(tok.value in bansyn)
    return next_any_word().if_test(p)

def next_phrase(ss:str)-> Parse:
    """parser constructor that matches word phrase up to white space and synonyms."""
    phrase = [next_word(s) for s in ss.split()]
    return Parse.all(phrase)

def first_phrase(phs)-> Parse: #was somephrase
    """parser constructor for the first matching phrase up to white space and synonyms"""
    return Parse.first([next_phrase(ph) for ph in phs])

def first_word(ss:str) -> Parse: #was someword
    """parser constructor for the first matching word up to white space and syns"""
    return Parse.first([next_word(s) for s in ss.split()])

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
    def f(item):
        trial_parse.process(item)
        return pr.nocatch(msg).process(item)
    return Parse('commit',f)
        
def commit_head(msg:str,head:Parse,pr2) -> Parse:
    """compose parsers applying head, then pr2(output data) with nocatch"""
    def f(item):
        item1 = head.process(item)
        return pr2(item1.acc).nocatch(msg)(item1)
    return Parse('commit_head',f)

def until(pr1:Parse,pr2:Parse) -> Parse:
    """accumulate pr1's in a list until pr2 succeeds, including pr2 output"""
    def t(t1,ts):
        t1s,t2 = ts
        return ([t1]+t1s,t2)
    def f(item):
        try:
            return pr2.process(item)  # no pr1
        except:
            item1=pr1.process(item)
            item2= until(pr1,pr2)(item1)
            return update(t(item1.acc,item2.acc),item2)
    return Parse('until',f)

def delimit(pr:Parse,left:str,right:str) -> Parse:
    """delimit a parser"""
    def flat(tok):
        ((a,b),c)=tok
        b = b if type(b) is list else [b]
        return [a]+b+[c]
    return (next_value(left)+pr+next_value(right)).set_repr(f'{left}delimit{right}').treat(flat)

def delimit_strip(pr:Parse,left:str,right:str) -> Parse:
    """delimit a parser, discarding delimiters"""
    def take_middle(tok):
        return tok[1:-1]
    return delimit(pr,left,right).treat(take_middle)

def paren(pr): 
    return delimit_strip(pr,'(',')')
    
def bracket(pr): 
    return delimit_strip(pr,'[',']')
        
def brace(pr):
    return delimit_strip(pr,'{','}')

def option_paren(pr):
    return paren(pr) | pr 

def balanced_cases(b):
    #print('bc-toks')
    yield Parse.next_token().if_test(b).plus() #,[b_not_delimiter]
    for left,right in [('(',')'),('[',']'),('{','}')]:
        #print(f'bc-delim-{left}{right}')
        yield (delimit(balanced_condition(lambda _ : True),left,right))

def balanced_condition(b) -> Parse:  #was balanced B
    """get list of balanced delimited tokens, applying token condition at outermost level"""
    def b_not_delimiter(tok):
        return not(tok.value in ['(',')','{','}','[',']']) and b(tok)  
#        return r
    return Parse.gen_first(balanced_cases,[b_not_delimiter]).many().treat(lib.flatten)

def balanced() -> Parse:
    return balanced_condition(lambda _: True)

def brace_semi():
    """construct parser for brace-delimited delimiter-balanced semicolon separated list"""
    nosemi = balanced_condition(lambda tok: tok.value != ';')
    return brace(Parse.separated_nonempty_list(nosemi,next_value(';')))
    
def comma_nonempty_list(pr:Parse) -> Parse:
    """construct parser for comma-separated list"""
    return Parse.separated_nonempty_list(pr,next_value(','))

def andcomma():
    """parser for 'and' or ','"""
    return next_value(',') | next_value('and')

def andcomma_nonempty_list(pr:Parse) -> Parse:
    """construct parser for and/comma separated list"""
    return Parse.separated_nonempty_list(pr,andcomma)

def or_nonempty_list(pr:Parse) -> Parse:
    """construct parser for 'or' separated list"""
    return Parse.separated_nonempty_list(pr,next_value('or'))

def cs_brace(cs_parse:Parse,brace_parse:Parse) -> Parse:
    """control sequence parser including arguments in braces"""
    return cs_parse + brace(brace_parse).many()

def phrase_list_transition():
    """parser for transition phrases"""
    prs = [Parse.phrase(s) for s in word_lists.transition]
    return (Parse.first(prs) + Parse.word('that').possibly()).nil()

def phrase_list_filler():
    """parser for filler words"""
    return (Parse.word('we').possibly() + first_word('put write have know see') + 
            Parse.word('that').possibly()).nil()

def phrase_list_proof_statement():
    """parser for canned proof statements"""
    return (Parse.phrase("we proceed as follows") |
            (Parse.word('the') + 
             first_word('result lemma theorem proposition corollary') +
             Parse.word('now').possibly() +
             Parse.word('follows')) |
            Parse.phrase('the other cases are similar') |
            (Parse.phrase('the proof is')+ first_word('obvious trivial easy routine'))).nil()

# case_sensitive_word -> use next_value(s)

# Atomic identifiers cannot be a single letter (a short var)
# wordlike atomic identifiers are case insensitive and can have synonym.
#  but hierarchical identifiers are always case sensitive.

def atomic():
    #I forget why I am converting integers.
    """parser for atomic identifiers, converting words and integers as needed"""
    def f(item):
        (result,item1) = Parse.next_token().process(item)
        if result.type == 'INTEGER' or result.type == 'WORD':
            tok = copy.copy(result)
            if tok.type == 'WORD':
                tok.value = synonymize(tok.value)
            tok.type = 'ATOMIC_IDENTIFIER'
            return (tok,item1)
        if result.type == 'ATOMIC_IDENTIFIER':
            return result
        raise ParseError
    return Parse('atomic',f)

def var():
    """parser for variables"""
    return Parse.next_token().if_type(['VAR'])

def var_or_atomic():
    """parser for a var or atomic"""
    return var() | atomic()

def var_or_atomics():
    """parser for a sequence of one or more var or atomics"""
    return Parse.plus(var_or_atomic())

def hierarchical_identifier():
    """parser for hierarchical identifiers"""
    return Parse.next_token().if_type(['HIERARCHICAL_IDENTIFIER'])

def identifier():
    """parser for hierarchical or atomic identifier"""
    return atomic() | hierarchical_identifier()

# canned phrases that have small variants

#debug 
#t = next_word('q')
#print("ehllo")
#print(t.type)

#def lits = {
#        'a':
#        }

#canned = {
#    'a':        next_word('a') | next_word('an'),
#    'article':  indefinite() | Parse.word('the'),
#    }
    

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
    return first_phrase(['is','are','be','to be'])

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






















         
            
    
            
        
    
    


    


    
        
    


    
    


    
        
        
        


        
            
                                  
    
    
                
    
    
            
            