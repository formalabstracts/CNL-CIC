# parser combinators 

"""
This module defines basic parser combinators.

Each 'Parse' object transforms an 'Item' to an 'Item'.

Following the class functions for Parse, we
give basic parsers for
  words, phrases, delimited expressions, and lists
"""

import msg
import lib 
import lexer
import word_lists
import copy
from collections import namedtuple


def copy_token(tok,attr):
    """make a new token by addding attributes 'attr' to tok"""
    tcopy = copy.copy(tok)
    for v in attr:
        tcopy.__setattr__(v,attr[v])
    return tcopy 

def mk_token(attr={'type':'INTEGER','value':'1'}):
    """make a new token with attributes given by dictionary attr
    For example,
         mk_token({'type':'COLOR','value':'blue'}).
    """
    #tok = copy.copy(mk_token._tok)
    #for v in attr:
    #    tok.__setattr__(v,attr[v])
    #return tok
    return copy_token(mk_token._tok,attr)

def init_mk_token():
    """Call this once to initialize mk_token."""
    lexer.tokenizer.input('1')
    mk_token._tok = [tok for tok in lexer.tokenizer][0]
    mk_token._tok.__setattr__('lineno',0)
    mk_token._tok.__setattr__('lexpos',0)
    mk_token._tok.__setattr__('lexer',None)
    pass

init_mk_token()



#test mk_token()
#mm = mk_token({'type' : 'RED','value' :'blue'})
#print(mk_token())
#print(f'mm={mm}')
#print(mm.__dict__)

    

# An item is a token embedded at a particular position of the tuple of tokens.
# The stream and individual tokens remain immutable.  
# pos changes.
# acc is the accumulator holding the parsed data, with stream range start:stop
# history is for error-handling, positions refer to positions of toks in stream.
Item = namedtuple('Item','stream pos acc history')

def init_item(s) -> Item:
    """Intialize item stream with a tuple of tokens"""
#   # a token used for cloning
    if len(s) > 0:
        init_item.tok = s[0]
    return Item(pos=0,stream=s,acc=None,history=[])

#v = init_item([3,4,5])
#print(init_item.tok)

def next_item(item:Item) -> Item:
    """Advance to the next item of the stream.
    The stream is left unchanged."""
    if item.pos >= len(item.stream):
        raise StopIteration
    return Item(pos = item.pos+1,stream = item.stream,
                acc = item.stream[item.pos],
                history = item.history+ [['next-item',item.pos,item.pos +1]])
    #it = copy.copy(item)
    #it.pos = it.pos + 1
    #it.acc = item.stream[item.pos]
    #it.history += [('next-item',item.pos,it.pos)]
    #return it

def update(acc,item:Item) -> Item:
    """Create a new item with replaced accumulator"""
    return Item(pos = item.pos,stream = item.stream,
            acc = acc,
            history = item.history)

#def replace_history(item:Item,h) -> Item:
#    """Replace history annotations"""
#    return Item(pos = item.pos,stream = item.stream,
#            acc = item.acc,
#            history = h)

def add_history(item:Item,h,drop=0) -> Item:
    """Add a history annotation"""
    #debug
    h1 = item.history
    if drop > 0:
        h1 = h1[:-drop]
    return Item(pos = item.pos,stream = item.stream,
            acc = item.acc,
            history = h1+h)

def range_history(name:str,hs):
    mn = min((i for (_,i,_) in hs if i > 0),default=0)
    mx = max((i for (_,_,i) in hs if i > 0),default=mn)
    #print(f'min,max=({mn},{mx})')
    return [name,mn,mx]

#exceptions

class ParseError(BaseException):
    """Standard parse error. Give item with position at which it fails as arg"""
    pass

class ParseNoCatch(BaseException):
    """Exception not caught by other parsers"""
    
    def __init__(self,msg=''):
        self.msg = msg
        
#def can_eval(f,x):
#    try:
#        f(x)
#        return True
#    except ParseError:
#        return False
    
#def raise_false(b):
#    if not(b):
#        raise ParseError
#    return b

#not yet used...
#class ParseCell:
#    """base class for parsed data"""
#    
#    def __init__(self,toks,cells,name,celltyp):
#        self.toks = toks
#        self.cells = cells
#        self.name = name 
#        self.celltyp = celltyp
        
#    def start_index(self):
#        lexer.token_len(self.toks[0])
        
#    def end_index(self):
#        t = self.toks[-1]
#        lexer.token_len(t)+t.lexpos 
     
#repr not yet used...


    


class Parse:
    """base class for parsers.
    f:Item->Item processes one or more tokens from the item stream.
    """
    def __init__(self,f):
        """r:Item->Item, repr:str"""
        self.process = f
        #self.repr = repr 
 #       self.err = errmsg
        
#    def __repr__(self):
#        """Description of production rule"""
#        return f'Parse({self.repr})'
    
    def set_repr(self,rep):
        self.repr = rep
        return self
        
    def next_token(): # constructor for next token
        return Parse(next_item)
    
    def finished():
        """fails if tokens remain in stream, otherwise do nothing"""
        def f(item):
            if item.pos < len(item.stream):
                vs = [i.value for i in item.stream[item.pos:len(item.stream)]].join(' ')
                item1 = add_history(item, [['excess tokens:'+ vs,item.pos,item.pos]])
                raise ParseError(item1)
            return item
        return Parse(f)
    
    def probe(self):
        """run parser but then undo"""
        def f(item):
            self.process(item)
            return item
        return Parse(f)
    
    def reparse(self):
        """Run parser as a reparser on list of accumulated tokens.  
        If accumulated tokens == [], then do nothing.
        All tokens must be consumed.
        """
        def f(item):
            acc = item.acc
            if len(acc) == 0:
                return item
            item1 = Item(acc,0,None,[])
            item2 = (self + Parse.finished()).process(item1)
            item3 = update(item2.acc,item)
            return item3
        return Parse(f)
    
    def reparse_list(self):
        """Run parser as reparser on each accumulated list entry.
        All tokens must be consumed."""
        def f(item):
            acc = item.acc
            its1 = [Item(a,0,None,[]) for a in acc]
            acc2 = [(self + Parse.finished()).process(it).acc for it in its1]
            item3 = update(acc2,item)
            return item3
        return Parse(f)
    
    def expect(self,history_label):
        """Add history annotation for expectation in case of error"""
        def f(item):
            try:    
                return self.process(item)  
            except ParseError as pe:
                item1 = add_history(pe.args[0],[[f'expecting:{history_label}',item.pos,item.pos]])
                raise ParseError(item1)
        return Parse(f)
    
    def history(self,h,drop=0):
        def f(item):
            return add_history(item,h,drop)
        return Parse(f)
    
    def clear_history(self):
        def f(item):
            return add_history(item,[],drop=len(item.history))
        return Parse(f)
        
    #def __call__(self,item):
    #    return self.process(item)

    def __add__(self,other):
        """combine two parsers in succession, returning pair of results."""
        def f(item:Item):
            item1 = self.process(item)
            item2 = other.process(item1)
            mh = range_history('add',item2.history)
            return add_history(update((item1.acc,item2.acc),item2),[mh])
        return Parse(f)

    def __or__(self,other):
        """try first parser then next. Lower precedence than +"""
        def f(item):
            try:
                return self.process(item)
            except ParseError as pe1:
                try:
                    return other.process(item)
                except ParseError as pe2:
                    item1 = pe1.args[0]
                    item2 = pe2.args[0]
                    if item1.pos > item2.pos: #raise the most progressed
                        raise ParseError(item1)
                    raise ParseError(item2)
        return Parse(f)
    
#    def compose(self,other): #was dependent plus
#        """compose parsers"""
#        def f(item):
#            return other.process(self.process(item))
#        return Parse(f)

#    def subparser(self):
#        """take acc and run paser P on it"""
    
    def nocatch(self,msg): #was fix
        """No catch error if failure"""
        def f(item):
            try:
                return self.process(item)
            except ParseError:
                raise ParseNoCatch(msg)
        return Parse(f)
    
    # was __rshift__ but Python gives it higher precedence than | +, which isn't helpful.
    def treat(self,treatment):
        """apply treatment to parser output."""
        def f(item):
            item1 = self.process(item)
            return update(treatment(item1.acc),item1)
        return Parse(f)
        
    def many(self):
        """parse zero or more times"""
#        def augment_history(item,mh):
#            (_,i) = item.history[-1][0].split(' ')
#            return ('many '+(int(i)+1),mh[1],mh[2])
        def f(item):
            try:
                item1 = self.process(item)
            except (ParseError, StopIteration):
                return add_history(update([],item),[['many',item.pos,item.pos]])
            item2 = self.many().process(item1) #this doesn't fail
            mh = range_history('many',item2.history)
            return add_history(update([item1.acc]+item2.acc,item2),[mh],drop=1)  
        return Parse(f)
    
    def atleast(self,n):
        """parse at least n times"""
        def f(item):
            if n < 1:
                item1 = self.many().process(item)
                h = item1.history[-1]
                h[0]= 'plus'
                return add_history(item1,[h],drop=1)
            else:
                item1 = (self + Parse.atleast(self,n-1)).treat(lib.prepend).process(item)
                h = range_history('plus',item1.history)
                return add_history(item1,[h],drop=2)
        return Parse(f)
    
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
        return Parse(f)
    
    def identity(): #was nothing
        """Does no parsing, identity parser"""
        return Parse(lambda item:item)
    
    def nil(self):
        """replaces output with nil list"""
        return self.treat(lambda _:[])
 
    def separated_nonempty_list(self,sep):
        """Sequence of at least one parse with separation sep"""
        return (self + 
                (sep + self).treat(lib.snd).many()).treat(lib.prepend).expect('list')
              
    def separated_list(self,sep):
        """sequence of parses with separation sep"""
        return (self.separated_nonempty_list(sep) | Parse.identity().nil())
                
    def if_test(self,p): #was some
        """Next passes boolean test or fail"""
        def f(item):
            item1 = self.process(item)
            if p(item1.acc):
                return item1
            else:
                raise ParseError(item)
        return Parse(f)
    
#    def if_test_treat(self,p): #was someX
#        """Next passes test and evaluates, or fail"""
#        def f(item):
#            item1 = self.process(item)
#            b,treat = p(item1.acc)
#            if b:
#                return update(treat,item1)
#            else:
#                raise ParseError(item)
#        return Parse(f)
    
    def if_value(self,v): #was a
        """parse if next token has value v or fail"""
        def p(tok):
            return tok.value == v
        return self.if_test(p).expect(v)
    
    def if_type(self,ts): 
        """parse if next type is in ts or fail"""
        def p(tok):
            return tok.type in ts
        return self.if_test(p).expect('token in '+' '.join(ts))
 
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
        return Parse(f)
    
    def first(prs): #was parse_some 
        """parse first in a list that does not fail"""
        def f(item):
            raise ParseError(item)
        if len(prs) == 0:
            return Parse(f)
        return Parse.__or__(prs[0],Parse.first(prs[1:]))
#            else:
#                
#                try: 
#                    return prs[0].process(item)
#                except:
#                    return Parse.first(prs[1:]).process(item)
#        return Parse(f)
    
    def gen_first(prs_gen,args):
        """Repeat (lazy) parse generator until first non-failure.
        Yields of generator function prs_gen should be a parser.
        Generator formed by prs_gen(*args)."""
        def f(item):
            gen = prs_gen(*args) 
            #print(f'\nentering first on {item.stream[item.pos].value}\n')
            item_max = item
            while True:
                try:
                    prs = next(gen)
                    #print(f'{prs}--start on {item.stream[item.pos].value}')
                    item1 = prs.process(item)
                    del gen
                    #print(f'{prs}--works')
                    return item1
                except ParseError as pe:
                    #print(f'{prs}--fails')
                    item_e = pe.args[0]
                    if item_e.pos > item_max.pos:
                        item_max = item_e
                    pass
                except StopIteration:
                    #print(f'{prs}--stop')
                    del gen
                    raise ParseError(item_max)
        return Parse(f)
    

    
#functions outside class.



# scoping 
scope_current = {}

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
    return p.if_test(can_wordify).treat(wordify).expect('word')

def next_any_word() -> Parse: #was anyword
    """parser constructor that matches any next word"""
    return word(Parse.next_token())

def next_any_word_except(ss):
    def p(tok):
        return not tok.value in ss
    return next_any_word().if_test(p)

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
    return next_any_word().if_value(syn).expect(s)
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
    return Parse.all(phrase).expect(ss)

def first_phrase(phs)-> Parse: #was somephrase
    """parser constructor for the first matching phrase up to white space and synonyms"""
    return Parse.first([next_phrase(ph) for ph in phs]).expect('first:'+ '/'.join(phs))

def first_word(ss:str) -> Parse: #was someword
    """parser constructor for the first matching word up to white space and syns"""
    return Parse.first([next_word(s) for s in ss.split(' ')]).expect('first:'+ss)

#repeat
#def nocatch(msg,pr:Parse) -> Parse:
#    """make a parser raise ParseNoCatch on failure"""
#    def f(tok):
#        try:    
#            return pr(tok)
#        except ParseError:
#            raise ParseNoCatch(msg)
#    return Parse(f)

def commit(msg:str,probe:Parse,pr:Parse) -> Parse:
    """if trial_parse does not fail, discard, then apply pr without catching"""
    def f(item):
        probe.process(item)
        return pr.nocatch(msg).process(item)
    return Parse(f)
        
##def commit_head(msg:str,head:Parse,pr2) -> Parse:
#    """compose parsers applying head, then pr2(output data) with nocatch"""
#    def f(item):
#        item1 = head.process(item)
#        return pr2(item1.acc).nocatch(msg)(item1)
#    return Parse(f)

def if_then_else(probe:Parse,pr1:Parse,pr2:Parse)-> Parse:
    """if probe fails do pr2, otherwise pr1"""
    def f(item):
        try:
            probe.process(item)
        except:
            return pr2.process(item)
        return pr1.process(item) 

#def until(pr1:Parse,pr2:Parse) -> Parse:
#    """accumulate pr1's in a list until pr2 succeeds, including pr2 output"""
#    def t(t1,ts):
#        t1s,t2 = ts
#        return ([t1]+t1s,t2)
#    def f(item):
#        try:
#            return pr2.process(item)  # no pr1
#        except:
#            item1=pr1.process(item)
#            item2= until(pr1,pr2)(item1)
#            return update(t(item1.acc,item2.acc),item2)
#    return Parse(f)

def delimit(pr:Parse,left:str,right:str) -> Parse:
    """delimit a parser"""
    def flat(tok):
        ((a,b),c)=tok
        b = b if type(b) is list else [b]
        return [a]+b+[c]
    return (next_value(left)+pr+next_value(right)).treat(flat)

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

def lambda_true(_):
    return True

def balanced_cases(b):
    #print('bc-toks')
    yield Parse.next_token().if_test(b).plus() #,[b_not_delimiter]
    for left,right in [('(',')'),('[',']'),('{','}')]:
        #print(f'bc-delim-{left}{right}')
        yield (delimit(balanced_condition(lambda_true),left,right)).expect('left delimiter')

def balanced_condition(b) -> Parse:  #was balanced B
    """get list of balanced delimited tokens, applying token condition b at outermost level"""
    def b_not_delimiter(tok):
        return not(tok.value in ['(',')','{','}','[',']']) and b(tok)  
#        return r
    return Parse.gen_first(balanced_cases,[b_not_delimiter]).many().treat(lib.flatten)

def balanced() -> Parse:
    return balanced_condition(lambda_true)

def brace_semi():
    """construct parser for brace-delimited delimiter-balanced semicolon separated list"""
    def p(tok):
        return tok.value != ';'
    nosemi = balanced_condition(p).expect('no ;')
    return brace(Parse.separated_nonempty_list(nosemi,next_value(';')))
    
def comma_nonempty_list(pr:Parse) -> Parse:
    """construct parser for comma-separated list"""
    return Parse.separated_nonempty_list(pr,next_value(','))

andcomma = next_value(',') | next_value('and')

def andcomma_nonempty_list(pr:Parse) -> Parse:
    """construct parser for and/comma separated list"""
    return Parse.separated_nonempty_list(pr,andcomma)

def or_nonempty_list(pr:Parse) -> Parse:
    """construct parser for 'or' separated list"""
    return Parse.separated_nonempty_list(pr,next_value('or'))

