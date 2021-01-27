#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jan 12 14:39:04 2021

@author: thales

This file contains parser combinators.
The original name was parse.py, but that conflicts with a python lib.

The combinators should preserves tokens; nothings should be discarded.
"""


from exception import ParseError, ParseNoCatch, DataProcess, ErrorItem
import copy, lib, msg, word_lists
import tokenlib
import inner_lexer
import lexer


def mk_inner_stream(s):
    """This function is primarily for debugging.
    It creates an initialized item-stream from a string,
    which is tokenized with the inner lexer.
    """
    inner_lexer.tokenizer.input(s)
    it = tokenlib.init_item(list(inner_lexer.tokenizer))
    return it

def mk_stream(s):
    """This function is primarily for debugging.
    It creates an initialized item-stream from a string,
    which is tokenized with the lexer.
    """
    lexer.tokenizer.input(s)
    return tokenlib.init_item(list(lexer.tokenizer))

        

class Parse:
    """base class for parsers.
    Many of the standard combinators are given.
    process:Item->Item processes one or more tokens from the item stream.
    """
    def __init__(self,process,nonterminal,production=''):
        """r:Item->Item, repr:str"""
        self.process = process
        self.nonterminal = nonterminal
        self.production = production

    def __repr__(self):
        if self.production:
            return self.nonterminal + '/' + self.production
        else:
            return self.nonterminal
        
    def name(self,nonterminal,production=''):
        self.nonterminal=nonterminal
        self.production=msg.setdefault(production,self.production)
        return self
        
    def next_token(): # constructor for next token
        def f(tok):
            return tokenlib.next_item(tok)
        return Parse(f,'.')
    
    def finished():
        """fails if tokens remain in stream, otherwise do nothing"""
        def f(item):
            if item.pos < len(item.stream):
                raise ParseError(ErrorItem(item=item,nonterminal='$',production=''))
            return item
        return Parse(f,'$')
    
    def probe(self):
        """run parser but then undo"""
        def f(item):
            try:
                self.process(item)
                return item
            except ParseError as pe:
                raise ParseError(ErrorItem(item=item,production='',nonterminal='probe')) from pe
        return Parse(f,self.nonterminal,self.production)
    
    def reparse(self,p2):
        """Run parser as a reparser on list of accumulated tokens.  
        If accumulated tokens == [], then do nothing.
        All tokens must be consumed.
        """
        def f(item):
            try:
                item0 = self.process(item)
                acc = item0.acc
                if not acc:
                    return item
                item1 = tokenlib.Item(stream=acc,pos=0,acc=None)
                item2 = (self + Parse.finished()).treat(lib.fst).process(item1)
                item3 = tokenlib.update(item2.acc,item0)
                return item3
            except ParseError as pe:
                raise ParseError(ErrorItem(item=item1,production=p2.nonterminal,nonterminal='reparse')) from pe
        return Parse(f,self.nonterminal,self.production)
    
    def reparse_list(self,p2):
        """Run parser p2 as reparser on each accumulated list entry.
        All tokens must be consumed."""
        def f(item):
            try:
                item1 = self.process(item)
                acc = item1.acc
                its1 = [tokenlib.Item(stream=a,pos=0,acc=None) for a in acc]
                acc2 = [(p2 + Parse.finished()).treat(lib.fst).process(it).acc for it in its1]
                item3 = tokenlib.update(acc2,item1)
                return item3
            except ParseError as pe:
                raise ParseError(ErrorItem(item=item,production=p2.nonterminal,nonterminal='reparse_list')) from pe
        return Parse(f,self.nonterminal,self.production)

    def __add__(self,other):
        """combine two parsers in succession, returning pair of results."""
        def f(item:tokenlib.Item):
            item1 = self.process(item)
            acc1 = item1.acc
            item2 = other.process(tokenlib.update(None,item1))
            return tokenlib.update((acc1,item2.acc),item2)
        return Parse(f,'_+_')

    def __or__(self,other):
        """try first parser then next. Lower precedence than +"""
        def f(item):
            try:
                return self.process(item)
            except ParseError as pe1:
                try:
                    return other.process(item)
                except ParseError as pe2:
                    #debug:print(f'pe1={pe1.args}')
                    parse_error1 = pe1.args[0]
                    item1 = parse_error1.item
                    item2 = pe2.args[0].item
                    if item2.pos > item1.pos: #raise the most progressed
                        raise pe2
                    raise pe1
        return Parse(f,'_or_')
    
    def nocatch(self,msg=''): 
        """No catch error if failure"""
        def f(item):
            try:
                return self.process(item)
            except ParseError as pe:
                nonterminal= msg.setdefault(msg,'nocatch')
                raise ParseNoCatch(ErrorItem(item=item,nonterminal=nonterminal,production='')) from pe
        return Parse(f,'nocatch')

    def treat(self,treatment,msg=''):
        """apply treatment to parser output."""
        def f(item):
            item1 = self.process(item)
            try:
                item2 = tokenlib.update(treatment(item1.acc),item1)
                return item2
            except ParseError as pe:
                nonterminal=msg.setdefault(msg,'treat')
                raise ParseError(ErrorItem(item=item,nonterminal=nonterminal,production='')) from pe
        return Parse(f,self.nonterminal,self.production)
        
    def many(self):
        """parse zero or more times"""
        def f(item):
            try:
                item1 = self.process(item)
            except (ParseError, StopIteration):
                return tokenlib.update([],item) #all returns must be a list
            acc1 = item1.acc
            item2 = self.many().process(tokenlib.update(None,item1)) #this doesn't fail
            return tokenlib.update([acc1]+item2.acc,item2)  
        return Parse(f,'*')
    
    def atleast(self,n):
        """parse at least n times"""
        def f(item):
            if n < 1:
                item1 = self.many().process(item)
                return item1 #acc is a list
            else:
                item1 = (self + Parse.atleast(self,n-1)).treat(lib.prepend).process(item)
                return item1
        return Parse(f,f'+{n}')
    
    def plus(self):
        """parse at least once"""
        return self.atleast(1)
    
    def possibly(self):
        """zero or one parses. It never fails.
        acc left unchanged (None), if no match."""
        def f(item):
            try:    
                return self.process(item)
            except (ParseError,StopIteration):
                return item
        return Parse(f,'?')
    
    def identity(): #was nothing
        """Does no parsing, identity parser"""
        return Parse(lambda item:item,'identity')
    
    def nil(self):
        """replaces output with nil list"""
        return self.treat(lambda _:[]).name('nil','')
 
    def separated_nonempty_list(self,sep):
        """Sequence of at least one parse with separation sep
        Non sep items are obtained by slice [0::2]
        """
        def f(acc):
            (x,xs) = acc
            return [x]+ lib.flatten(xs)
        return (self + 
                (sep + self).many()).treat(f).name('list',self.nonterminal)
              
    def separated_list(self,sep):
        """sequence of parses with separation sep"""
        return (self.separated_nonempty_list(sep) | Parse.identity().nil()).name('list')

    def if_test(self,p): #was some
        """Next token passes boolean test or fail"""
        def f(item):
            item1 = self.process(item)
            if p(item1.acc):
                return item1
            else:
                raise ParseError(ErrorItem(item=item,production='',nonterminal='if_test'))
        return Parse(f,'if')
    
    def if_value(self,v): #was a
        """parse if next token has value v or fail"""
        def p(tok):
            return tok.value == v
        return self.if_test(p).name(self.nonterminal,v)
    
    def if_raw_value(self,v): 
        """parse if next token has value v or fail.
        
        >>> Parse.next_token().if_raw_value('True').process(mk_stream('True')).acc
        LexToken(WORD,'true',1,0)
        """
        def p(tok):
            return tok.rawvalue == v
        return self.if_test(p).name(self.nonterminal,v)
    
    def if_type(self,ts): 
        """parse if next type is in ts or fail"""
        def p(tok):
            return tok.type in ts
        return self.if_test(p).name('if'+list(ts)[0])
 
    # class methods
    def all(prs):
        """sequentially parse a list of parsers and return list of results"""
        def f(item):
            if not prs:
                return tokenlib.update([],item)
            else:
                item1 = prs[0].process(item)
                acc1 = item1.acc
                item2 = Parse.all(prs[1:]).process(tokenlib.update(None,item1))
                return tokenlib.update([acc1]+item2.acc,item2)
        return Parse(f,'all')
    
    def first(prs): #was parse_some 
        """parse first in a list that does not fail"""
        def f(item):
            raise ParseError(ErrorItem(item=item,nonterminal='first',production='empty'))
        if not prs:
            return Parse(f,'first')
        return Parse.__or__(prs[0],Parse.first(prs[1:])).name('first')
    
    def gen_first(prs_gen,args):
        """Repeat (lazy) parse generator until first non-failure.
        Yields of generator function prs_gen should be a parser.
        Generator formed by prs_gen(*args).
        Deprecated.  Use LazyParser instead.
        """
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
                    raise ParseError(ErrorItem(item=item,nonterminal='gen_first'))
        return Parse(f,'gen_first')
    
class LazyParse(Parse):
    """
    With eager evaluation, the parser is expanded in function call args before
    being applied to any elements of the token stream.  
    Eager evaluation and mutually recursive parsers result in infinite loops.
    
    The lazy parser uses a lambda to prevent expansion until it
    is time to apply the parser to the token stream.   An eager parser
    is constructed when needed by a function call fn(data).
    """
    
    def __init__(self,fn,data,nonterminal='thunk',production=''):
        super().__init__(self._process,nonterminal,production)
        self.fn = fn
        self.data = data
    
    def _process(self,item):
        par = self.fn(self.data)
        return par.process(item)


    
def next_value(v):
    """Parser constructor that accepts a token with given value.
    
    >>> next_value('the').process(mk_stream('the test')).acc
    LexToken(WORD,'the',1,0)
    
    >>> next_value('3').process(mk_stream('3 + test')).acc
    LexToken(INTEGER,'3',1,0)   
    """
    return Parse.next_token().if_value(v)

def next_type(t):
    """Parser constructor that accepts a token with given type.
    
    >>> next_type('WORD').process(mk_stream('the test')).acc
    LexToken(WORD,'the',1,0)
    """
    return Parse.next_token().if_type([t])

def getvalue(tok):
    """Extract the string value from a token with default ''"""
    if tok:
        return tok.value 
    else:
        return ''

def debug_lazyparse():
    """This procedure is for debugging and testing only.

    >>> item = mk_inner_stream("hello there")
    >>> lazy = LazyParse(lambda p:p(),Parse.next_token)
    >>> lazy.process(item).acc.value
    'hello'
    
    >>> Parse.next_token().atleast(0).process(item).pos
    2
    
    >>> Parse.next_token().plus().process(item)
    Item(... pos=2, ...)
    
    >>> lazy = LazyParse(lambda p:p().many(),Parse.next_token)
    >>> lazy.process(item)
    Item(... pos=2, ...)  
    """
    pass

class Inner():
    """parsing of inner tokens.
    Inner parsing is used for parser generation, based on string specs.
    
    >>> st = "a/b th:\the$"
    >>> inner_lexer.tokenizer.input(st)
    
    """
    
    def escape():
        def f(acc):
            return ('ESCAPED',getvalue(acc))
        return next_type('ESCAPED').treat(f).name('escaped')
    
    def label():
        return next_type('LABEL')
    
    def dot():
        def f(acc):
            return ('DOT','.')
        return next_type('.').treat(f).name('dot')
        
    def id():
        def f(acc):
            return (acc.type,acc.value)
        return Parse.next_token().if_type(['TY','ID']).treat(f)
    
    def bracket():
        def f(acc):
            ((_,a),_) = acc
            return a
        return (next_type('[') + Inner.annotated().plus() + next_type(']')).treat(f).name('bracket')
    
    def opt():
        def f(acc):
            ((_,a),_) = acc
            return ('OPT',a)
        return (next_type('(') + Inner.annotated().separated_nonempty_list(next_value('|')) + next_type(')')).treat(f).name('opt')
    
    def ending():
        def f(acc):
            l = {}
            (a,b) = acc
            if a:
                l['bracket']=a
            if b:
                l['rep']=b.value
            return l
        return (Inner.bracket().possibly() + ((next_type('*') | next_type('?')).possibly())).treat(f).name('ending')
    
    def parser():
        return ((Inner.id() | Inner.dot() | Inner.escape() | Inner.opt())).name('parser')

    def annotated():
        """This parser must be lazy to delay infinite recursion"""
        def f(acc):
            ((a,b),c) = acc
            d= {'parser':b}
            if a:
                d['label'] = getvalue(a)
            d.update(c)
            return d
        return LazyParse(lambda p: (Inner.label().possibly() + p()  + Inner.ending()).treat(f),Inner.parser,'annotated')
    
    def nontermprod():
        return (Inner.id() + next_type('/')).treat(lib.fst) + Inner.id()
         
    def top_level():
        def f(acc):
            (((a,b),c),p) = acc
            return {'finished':bool(p),'nonterminal':a,'production':b,'annotated':c}
        return (Inner.nontermprod() + Inner.annotated().plus() + next_value('$').possibly()).treat(f)
    
def debug_lazyparse2():
    r"""This procedure is for debugging and testing only. 
    
    >>> Inner.nontermprod().process(mk_inner_stream("a/b")).acc
    (('ID', 'a'), ('ID', 'b'))
    
    >>> Inner.escape().process(mk_inner_stream(r'\abc. d')).acc
    ('ESCAPED', 'abc')
    
    >>> Inner.label().process(mk_inner_stream(r"abc:\ d")).acc
    Illegal inner lexer character '\'
    LexToken(LABEL,'abc',1,0)
    
    >>> Inner.dot().process(mk_inner_stream(r"...")).acc
    ('DOT', '.')
    
    >>> Inner.id().process(mk_inner_stream(r"abc_d efg")).acc
    ('ID', 'abc_d')
    
    >>> Inner.id().process(mk_inner_stream(r"ABC_ efg")).acc
    ('TY', 'ABC_')
    
    >>> (Inner.id() + Inner.label()).process(mk_inner_stream(r"AB cd:")).acc
    (('TY', 'AB'), LexToken(LABEL,'cd',1,3))
    
    >>> next_type('*').process(mk_inner_stream(r"*")).acc
    LexToken(*,'*',1,0)
    
    >>> Inner.ending().process(mk_inner_stream(r"?")).acc
    {'rep': '?'}
    
    >>> Inner.annotated().process(mk_inner_stream(r"rgb:ochre")).acc
    {'parser': ('ID', 'ochre'), 'label': 'rgb'}
    
    >>> Inner.annotated().plus().process(mk_inner_stream(r"color:red* part:leg? separate \one . ")).acc
    [{'parser': ('ID', 'red'), 'label': 'color', 'rep': '*'}, ... 
     
    >>> Inner.bracket().process(mk_inner_stream(r"[a \b]")).acc
    [{'parser': ('ID', 'a')}, {'parser': ('ESCAPED', 'b')}]
    
    >>> Inner.top_level().process(mk_inner_stream(r"term/prod a:b?  the . \red (c | d [ts:t] | ee:e)* ")).acc
    {'finished': False, ... {'parser': ('OPT', ... 'rep': '*'}]}
    """

class OuterParser(Parse):
    """parsing of outer tokens"""
    pass


# synonym handling uses a global dictionary, must be single words.

synonym = { key: key for key in word_lists.invariable }

MIN_LEN_SYNONYM = 4

def synonym_add(ts):
    """add synonym list to dictionary.
    All the words in the list are singularized, then made synonymous.
    The canonical form of the group of synonyms is created."""
    #XX Debug: should check that at most one variant in ts is defined anywhere.
    for s in ts:
        if len(s.split(' '))> 1:
            raise DataProcess(f'synonym entries must be single words:{s}')
        if lexer.singularize(s) in synonym:
            raise DataProcess(f'synonym already declared: {s}')
        # len restriction prevents VAR from being added to dict.
        if len(s) < MIN_LEN_SYNONYM:
            raise DataProcess(f'synonyms must have at least {MIN_LEN_SYNONYM} chars: {s}')
        if not(s.isalpha()):
            raise DataProcess(f'synonyms must be words: {s}')
    #make the canonical_form
    ls = [lexer.singularize(s) for s in ts]
    ls.sort()
    canonical_form = ' '.join(ls)
    #record the canonical_form as the key
    for s in ls:
        synonym[s] = canonical_form
        
def synonymize(s:str) -> str:
    """return canonical form of s in a synonym group. 
    string s assumed lower case singular.
    
    >>> synonym_add(['xxxworld','vvandulux','uuawayto'])
    >>> synonym_add(['Rxxeal','Wvvorldly','cuuryptos'])
    >>> synonymize('vvandulux')
    'uuawayto vvandulux xxxworld'
 
    >>> synonymize('cuurypto')
    'cuurypto rxxeal wvvorldly'
    """
    if len(s) < MIN_LEN_SYNONYM:
        return s
    return synonym.get(s,s)

def synw(tok) -> str:
    """get synonym of a word token"""
    s = tok.value 
    if tok.type == 'VAR':
        s = s.lower()
    return synonymize(s)

def can_wordify(tok) -> bool:
    """True if token can be converted to a word token
    
    >>> can_wordify(tokenlib.mk_token({'type':'WORD'}))
    True
    
    >>> can_wordify(tokenlib.mk_token({'type':'VAR','value':'x'}))
    True
    """
    return (tok.type == 'WORD') or (tok.type == 'VAR' and len(tok.value)==1 and tok.value.isalpha())

def wordify(tok):
    """convert a var/word token to word token up to synonym
    
    >>> wordify(tokenlib.mk_token({'type':'UNKNOWN','value':'3'}))
    LexToken(WORD,'3',0,0)
    """
    # need to (shallow) clone because of backtracking.
    value = synw(tok)
    if tok.type == 'WORD' and tok.value == value:
        return tok
    clone = copy.copy(tok)
    clone.type = 'WORD'
    clone.value = str(value)
    return clone

def word(p:Parse) -> Parse:
    """Parser treatment attempts to coerce token to a word token up to synonym.
    
    >>> word(next_value('x')).process(mk_stream('x + 4')).acc
    LexToken(WORD,'x',1,0)
    """
    return p.if_test(can_wordify).treat(wordify).name('word')

def next_any_word() -> Parse: #was anyword
    """parser constructor that matches any next word
    
    >>> next_any_word().process(mk_stream('x + 4')).acc
    LexToken(WORD,'x',1,0)
    """
    return word(Parse.next_token())

def next_word(s:str) -> Parse: #was_next_word_syn
    """parser constructor that matches next word s, 
    up to synonym, singularization, and case.
    
    >>> next_word('trial').process(mk_stream('Trials x')).acc
    LexToken(WORD,'trial',1,0)
    """
    syn = synonymize(lexer.singularize(s.lower()))
    def p(tok):
        return syn == synonymize(tok.value.lower())
    return next_any_word().if_test(p).name(s)

def next_any_word_except(banned) -> Parse:
    """parser constructor that matches any next word except banned.
    Matching is up to synonym, singularization, and case.
    
    >>> try:
    ...     next_any_word_except(['trial']).process(mk_stream('Trials x')).acc
    ... except:
    ...     print('exception')
    exception
    
    >>> next_any_word_except(['trail']).process(mk_stream('Trials x')).acc
    LexToken(WORD,'trial',1,0)
    """
    bansyn = [synonymize(lexer.singularize(b.lower())) for b in banned]
    def p(tok):
        return not(synonymize(tok.value.lower()) in bansyn)
    return next_any_word().if_test(p)

def next_phrase(ss:str)-> Parse:
    """parser constructor that matches word phrase 
    up to white space and syn-sing-case.
    
    >>> next_phrase('this test').process(mk_stream(' This   test. and...')).acc
    [LexToken(WORD,'this',1,1), LexToken(WORD,'test',1,8)]
    """
    phrase = [next_word(s) for s in ss.split()]
    return Parse.all(phrase).name('phrase',ss)

def first_phrase(phs)-> Parse: #was somephrase
    """parser constructor for the first matching phrase 
    up to white space and syn-sing-case
    
    production_rules.next_word_net is more efficient than this.
    
    >>> first_phrase(['this','that','the other']).process(mk_stream('The  Other. + ..')).acc
    [LexToken(WORD,'the',1,0), LexToken(WORD,'other',1,5)]
    """
    return Parse.first([next_phrase(ph) for ph in phs]).name('first:'+ '/'.join(phs))

def first_word(ss:str) -> Parse: #was someword
    """parser constructor for the first matching word up to white space and syn-sing-case
    
    >>> first_word('this that the').process(mk_stream('That Other. + ..')).acc
    LexToken(WORD,'that',1,0)
    """
    s1 = ss.split(' ')
    def p(tok):
        return tok.value in s1
    return next_any_word().if_test(p).name('first_word:'+ss)
    #return Parse.first([next_word(s) for s in ]).name('first:'+ss)

def commit(probe:Parse,pr:Parse,msg='') -> Parse:
    """if trial_parse does not fail, discard, then apply pr without catching"""
    def f(item):
        probe.process(item)
        return pr.nocatch(msg).process(item)
    return Parse(f)
    # more directly
    # try:
    #    return pr.process(item)
    # except pe:
    #   try:
    #       probe.process(item)
    #       raise NoCatch(pe.args[0])
    #   except:
    #       raise pe

def if_then_else(probe:Parse,pr1:Parse,pr2:Parse)-> Parse:
    """if probe fails do pr2, otherwise pr1"""
    def f(item):
        try:
            probe.process(item)
        except:
            return pr2.process(item)
        return pr1.process(item)
    return Parse(f,'if_then_else')

def delimit(pr:Parse,left:str,right:str) -> Parse:
    """delimit a parser with left and right strings."""
    def flat(acc):
        ((a,b),c)=acc
        return (a,b,c)
    return (next_value(left)+pr+next_value(right)).treat(flat)

#def headtok(tok,htok):
#    """
#    For debugging purposes, we might want to record the token at the head
#    of a phrase, even when it no longer serves a purpose in the parsing.
#    """
#    return tokenlib.copy_token(tok,{'headtok':htok})

#def delimit_strip(pr:Parse,left:str,right:str) -> Parse:
#    """delimit a parser, discarding delimiters"""
#    def take_middle(acc):
#        a1 = acc[1:-1]  #record discarded head.
#        # This is an error, when a1[0] is a *list* of tokens.
#        #if a1:
#        #    #print(acc[0])
#        #    a1[0] = headtok(a1[0],acc[0])
#        return a1
#    return delimit(pr,left,right).treat(take_middle)

def strip_delim(acc):
    """treatment to remove outer delimiters"""
    return list(acc)[1]

def paren(pr): 
    """Parse an expression in parentheses, keeping parentheses.
    
    >>> paren(next_any_word().many()).process(mk_stream('(this that the other)')).acc
    (LexToken((,'(',1,0), [LexToken(WORD,'this',1,1),...
    """
    return delimit(pr,'(',')')
    
def bracket(pr): 
    """Parse an expression in brackets, keeping brackets."""
    return delimit(pr,'[',']')
        
def brace(pr):
    """Parse an expression in braces, keeping braces."""
    return delimit(pr,'{','}')


def lambda_true(_):
    return True

#def balanced_cases(b):
#    #print('bc-toks')
#    yield Parse.next_token().if_test(b).plus() #,[b_not_delimiter]
#    for left,right in [('(',')'),('[',']'),('{','}')]:
#        #print(f'bc-delim-{left}{right}')
#        yield (delimit(balanced_condition(lambda_true),left,right)).name('left delimiter')

#def balanced_condition(b) -> Parse:  #was balanced B
#    """get list of balanced delimited tokens, applying token condition b at outermost level"""
#    def b_not_delimiter(tok):
#        return not(tok.value in ['(',')','{','}','[',']']) and b(tok)  
#        return r
#    return Parse.gen_first(balanced_cases,[b_not_delimiter]).many().treat(lib.flatten)

def balanced_condition(b):
    """Parser for token string with matching parentheses, as long as possible.
    The outermost tokens must satisfy condition b.
    """
    def b_not_delimiter(tok):
        return not(tok.value in ['(',')','{','}','[',']']) and b(tok)  
    return (
        Parse.next_token().if_test(b_not_delimiter).plus() |
        LazyParse((lambda p: delimit(p(lambda_true),'(',')')),balanced_condition,'balanced','paren') | 
        LazyParse((lambda p: delimit(p(lambda_true),'[',']')),balanced_condition,'balanced','brack') | 
        LazyParse((lambda p: delimit(p(lambda_true),'{','}')),balanced_condition,'balanced','curly')).many().treat(lib.flatten)

def balanced() -> Parse:
    return balanced_condition(lambda_true)

def brace_semi():
    """construct parser for brace-delimited delimiter-balanced semicolon separated list
    
    >>> brace_semi().process(mk_stream('{ a ; b { b1 ; (d) } ; c }')).acc
    (LexToken({,'{',1,0), [[LexToken(VAR,'a',1,2)], LexToken(;,';',1,4),...
    """
    def p(tok):
        return tok.value != ';'
    nosemi = balanced_condition(p).name('balanced-semi')
    return brace(Parse.separated_nonempty_list(nosemi,next_value(';')))
    
def comma_nonempty_list(pr:Parse) -> Parse:
    """construct parser for comma-separated list"""
    return pr.separated_nonempty_list(next_value(','))

def comma_list(pr:Parse) -> Parse:
    """parser for comma separated list
    
    >>> comma_list(next_any_word()).process(mk_stream('this,that,other')).acc
    [LexToken(WORD,'this',1,0), LexToken(,,',',1,4), LexToken(WORD,'that',1,5),...
    """
    return pr.separated_list(next_value(','))

def andcomma():
    """parse a comma or the value 'and'
    
    >>> andcomma().process(mk_stream(', ...')).acc
    LexToken(,,',',1,0)
                                                                
    >>> andcomma().process(mk_stream('and ...')).acc
    LexToken(WORD,'and',1,0)
    """
    return (next_value(',') | next_value('and'))

def andcomma_nonempty_list(pr:Parse) -> Parse:
    """construct parser for and/comma separated list

    >>> andcomma_nonempty_list(next_any_word()).process(mk_stream('this , that and other')).acc
    [LexToken(WORD,'this',1,0), LexToken(,,',',1,5), LexToken(WORD,'that',1,7),...
    """
    return pr.separated_nonempty_list(andcomma())

def or_nonempty_list(pr:Parse) -> Parse:
    """construct parser for 'or' separated list
    
    >>> or_nonempty_list(next_any_word()).process(mk_stream('this or that or other')).acc
    [LexToken(WORD,'this',1,0), LexToken(WORD,'or',1,5), LexToken(WORD,'that',1,8)...
    """
    return pr.separated_nonempty_list(next_value('or'))


if __name__ == "__main__":
    import doctest
    doctest.testmod(optionflags=doctest.ELLIPSIS | doctest.NORMALIZE_WHITESPACE)
#    doctest.testmod(verbose=True, optionflags=doctest.ELLIPSIS | doctest.NORMALIZE_WHITESPACE)
#    doctest.testmod()

    

    

    
    