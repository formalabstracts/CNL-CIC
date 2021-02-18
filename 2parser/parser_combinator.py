#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jan 12 14:39:04 2021

@author: Thomas Hales

This file contains parser combinators.
The original name was parse.py, but that conflicts with a python lib.

The combinators should preserves tokens; nothings should be discarded.

The bottom part of this module contains
various parsers for words, phrases, and delimited matters.

"""

import logging
from exception import ParseError, ParseNoCatch, DataProcess, ErrorItem
import copy, lib, msg, word_lists
import tokenlib
import inner_lexer
import lexer
from tokenlib import (mk_stream , Etok, Item)
from ply.lex import LexToken
import sample
#import sample
from state import state

logger = logging.getLogger(__name__)




def pstream(p:'Parse',s):
    """For reunning test cases on different parsers.
    p is a parser
    s:str is input to parser
    
    output is the output of p applied to s.
    """
    #try:
    return (p.process(mk_stream(s)).acc)
    #except ParseError as pe:
    #    logger.exception(pe)
    #    raise pe

        #raise #traceback.print_stack()
        


class Parse:
    """base class for parsers.
    Many of the standard combinators are given.
    process:Item->Item processes one or more tokens from the item stream.
    We refer to process as the 'item transformer'.
    
    A sample is a function that returns a random parse
    on input tokenlib.random.
    It is a function sample:acc -> acc transforming accumulated toks.
    """
    def __init__(self,process,nonterminal,production='',sample=None,show_up=('',[])):
        """r:Item->Item, repr:str"""
        self._process = process
        self.nonterminal = nonterminal
        self.production = production
        self._sample = sample
        self.show_up=show_up

    def __repr__(self):
        if self.production:
            return self.nonterminal + '-' + self.production
        else:
            return self.nonterminal
        
    empty_item = Item(stream=[],pos=0,acc=None)
        
    def sample(self):
        if not(self._sample):
            raise ParseError(ErrorItem(item=None,nonterminal=f'sample not installed: {self.nonterminal}-{self.production}'))
        return self._sample()
        
    def process(self,item):
        
        # sample mode
        if state.mk_sample:
            acc = self.sample()
            return tokenlib.update(acc,item)
        
        # process mode, the front door.
        try:
            return self._process(item)
        except ParseError as pe:
            ei = pe.error_stack
            nt = ei.nonterminal
            if self.nonterminal or self.production:
                nt = f'({nt}; {self})' 
                
        #As a last resort try a backdoor, nonterminal as terminal...
        #the backdoor should never raise an exception.
        if not(tokenlib.eof(item)):
            item1 = tokenlib.next_item(item)
            if item1.acc.value == self.nonterminal:
                acc = Etok.etok(item1.acc)
                return tokenlib.update(acc,item1)
        raise ParseError(ErrorItem(item=ei.item, nonterminal=nt))

    def preprocess(self,pre):
        """Use pre if possible for process."""
        def f(item):
            try:
                return pre(item)
            except:
                self.process(item)
        return Parse(f,nonterminal=self.nonterminal,production=self.production,show_up=self.show_up)
    
    def update(self,d):
        for key,value in d.items():
            setattr(self,key,value)
        return self
    
    def setsample(self,s):
        self._sample = s
        return self
    
    def name(self,nonterminal,production=''):
        self.nonterminal=nonterminal
        if production:
            self.production=production
        #msg.setdefault(production,self.production)
        return self

    
    def show(self,level=1):
        """Generate the parse tree (in words) of the parser,
        down to given level"""
        if (level==0):
            return (f'{self}',[])
        if self.show_up==('',[]):
            return ('',[])
        (comb,ps)=self.show_up 
        if comb[0:4]=='pass':
            return ps[0].show(level)
        if (level==1):
            pass  ## DEBUG FIX
                  
    def next_token(): # constructor for next token
        """The work horse of parsing.
        Parser to fetch the next token.
        """
        def f(item):
            return tokenlib.next_item(item)
        return Parse(f,'.',sample=sample.next_token)
    
    def finished():
        """fails if tokens remain in stream, otherwise do nothing"""
        def f(item):
            if item.pos < len(item.stream):
                raise ParseError(ErrorItem(item=item,nonterminal='$'))
            return item
        return Parse(f,'$',sample=sample.none)

    def identity(): #was nothing
        """Does no parsing, identity parser"""
        return Parse(lambda item:item,'identity',sample=sample.none)
    
    def fail():
        """This parser always fails"""
        def f(item):
            raise ParseError(ErrorItem(item=item,nonterminal='fail'))
        return Parse(f,'fail')
    
#    def nil(self):
#        """replaces output with nil list"""
#        return self.treat(lambda _:[]).name('nil','')
    
    def probe(self):
        """run parser but then undo"""
        def f(item):
            try:
                self.process(item)
                return item
            except ParseError as pe:
                raise ParseError(ErrorItem(item=item,nonterminal='-probe')) from pe
        return Parse(f,self.nonterminal,self.production,sample=sample.none,show_up=('pass',[self]))

    def nocatch(self,nt=''): 
        """No catch error if failure"""
        def f(item):
            try:
                return self.process(item)
            except ParseError as pe:
                nonterminal= msg.setdefault(nt,'nocatch')
                raise ParseNoCatch(ErrorItem(item=item,nonterminal=nonterminal)) from pe
        return Parse(f,'nocatch',sample=self.sample,show_up=('pass',[self]))

    def commit(self,probe:'Parse',msg='') -> 'Parse':
        """if trial_parse does not fail, discard, 
        then apply pr without catching"""
        def f(item):
            probe.process(item)
            return self.nocatch(msg).process(item)
        return Parse(f,self.nonterminal,self.production,sample=self.sample)
        # more directly
        # try:
        #    return pr.process(item)
        # except pe:
        #   try:
        #       probe.process(item)
        #       raise NoCatch(pe.args[0])
        #   except:
        #       raise pe
    
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
                raise ParseError(ErrorItem(item=item1,nonterminal='reparse-'+p2.nonterminal)) from pe
        return Parse(f,self.nonterminal,self.production,show_up=('reparse',[p2]))
    
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
                raise ParseError(ErrorItem(item=item,nonterminal='reparse_list-'+p2.nonterminal)) from pe
        return Parse(f,self.nonterminal,self.production,show_up=('reparse_list',[p2]))

    def __add__(self,other):
        """combine two parsers in succession, returning pair of results."""
        def f(item:tokenlib.Item):
            item1 = self.process(item)
            acc1 = item1.acc
            #try:
            item2 = other.name(f'(+{other})').process(tokenlib.update(None,item1))
            #except ParseError:
            #    raise ParseError(ErrorItem(item=item1,nonterminal= f'{other.nonterminal}-{other.production}'))
            return tokenlib.update((acc1,item2.acc),item2)
        if (self.show_up[0]=='add'):
            show_up = ('add',self.show_up[1]+[other])
        else:
            show_up = ('add',[self,other])
        return Parse(f,'',sample=sample.add_sample(self,other),show_up=show_up)

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
                    parse_error1 = pe1.error_stack #args[0]
                    item1 = parse_error1.item
                    item2 = pe2.error_stack.item # args[0].item
                    if item2.pos > item1.pos: #raise the most progressed
                        raise pe2
                    raise pe1
        if (self.show_up[0]=='or'):
            show_up = ('or',self.show_up[1]+[other])
        else:
            show_up = ('or',[self,other])
        return Parse(f,'',sample=sample.or_sample(self,other),show_up=show_up)
    
    def treat(self,treatment,name=''):
        """apply treatment to parser output.
        The treatment transforms the accumulator."""
        def f(item):
            item1 = self.process(item)
            try:
                item2 = tokenlib.update(treatment(item1.acc),item1)
                return item2
            except ParseError as pe:
                nonterminal=msg.setdefault(name,'treat')
                raise ParseError(ErrorItem(item=item,nonterminal=nonterminal)) from pe
        pr = Parse(f,self.nonterminal,self.production,sample=sample.treat_sample(self,treatment),show_up=('pass',[self]))
        if name:
            return pr.name(name)     
        return pr
        
    def _many_nosep(self):
        """parse zero or more times"""
        def f(item):
            try:
                item1 = self.process(item)
            except (ParseError, StopIteration):
                return tokenlib.update([],item) #all returns must be a list
            acc1 = item1.acc
            #
            item2 = f(tokenlib.update(None,item1)) #this doesn't fail
            return tokenlib.update([acc1]+item2.acc,item2) 
        show_up=(f'{self}*',self.show_up[1])
        return Parse(f,self.nonterminal+'*',sample=sample.many(self,None),show_up=show_up)
    
    def atleast(self,n):
        """parse at least n times.
        Output acc is a list
        """
        def f(item):
            if n < 1:
                item1 = self.many().process(item)
                return item1 #acc is a list
            else:
                item1 = (self + Parse.atleast(self,n-1)).treat(lib.prepend).process(item)
                return item1
        if n==1:
            supp = ''
        else:
            supp = f'{n}'
        return Parse(f,f'{self.nonterminal}+'+supp,sample=sample.atleast(self,n))
    
    def _plus_nosep(self):
        """parse at least once"""
        return self.atleast(1)
    
    def plus(self,sep=None):
        """Sequence of at least one parse with 
        optional separation sep
        
        Output is a list
        If sep, then non sep items are obtained by slice [0::2]
        """
        if not(sep):
            return self._plus_nosep()
        def f(acc):
            (x,xs) = acc
            return [x]+ lib.flatten(xs)
        return (self + 
                (sep + self).many()).treat(f).name('plus',self.nonterminal).setsample(sample.plus(self,sep))
              
    def many(self,sep=None):
        """sequence of parses with optional separation sep
        
        """
        def f(_):
            return []
        if not(sep):
            return self._many_nosep()
        return (self.plus(sep) | Parse.identity().treat(f)).setsample(sample.many(self,sep))
    
    def possibly(self):
        """zero or one parses. It never fails.
        acc left unchanged (None), if no match."""
        def f(item):
            try:    
                return self.process(item)
            except (ParseError,StopIteration):
                return item
        return Parse(f,'?',sample=sample.possibly(self))
    

    def if_test(self,p): #was some
        """Next token passes boolean test or fail"""
        def f(item):
            item1 = self.process(item)
            if p(item1.acc):
                return item1
            else:
                raise ParseError(ErrorItem(item=item,nonterminal=f'{self.nonterminal}-if-{self.production}'))
        return Parse(f,'if',sample=sample.if_test(self,p))
    
    def if_value(self,v): #was a
        """parse if next token has value v or fail"""
        def p(tok):
            return tok.value == v
        return self.name(self.nonterminal,v).if_test(p).setsample(sample.if_value(v))
    
    def if_rawvalue(self,v): 
        """parse if token has rawvalue v or fail.
        
        >>> pstream(Parse.next_token().if_rawvalue('True'),'True')
        LexToken(WORD,'true',1,0)
        """
        def p(tok):
            return (lexer.rawvalue(tok) == v)
        return self.name(self.nonterminal,v).if_test(p).setsample(sample.if_rawvalue(v))
    
    def if_types(self,ts): 
        """parse if next type is in ts or fail"""
        def p(tok):
            return tok.type in ts
        return self.name(list(ts)[0]).if_test(p).setsample(sample.if_types(ts))
 
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
        return Parse(f,'all',sample=sample.all_sample(prs))
    
    def first(prs): #was parse_some 
        """parse first in a list that does not fail"""
        def f(item):
            raise ParseError(ErrorItem(item=item,nonterminal='first-empty'))
        if not prs:
            return Parse(f,'first')
        return Parse.__or__(prs[0],Parse.first(prs[1:])).name('first').setsample(sample.first(prs))
    
#    def gen_first(prs_gen,args):
#        """Repeat (lazy) parse generator until first non-failure.
#        Yields of generator function prs_gen should be a parser.
#        Generator formed by prs_gen(*args).
#        Deprecated.  Use LazyParser instead.
#        """
#        def f(item):
#            gen = prs_gen(*args) 
#            #print(f'\nentering first on {item.stream[item.pos].value}\n')
#            item_max = item
#            while True:
#                try:
#                    prs = next(gen)
#                    #print(f'{prs}--start on {item.stream[item.pos].value}')
#                    item1 = prs.process(item)
#                    del gen
#                    #print(f'{prs}--works')
#                    return item1
#                except ParseError as pe:
#                    #print(f'{prs}--fails')
#                    item_e = pe.args[0]
#                    if item_e.pos > item_max.pos:
#                        item_max = item_e
#                    pass
#                except StopIteration:
#                    #print(f'{prs}--stop')
#                    del gen
#                    raise ParseError(ErrorItem(item=item,nonterminal='gen_first'))
#        return Parse(f,'gen_first')
    
class LazyParse(Parse):
    """
    With eager evaluation, the parser is expanded in function call args before
    being applied to any elements of the token stream.  
    Eager evaluation and mutually recursive parsers result in infinite loops.
    
    The lazy parser uses a lambda to prevent expansion until it
    is time to apply the parser to the token stream.   An eager parser
    is constructed when needed by a function call fn(data).
    """
    def __init__(self,fn,data,nonterminal='lazy',production='',sample=None):
        super().__init__(self._process,nonterminal,production)
        self.fn = fn
        self.data = data
    
    def _process(self,item):
        par = self.fn(self.data)
        return par.process(item)
    
    def sample(self):
        par = self.fn(self.data)
        return par.sample()
    
def lazy_call(pr):
    """pr is a function with output a parser.
    Output is a parser, with lazy evaluation"""
    return LazyParse((lambda p: p()),pr,nonterminal='lazy')

    
def next_value(v):
    """Parser constructor that accepts a token with given value.
    
    >>> pstream(next_value('the'),'the test')
    LexToken(WORD,'the',1,0)
    
    >>> pstream(next_value('3'),'3 + test')
    LexToken(INTEGER,'3',1,0)   
    """
    return Parse.next_token().if_value(v).name('next_value',v)

def next_type(t):
    """Parser constructor that accepts a token with given type.
    
    >>> pstream(next_type('WORD'),'the test')
    LexToken(WORD,'the',1,0)
    """
    return Parse.next_token().if_types([t]).name('next_type',t)

def getvalue(tok):
    """Extract the string value from a token with default ''"""
    if tok:
        return tok.value 
    else:
        return ''
    
def retreat_list(p2,ls):
    """Convert list ls to a list of items.
    Run the parser p2 on each item, requiring full consumption.
    Collect the accumulated output and return.
    The len is preserved in output.
    
    >>> acc = pstream(Parse.next_token().plus(),'a b c')  
    >>> ls = [[a] for a in acc]   
    >>> retreat_list(next_type('VAR'),ls)
    [LexToken(VAR,'a',1,0), LexToken(VAR,'b',1,2), LexToken(VAR,'c',1,4)]
    """
    its1 = [tokenlib.Item(stream=a,pos=0,acc=None) for a in ls]
    acc2 = [(p2 + Parse.finished()).treat(lib.fst).process(it).acc for it in its1]
    return acc2

def debug_lazyparse():
    """This procedure is for debugging and testing only.

    >>> item = mk_stream("hello there")
    >>> lazy = LazyParse(lambda p:p(),Parse.next_token)
    >>> lazy.process(item).acc.value
    'hello'
    
    >>> Parse.next_token().atleast(0).process(item).pos
    2
    
    >>> Parse.next_token().plus().process(item)
    Item(...***...
    
    >>> lazy = LazyParse(lambda p:p().many(),Parse.next_token)
    >>> lazy.process(item)
    Item(...***...  
    """
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
    
    >>> pstream(word(next_value('x')),'x + 4')
    LexToken(WORD,'x',1,0)
    """
    return p.if_test(can_wordify).treat(wordify).name('word').setsample(sample.if_types(['WORD']))

def next_any_word() -> Parse: #was anyword
    """parser constructor that matches any next word
    
    >>> pstream(next_any_word(),'x + 4')
    LexToken(WORD,'x',1,0)
    """
    return word(Parse.next_token())

def next_word(s:str) -> Parse: #was_next_word_syn
    """parser constructor that matches next word s, 
    up to synonym, singularization, and case.
    
    >>> pstream(next_word('trial'),'Trials x')
    LexToken(WORD,'trial',1,0)
    """
    syn = synonymize(lexer.singularize(s.lower()))
    def p(tok):
        return syn == synonymize(tok.value.lower())
    return next_any_word().if_test(p).name(s).setsample(sample.if_value(s))

def next_any_word_except(banned) -> Parse:
    """parser constructor that matches any next WORD token except banned.
    Matching is up to synonym, singularization, and case.
    
    >>> try:
    ...     pstream(next_any_word_except(['trial']),'Trials x')
    ... except ParseError:
    ...     print('exception')
    exception
    
    >>> pstream(next_any_word_except(['trail']),'Trials x')
    LexToken(WORD,'trial',1,0)
    """
    bansyn = [synonymize(lexer.singularize(b.lower())) for b in banned]
    def p(tok):
        return not(synonymize(tok.value.lower()) in bansyn)
    return Parse.next_token().if_types(['WORD']).if_test(p).setsample(sample.if_types(['WORD']))

def next_phrase(ss:str)-> Parse:
    """parser constructor that matches word phrase 
    up to white space and syn-sing-case.
    
    >>> pstream(next_phrase('this test'),' This   test. and...')
    [LexToken(WORD,'this',1,1), LexToken(WORD,'test',1,8)]
    """
    phrase = [next_word(s) for s in ss.split()]
    return Parse.all(phrase).name('phrase',ss)

def first_phrase(phs)-> Parse: #was somephrase
    """parser constructor for the first matching phrase 
    up to white space and syn-sing-case
    
    production_rules.next_word_net is more efficient than this.
    
    >>> pstream(first_phrase(['this','that','the other']),'The  Other. + ..')
    [LexToken(WORD,'the',1,0), LexToken(WORD,'other',1,5)]
    """
    return Parse.first([next_phrase(ph) for ph in phs]).name('first:'+ '/'.join(phs))

def first_word(ss:str) -> Parse: #was someword
    """parser constructor for the first matching word up to white space and syn-sing-case
    
    >>> pstream(first_word('this that the'),'That Other. + ..')
    LexToken(WORD,'that',1,0)
    """
    s1 = ss.split()
    if not s1:
        raise IndexError(f'first_word empty: {ss}')
    def p(tok):
        return tok.value in s1
    return next_any_word().if_test(p).name('first_word:'+ss).setsample(sample.first_word(ss))
    #return Parse.first([next_word(s) for s in ]).name('first:'+ss)

#not used
#def if_then_else(probe:Parse,pr1:Parse,pr2:Parse)-> Parse:
#    """if probe fails do pr2, otherwise pr1"""
#    def f(item):
#        try:
#            probe.process(item)
#        except:
#            return pr2.process(item)
#        return pr1.process(item)
#    return Parse(f,'if_then_else')

def delimit(pr:Parse,left:str,right:str) -> Parse:
    """delimit a parser with left and right strings.
    >>> pstream(delimit(next_word('hello'),'(',')'),'(hello)')
    (LexToken((,'(',1,0), LexToken...
    """
    def flat(acc):
        ((a,b),c)=acc
        return (a,b,c)
    return (next_value(left)+pr+next_value(right)).treat(flat,f'{left}{pr.nonterminal}{right}')

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


def paren(pr): 
    """Parse an expression in parentheses, keeping parentheses.
    
    >>> pstream(paren(next_any_word().many()),'(this that the other)')
    (LexToken((,'(',1,0), [LexToken(WORD,'this',1,1),...
    """
    return delimit(pr,'(',')')

def opt_paren(pr):
    """
    Parse an expression optionally in parentheses. 
    
    Input pr : An etok parser.
    Output: (_,pr-output,_), padding with None if no parentheses exist
    
    >>> pstream(opt_paren(Parse.next_token().treat(Etok.etok)),'(hello)')
    (LexToken((,'(',1,0), Etok(WORD,hello,'hello'), LexToken(),')',1,6))
    """
    def f(acc):
        return (None,acc,None)
    return (paren(pr) | pr.treat(f) )
    
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
    def dot(pr):
        pr_sample = Parse.next_token().if_test(b_not_delimiter)
        return (pr.many().treat(lib.flatten).
                setsample(sample.plus(pr_sample,None)))
    return dot(Parse.first([
        Parse.next_token().if_test(b_not_delimiter).plus() ,
        LazyParse((lambda p: delimit(p(lambda_true),'(',')')),balanced_condition,'balanced','paren') ,
        LazyParse((lambda p: delimit(p(lambda_true),'[',']')),balanced_condition,'balanced','brack') ,
        LazyParse((lambda p: delimit(p(lambda_true),'{','}')),balanced_condition,'balanced','curly')]))

def balanced() -> Parse:
    return balanced_condition(lambda_true)

def brace_semi():
    """construct parser for brace-delimited delimiter-balanced semicolon separated list
 
    output {,(b,;),}, where b[i] is a list of (possibly highly nested list) of tokens.

    Use brace_semif instead to flatten out the nested lists.
    
    The empty brace {} is not permitted.
    
    >>> pstream(brace_semi(),'{ a ; b { b1 ; (d) } ; c }')
    (LexToken({,'{',1,0), [[LexToken(VAR,'a',1,2)], LexToken(;,';',1,4),...
    """
    def p(tok):
        return not(tok.value in  [';','.'])
    nosemi = balanced_condition(p).name('balanced-semi')
    return brace(nosemi.plus(next_value(';')))

def _brace_semi_fflatten(acc):
    """Treatment to remove nesting in the Token lists produced
    by brace_semi.
    
    >>> pstream(brace_semi().treat(_brace_semi_fflatten),'{ a ; b { b1 ; (d) } ; c }')
    (LexToken({,'{',1,0), [[LexToken(VAR,'a',1,2)], LexToken(;,';',1,4), [LexToken(VAR,'b',1,6), LexToken({,'{',1,8), LexToken(VAR,'b1',1,10)...
    """
    (a,b,c) = acc 
    b[0::2] = [lib.fflatten(u) for u in b[0::2]] # semi at odds
    return (a,b,c)

def brace_semif():
    return brace_semi().treat(_brace_semi_fflatten,'brace_semif')

def plus_comma(pr:Parse) -> Parse:
    """construct parser for comma-separated list"""
    return pr.plus(next_value(','))

def many_comma(pr:Parse) -> Parse:
    """parser for comma separated list
    
    >>> pstream(many_comma(next_any_word()),'this,that,other')
    [LexToken(WORD,'this',1,0), LexToken(,,',',1,4), LexToken(WORD,'that',1,5),...
    """
    return pr.many(next_value(','))

def andcomma():
    """parse a comma or the value 'and'
    
    >>> pstream(andcomma(),', ...')
    LexToken(,,',',1,0)
                                                                
    >>> pstream(andcomma(),'and ...')
    LexToken(WORD,'and',1,0)
    """
    return (next_value(',') | next_value('and'))

def plus_andcomma(pr:Parse) -> Parse:
    """construct parser for and/comma separated list

    >>> pstream(plus_andcomma(next_any_word()),'this , that and other')
    [LexToken(WORD,'this',1,0), LexToken(,,',',1,5), LexToken(WORD,'that',1,7),...
    """
    return pr.plus(andcomma())

def plus_or(pr:Parse) -> Parse:
    """construct parser for 'or' separated list
    
    >>> pstream(plus_or(next_any_word()),'this or that or other')
    [LexToken(WORD,'this',1,0), LexToken(WORD,'or',1,5), LexToken(WORD,'that',1,8)...
    """
    return pr.plus(next_value('or'))


if __name__ == "__main__":
    import doctest
    from tokenlib import Item
    doctest.testmod(optionflags=doctest.ELLIPSIS | doctest.NORMALIZE_WHITESPACE)
#    doctest.testmod(verbose=True, optionflags=doctest.ELLIPSIS | doctest.NORMALIZE_WHITESPACE)
#    doctest.testmod()

    

    

    
    