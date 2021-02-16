#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 17 10:44:00 2021

@author: thales

The token class is obtained from the lexer.
A LexToken object has attributes 
tok.type, tok.value, tok.lineno, and tok.lexpos:
https://www.dabeaz.com/ply/ply.html (Sec 4.1)
"""

import lexer
import copy
import lib
from collections import namedtuple
from exception import (ParseError,ParseNoCatch)
from exception import ErrorItem
from ply.lex import LexToken

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
         
    >>> print(mk_token())
    LexToken(INTEGER,'1',0,0)
    
    >>> mm = mk_token({'type' : 'RED','value' :'blue'})
    >>> print(f'mm={mm}')
    mm=LexToken(RED,'blue',0,0)
    """
    return copy_token(mk_token._tok,attr)

def init_mk_token():
    """Call this once to initialize mk_token."""
    lexer.tokenizer.input('1') #stream with a single element
    mk_token._tok = [tok for tok in lexer.tokenizer][0]
    mk_token._tok.__setattr__('lineno',0)
    mk_token._tok.__setattr__('lexpos',0)
    mk_token._tok.__setattr__('lexer',None)
    pass

init_mk_token()

# A derived token has the token fields + nonterminal + production (rule)
# The lineno,lexpos are at start position of parse, including ignored toks
# The value may be structured.

    

# An item is a token embedded at a particular position of the tuple of tokens.
# The stream and individual tokens remain immutable.  
# pos changes (position index into stream).
# acc (which appears ubiquitously) 
#  is the accumulator holding the parsed data

Item = namedtuple('Item','stream pos acc')

def item_repr(self):
    s = self.stream[0:self.pos] + ['***'] + self.stream[self.pos:]
    return (f'Item({s},acc={self.acc})')

Item.__repr__ = item_repr

def init_item(s) -> Item:
    """Intialize item stream with a tuple of tokens"""
#   # a token used for cloning
    #if s:
    #    init_item.tok = s[0]
    return Item(pos=0,stream=s,acc=None)

#v = init_item([3,4,5])
#print(init_item.tok)

def mk_stream(s:str):
    """This function is primarily for debugging.
    It creates an initialized item-stream from a string,
    which is tokenized with the lexer.
    """
    lexer.tokenizer.input(s)
    return init_item(list(lexer.tokenizer))

def eof(item):
    """True if at end of stream"""
    return item.pos >= len(item.stream)

def next_item(item:Item) -> Item:
    """Advance to the next item of the stream.
    The stream is left unchanged.
    NoCatch Exception if accumulated nonempty.  Nonempty would be a bug in this program.
    """
    if item.pos >= len(item.stream):
        raise ParseError(ErrorItem(item=item,nonterminal='eof'))  #StopIteration
    if item.acc:
        raise ParseNoCatch([ErrorItem(item=item,nonterminal='.')])
    return Item(pos = item.pos+1,stream = item.stream,
                acc = item.stream[item.pos])

def update(acc,item:Item) -> Item:
    """Create a new item with replaced accumulator"""
    return Item(pos = item.pos,stream = item.stream,acc = acc)

class Etok:
    """
    This is the class that represents the nodes 
    in the abstract syntax tree.  I'm not using subclasses for now.
    So the representation is rather basic.  Subclasses can be simulated
    by the name of the Etok.
    
    name: convention- uppercase if the 'type' of a terminal token
        This means that the rule acts as the value
          lowercase if the name of the nonterminal.
        
    rule = '': optional production_rule string
    raw = [LexToken]: flat token list from the original text, for error messages
    misc = None: optional additional data, not belong anywhere else.
        For ease of traversal,
        misc should not contain any Etoks at any level of nesting.
    altrepr = '': optional string reprsentation of Etok, can be value of a Tok
    etoks: (nested) Etok list, children in AST
    
    We generally expect a production rule to output a single Etok.
    
    We should try to avoid having to extract essential data from raw.
    Otherwise, there should be no redundant information in Etok.
    
    >>> try:
    ...     Etok(['hi'],[],[])
    ... except TypeError as e:
    ...     print(e) 
    Etok string expected in name:['hi']
    """
    def __init__(self,name,etoks,raw,rule='',misc=None,altrepr=''):
        self.name = name
        self.etoks = etoks
        if isinstance(etoks,Etok):
            self.etoks = [etoks]
        self.raw = Etok.get_raw(raw)
        self.rule = rule
        self.altrepr = altrepr
        self.misc = misc
        self.validate()
        
    def __repr__(self):
        if self.altrepr:
            return f"Etok({self.altrepr})"
        name2 = self.name
        if self.rule:
            name2 += ','+self.rule
        return f"Etok({name2},'{self.rawstring()}')"
    
    def s_expression(self):
        def s2(es):
            if isinstance(es,Etok):
                return es.s_expression()
            if lib.iterable(es):
                return '['+' '.join([s2(e) for e in es if e ])+']'
            if not(es):
                return ''
        name2 = self.name 
        if self.rule:
            name2 += '-'+self.rule 
        re = ''
        if lib.fflatten(self.etoks):
            re = s2(self.etoks)
        #' '.join([e.s_expression() for e in lib.fflatten(self.etoks) if e])
        if re:
            re = ' '+re
        return (f"({name2}"+re+')')
    
    def rawstring(self):
        return ' '.join([tok.value for tok in self.raw])
    
    def validate(self):
        if not(isinstance(self.name,str)):
            raise TypeError('Etok string expected in name:'+str(self.name))
        if not(isinstance(self.altrepr,str)):
            raise TypeError('Etok string expected in altrepr:'+str(self.altrepr))
        if not(isinstance(self.rule,str)):
            raise TypeError('Etok string expected in rule:'+str(self.rule))
        for e in self.raw:
            if not(isinstance(e,LexToken)):
                raise TypeError(f'raw must be list of Tok in {self.name}, not {str(e)}')
        for e in lib.fflatten(self.etoks):
            if e and not(isinstance(e,Etok)): 
                raise TypeError(f'etoks must be a list of Etok in {self.name}, not {e}:{type(e).__name__}')
        pass
    
    @property
    def type(self):
        """Augment Etok with fields of token"""
        return self.name 
    
    @property 
    def value(self):
        return self.rule 
    
    @property 
    def lineno(self):
        return self.raw[0].lineno 
    
    @property 
    def lexpos(self):
        return self.raw[0].lexpos
    
    def update(self,d:dict ={}):
        """Use a dictionary d to update self.
        If the dictionary is empty, a copy of self is made.
        """
        e1 = Etok(name=self.name,etoks=self.etoks,raw=self.raw,rule=self.rule,misc=self.misc,altrepr=self.altrepr)
        for key,value in d.items():
            if key=='raw':
                value = Etok.get_raw(value)
            setattr(e1,key,value)
        e1.validate()
        return e1
    
    def _get_raw1(tok):
        if (isinstance(tok,Etok)):
            return tok.raw
        if isinstance(tok,LexToken):
            return [tok]
        return []
    
    def get_raw(etoks):

        """extract the raw fields from a 
        nested list of Etoks and LexTokens"""
        return lib.flatten([Etok._get_raw1(e) for e in lib.fflatten(etoks)])

    def etok(tok):
        """convert a token to Etok"""
        return Etok(tok.type,[],[tok],rule=tok.value)
    
    def parse(p : 'Parse'):
        """Promote a LexToken parser to a Etok parser.
        This is the same as p.treat(Etok.etok)
        """
        return p.treat(Etok.etok)




if __name__ == "__main__":
    import doctest
    doctest.testmod()