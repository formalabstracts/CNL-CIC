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
from collections import namedtuple
from exception import ParseError
from exception import ErrorItem

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
# acc is the accumulator holding the parsed data

Item = namedtuple('Item','stream pos acc')

def item_repr(self):
    s = self.stream[0:self.pos] + ['***'] + self.stream[self.pos:]
    return (f'Item({s},acc={self.acc}')

Item.__repr__ = item_repr

def init_item(s) -> Item:
    """Intialize item stream with a tuple of tokens"""
#   # a token used for cloning
    #if s:
    #    init_item.tok = s[0]
    return Item(pos=0,stream=s,acc=None)

#v = init_item([3,4,5])
#print(init_item.tok)

def next_item(item:Item) -> Item:
    """Advance to the next item of the stream.
    The stream is left unchanged.
    NoCatch Exception if accumulated nonempty.  Nonempty would be a bug in this program.
    """
    if item.pos >= len(item.stream):
        raise ParseError(ErrorItem(item=item,nonterminal='eof',production=''))  #StopIteration
    if item.acc:
        raise ParseNoCatch([ErrorItem(item=item,nonterminal='.',production='')])
    return Item(pos = item.pos+1,stream = item.stream,
                acc = item.stream[item.pos])

def update(acc,item:Item) -> Item:
    """Create a new item with replaced accumulator"""
    return Item(pos = item.pos,stream = item.stream,acc = acc)

if __name__ == "__main__":
    import doctest
    doctest.testmod()