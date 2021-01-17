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

def init_item(s) -> Item:
    """Intialize item stream with a tuple of tokens"""
#   # a token used for cloning
    if len(s) > 0:
        init_item.tok = s[0]
    return Item(pos=0,stream=s,acc=None)

#v = init_item([3,4,5])
#print(init_item.tok)

def next_item(item:Item) -> Item:
    """Advance to the next item of the stream.
    The stream is left unchanged.
    Accumulated tokens discarded, retaining current token.
    """
    if item.pos >= len(item.stream):
        raise StopIteration
    return Item(pos = item.pos+1,stream = item.stream,
                acc = item.stream[item.pos])


if __name__ == "__main__":
    import doctest
    doctest.testmod()