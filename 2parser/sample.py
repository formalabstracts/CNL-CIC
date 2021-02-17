#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb 16 05:48:26 2021

@author: thales

Generate random samples from parsers

"""

from numpy.random import (poisson , binomial, randint)

from tokenlib import (Item , Etok, mk_stream)

import lib

def bernoulli(p):
    return binomial(1,p)

def ran(ls):
    if not ls:
        return ls
    return ls[randint(0,len(ls))]

def mk_tok(v):
    toks = mk_stream(v)
    return toks.stream[0]

def next_token():
    return mk_tok('blah')

def none():
    return None

def add_sample(self,other):
    def sample():
        acc1 = self.sample()
        acc2 = other.sample()
        return (acc1,acc2)
    return sample

def or_sample(self,other):
    def sample():
        if bernoulli(0.5):
            return self.sample()
        return other.sample()
    return sample

def treat_sample(self,treatment):
    def sample():
        return treatment(self.sample())
    return sample 

def some(self,sep,m):
    def sample():
        if sep:
            if m==0:
                return []
            return lib.flatten((self.sample(),sep.sample()) for _ in range(0,m-1))+[self.sample()]
        return [self.sample() for _ in range(0,m-1)]
    return sample

def plus(self,sep):
    return some(self,sep,1 + poisson(3))
         
def many(self,sep):
    return some(self,sep,0 + poisson(3))

def atleast(self,n):
    return some(self,None,n + poisson(3))

def possibly(self):
    def sample():
        if bernoulli(0.5):
            return self.sample()
        return None
    return sample

def if_test(self,p):
    def sample():
        iteration_limit = 10 # arbitrary limit
        for _ in range(0,iteration_limit):
            acc = self.sample() # randomized guess
            if p(acc):
                return acc 
        return next_token() # give up on test
    return sample

def if_value(v):
    def sample():
        return mk_tok(v)
    return sample

def if_rawvalue(v):
    return if_value(v)

def type_sample(ty:str):
    """ 
    >>> type_sample('WORD')
    '...'
    """
    d = {'STRING': ['"'+s+'"' for s in 'hello world so little time'.split()],
         'CONTROLSEQ':['\\'+s for s in 'alpha beta gamma delta sum prod deg circ ast lneg times rtimes'],
         'DECIMAL':['3.14','2.718','1.0','4.96'],
         'INTEGER': list(range(0,10)) ,
         'SYMBOL':['<','>','!=','+','-','*','^'],
         'SYMBOL_QED':['\\qed'],
         'MAPSTO':['\mapsto'],
         'MID':['\mid'],
         'TMID':['\tmid'],
         'ASSIGN':[':='],
         'ARROW':['\to'],
         'BLANK':['_'],
         'ALT':['|'],
         'PERIOD':['.'],
         'COLON':[':'],
         'APPLYSUB':['\\sub'],
         'COERCION': ['\\^'],
         'LAMBDA':['\\lambda'],
         'PITY':['\\Pity'],
         'QUANTIFIER':['\\forall','\\exists'],
         'VAR':'a b c x y z a1 a2 x1 x33 y7 u v w'.split(),
         'WORD':"""estimate equation solution expression inequality random sample 
             mean pair ordered function evaluate order operation property divisible 
             exponent base multiple square common prime form factorization point 
             plane line angle ray parallel intersecting perpendicular regular 
             polygon degree circle diameter chord similar congruent symmetry 
             leg triangle scalene equilateral trapezoid rotation transformation 
             translation polyhedron integer positive opposite value origin 
             coordinate area circumference word number blah part""".split(),
         'ATOMIC_IDENTIFIER':'foo_bar bar3 foo22 sin_ cos_ atan2 ceil_ comb_ fabs_ factorial_ floor_ gcd_ sqrt_ log2 log10 pow_ '.split(),
         'HIERARCHICAL_IDENTIFIER':['math.pi','math.ceil','math.abs'],
         'FIELD_ACCESSOR':['.assoc','.distrib'],
         'UNKNOWN':['?'],
         'TEX_ERROR':['\\error']
         }
    return ran(d[ty])

def if_types(tys):
    """ 
    >>> if_types(['WORD','INTEGER','DECIMAL'])()
    LexToken(...)
    """
    def sample():
        ty = ran(tys)
        return mk_tok(type_sample(ty))
    return sample

def all_sample(prs):
    def sample():
        return [p.sample() for p in prs]
    return sample

def first(prs):
    def sample():
        i = randint(0,len(prs))
        return prs[i].sample()
    return sample

def lazy_call(pr):
    def sample():
        return pr().sample()
    return sample

def first_word(ss):
    def sample():
        s = ran(ss.split(' '))
        return mk_tok(s)
    return sample

if __name__ == "__main__":
    import doctest

    doctest.testmod(optionflags=doctest.ELLIPSIS | doctest.NORMALIZE_WHITESPACE)
#    doctest.testmod(verbose=True, optionflags=doctest.ELLIPSIS | doctest.NORMALIZE_WHITESPACE)
#    doctest.testmod()

    

