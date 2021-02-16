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

#print(poisson(7.0))

#print(bernoulli(0.25)) # 0 or 1.

#print(randint(0,33))

random_item = Item(stream=['RX'],pos=0,acc=None)

def ran(ls):
    if not ls:
        return ls
    return ls[randint(0,len(ls))]

def next_token():
    return Etok(name='WORD',rule='blah')

# finished,identity,probe,nocatch,commit -> identity
# fail -> fail
# reparse reparse_list, custom,
# if test -> custom

def add_sample(self,other):
    def sample():
        return (self.sample(),other.sample())
    return sample

def or_sample(self,other):
    def sample():
        if bernoulli(0.5):
            return self.sample()
        return other.sample()
    return sample

def some(self,sep,m):
    def sample():
        if sep:
            return lib.flatten((self.sample(),sep.sample()) for _ in range(0,m))
        return [self.sample() for _ in range(0,m)]
    return sample

def plus(self,sep):
    return some(self,sep,1 + poisson(1))
         
def many(self,sep):
    return some(self,sep,0 + poisson(1))

def at_least(self,n):
    return some(self,None,n + poisson(2))

def possibly(self):
    def sample():
        if bernoulli(0.5):
            return self.sample()
        return None
    return sample

def if_value(v):
    def sample():
        toks = mk_stream(v)
        return toks.stream[0]
    return sample

#print (if_value('the')())

def if_rawvalue(v):
    def sample():
        return Etok(name='WORD',rule=v)
    return sample

def type_sample(ty:str):
    d = {'STRING': ['"'+s+'"' for s in 'hello world so little time'.split()],
         'CONTROLSEQ':['\\'+s for s in 'alpha beta gamma delta sum prod deg circ ast lneg times rtimes'],
         'DECIMAL':['3.14','2.718','1.0','4.96'],
         'INTEGER':['0','1','2','12','1728','37'],
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
             polygon degree circle radius diameter chord similar congruent symmetry 
             leg triangle scalene equilateral trapezoid rhombus rotation transformation 
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
    def sample():
        ty = ran(tys)
        return Etok(ty,type_sample(ty))
    return sample

def all(prs):
    def sample():
        return [p.sample() for p in prs]
    return sample

def first(prs):
    def sample():
        i = randint(0,len(prs))
        return prs[i].sample()
    return sample

def lazy_call(pr):
    return pr().sample


    


# identity -> identity
# fail -> fail.
# 


    

