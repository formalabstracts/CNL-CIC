#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 18 12:37:25 2021

@author: thales
"""

import production_rules

import state

from tokenlib import mk_stream
from parser_combinator import pstream
import production_rules as r
from tokenlib import Etok



state.state.mk_sample=True

empty = mk_stream('')


def sample_parse(pr):
    return (pr.process(empty).acc)

def brief_sig(e):
    if e==[]:
        return '[]'
    if e==None:
        return 'none'
    if isinstance(e,tuple):
        es = ','.join([brief_sig(e1) for e1 in e])
        return f'({es})'
        #return f'({len(e)})'
    if isinstance(e,list):
        es = ','.join([brief_sig(e1) for e1 in e])
        return f'[{es}]'
        #return f'[{len(e)}]'
    if isinstance(e,Etok):
        return (f'{e.name}')
    return f'{e}?'

def sig(etok):
    es = ', '.join([brief_sig(e) for e in etok.etoks])
    return f'({etok.name}: {es})'
    

#def signature(etok):
#    n = etok.name 
#    es = etok.etoks 
#    if not es:
#        return f'({n})'
#    if isinstance(es,Etok):
#        return f('({n} es.name)')
#    if lib.nonstringiterable(es):
#        es = [signature( for es]
#        return f('({n})')

for _ in range(0,0):
    print(sample_parse(production_rules.utterance()))
    print('\n')
    
for _ in range(0,0):
    sample_parse(production_rules.utterance())
 
for _ in range(0,0):
    print(sig(sample_parse(production_rules.Pattern.word_pattern())))
    
for _ in range(0,15):
    print(sig(sample_parse(production_rules.Pattern.word_extended())))
    
state.state.mk_sample=False

print(pstream(r.get_lookup_parse('statement'),'[STATEMENT x < y]'))



