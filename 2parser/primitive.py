#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb 10 13:48:42 2021

@author: Thomas Hales

Storage and parsing of primitives
"""

"""
prim_adjective
prim_adjective_multisubject
prim_binary_relation_controlseq
prim_binary_relation_op
prim_binder_prop
+prim_classifier
prim_definite_noun
prim_field_prop_accessor
prim_field_term_accessor
prim_field_type_accessor
prim_identifier_term
prim_identifier_type
prim_lambda_binder
prim_pi_binder
prim_possessed_noun
prim_propositional_op
prim_propositional_op_controlseq
prim_relation
prim_simple_adjective
prim_simple_adjective_multisubject
prim_structure
prim_term_controlseq
prim_term_op
prim_term_op_controlseq
prim_type_controlseq
prim_type_op
prim_type_op_controlseq
prim_typed_name
prim_verb
prim_verb_multisubject
"""

from exception import (ParseError, ParseNoCatch, 
                       DataProcess, ErrorItem)

import lib
#import word_lists
#import tokenlib
#import lexer

import parser_combinator as c

import production_rules as r

from production_rules import (Macro,Pattern)

from parser_combinator import (Parse, Etok,
                               first_word, 
                               first_phrase, next_word, next_any_word,
                               next_phrase, next_value,
                               pstream)

#DataProcess (line,etok,str,...)

def line(etok):
    """String giving the starting line number of Etok."""
    raw = etok.raw
    if raw:
        return f'line={raw[0].lineno}.'
    return ''

def unwrap(etok,s):
    """Raise DataProcess if the etok is not s
    Return etoks
    """
    if not(s):
        return etok
    if (not(etok.name)==s):
        raise DataProcess((line(etok),etok,f'{s} expected, found {etok}'))
    return etok.etoks

def traverse_tree(etok,f):
    """Recursively traverse over etok tree 
    replacing e -> f(e).
    Has side effect on etok.
    Return f(etok-recursively-modified))
    """
    if not etok:
        return etok
    if lib.nonstringiterable(etok):
        return [traverse_tree(e,f) for e in etok]
    etok.etoks = traverse_tree(etok.etoks,f)
    return f(etok)

def all_tree(p,etok):
    """Recursively check predicate p is true on etok tree"""
    return p(etok) and all(p(e) for e in lib.fflatten(etok.etoks) if e)
    
def collect_tree(etok,f):
    """Recursively call f on etok tree. No return.
    State can be stored in f to collect data.
    """
    f(etok)
    for e in lib.fflatten(etok.etoks):
        if e:
            collect_tree(e,f)
    pass

def collect_list(etok,f=None):
    """Specialization of collect_tree.
    Collect f from etok tree into a list
    
    f:Etok -> A 
    
    >>> list(set(collect_list(pstream(r.tdop_rel_prop(),'x,y,z PRIM_BINARY_RELATION_OP u PRIM_BINARY_RELATION_OP x'))))
    ['app_term', 'VAR', 'prim_binary_relation_op', 'app_args', 'tdop_rel_prop', 'tightest_term']
    """
    data=[]
    if not(f):
        f= (lambda e: e.name)
    collect_tree(etok,(lambda e: data.append(f(e))))
    return data

def collect_name(etok,s):
    """
    Make a list of all subtokens with name s.
    >>> collect_name(pstream(r.tdop_rel_prop(),'x,y,z PRIM_BINARY_RELATION_OP u PRIM_BINARY_RELATION_OP x'),'VAR')
    [Etok(VAR,x,'x'), Etok(VAR,u,'u'), Etok(VAR,y,'y'), Etok(VAR,u,'u'), Etok(VAR,z,'z'), Etok(VAR,u,'u'), Etok(VAR,u,'u'), Etok(VAR,x,'x')]

    ## get duplicates because binary relation processing does de-chaining.
    """
    data = []
    def action(e):
        if e.name==s:
            data.append(e)
    collect_tree(etok,action)
    return data
    

class Prim_data:
    """primitive dict has keys (ptype:str,ikey:str)
    # ptype is one of the primitives above,
    # ikey is the first word.
    # value in dict is the parser of the remaining segment.
    """
    prim_dict = {}
    
    def add(etok,key,value):
        if key in Prim_data.prim_dict:
            raise DataProcess((line(etok),etok,f'{key} already declared'))
        Prim_data.prim_dict[key]=value
        
def prim_classifier_add(etok):
    """
    >>> e = pstream(Macro.classifier_def(),'Let function, symbol, object be classifiers')
    >>> unwrap(e,'classifier_word_pattern')
    [Etok(WORD,function,'function'), ...
    
    >>> prim_classifier_add(e)

    >>> Prim_data.prim_dict[('prim_classifier','symbol')]
    """
    e = unwrap(etok,'classifier_word_pattern')
    for e in etok.etoks:
        Prim_data.add(etok,('prim_classifier',c.getvalue(e)),None)
    pass

def prim_classifier_parse():
    """Parser for a prim_classifier word
    
    >>> pstream(prim_classifier_parse(),'object') #added in earlier test
    Etok(WORD,object,'object')
    """
    def p(tok):
        return ('prim_classifier',c.getvalue(tok)) in Prim_data.prim_dict
    return c.next_any_word().if_test(p).treat(Etok.etok,'prim_classifier_parse')


    
def strip_syn_from_word_extended(etok):
    """Action for processing synonyms.
    
    Synonyms are removed from word_extended and added 
    to synonym list.
    
    >>> etok = pstream(Pattern.word_extended(),'syntax (or semantics)')
    >>> strip_syn_from_word_extended(etok).s_expression()
    '(word_extended [(WORD-syntax)])'
    
    >>> c.synonymize('syntax')
    'semantic syntax'
    """
    if not etok:
        return etok 
    if etok.name == 'word_extended':
        (w,wp,o)=etok.etoks
        if o:
            c.synonym_add([c.getvalue(w),c.getvalue(o)])
            return etok.update({'etoks':(w,wp,None)})
    return etok 

def get_prim_adjective_pattern(etok):
    """
    prim_adjective_pattern with
    (no synonyms, no optional material, no annotation)
    
    >>> e=pstream(Pattern.word_pattern(),'integrable with respect to x')
    >>> traverse_tree(e,get_prim_adjective_pattern).s_expression()

    """
    if not etok:
        return etok
    if etok.name=='word_extended':
        (w,x,y)=etok.etoks
        if x or y:
            raise DataProcess(f'simple word_pattern expected {etok}')
        return w
    if etok.name=='annotated':
        raise DataProcess(f'simple word_pattern expected (no annotation) {etok}')
    return etok
    
    

def quick_prim_adjective_add(etok):
    """Add a parser production rule for prim_adjective.
    Word pattern only. 
    No definition.
    No synonmyms.
    No optional material."""
    e1 = get_prim_adjective_pattern(etok)
    vars = collect_name(e1, 'VAR')
    if not (len(vars)==len(list(set(vars)))):
        raise DataProcess(f'linear variable patter expected in {etok}')
    d = {v : 'TERM' for v in vars}
    Prim_data.add(etok,('prim_adjective',), value)
    
    
    
    
    



if __name__ == "__main__":
    import doctest
    doctest.testmod(optionflags=doctest.ELLIPSIS | doctest.NORMALIZE_WHITESPACE)
