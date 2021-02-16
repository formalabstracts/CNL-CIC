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
    raw = etok.raw
    if raw:
        return f'line={raw[0].lineno}.'
    return ''

def unwrap(etok,s):
    if not(s):
        return etok
    if (not(etok.name)==s):
        raise DataProcess((line(etok),etok,f'{s} expected, found {etok}'))
    return etok.etoks

def drill(etok,f):
    pass

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

def traverse_tree(etok,f):
    """Recursively traverse over etok tree 
    replacing e -> f(e).
    Has side effect on etok.
    """
    etok.etoks = [traverse_tree(e,f) for e in etok.etoks]
    return f(etok)

def all_tree(etok,p):
    """Recursively check predicate p is true on etok tree"""
    return p(etok) and all(p(e) for e in lib.fflatten(etok.etoks) if e)
    
def collect_tree(etok,f):
    """Recursively call f on etok tree. No return.
    State can be stored in f to collect data.
    """
    for e in lib.fflatten(etok.etoks):
        f(e)
    f(etok)
    pass

def collect_list(etok,f=None):
    """Specialization of collect_tree.
    Collect f from etok tree into a list
    
    f:Etok -> A 
    
    >>> list(set(collect_list(pstream(r.tdop_rel_prop(),'x,y,z prim_binary_relation_op u prim_binary_relation_op x'))))
    ['app_term', 'prim_binary_relation_op', 'tdop_rel_prop']
    """
    data=[]
    if not(f):
        f= (lambda e: e.name)
    collect_tree(etok,(lambda e: data.append(f(e))))
    return data



    
    
def strip_syn_from_word_extended(etok):
    """Action for processing synonyms.
    
    Synonyms are removed from word_extended and added 
    to synonym list.
    
    >>> etok = pstream(Pattern.word_extended(),'syntax (or semantics)')
    >>> strip_syn_from_word_extended(etok).s_expression()
    '(word_extended [(WORD-syntax) []])'
    
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

def prim_adjective_add(etok):
    """Add a parser production rule for prim_adjective"""
    



if __name__ == "__main__":
    import doctest
    doctest.testmod(optionflags=doctest.ELLIPSIS | doctest.NORMALIZE_WHITESPACE)
