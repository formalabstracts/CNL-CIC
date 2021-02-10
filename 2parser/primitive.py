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
prim_classifier
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

from exception import ParseError, ParseNoCatch, DataProcess, ErrorItem
import lib, word_lists
import tokenlib
import lexer

import parser_combinator as c

import production_rules as r

from production_rules import (Macro)

from parser_combinator import (Parse, Etok,
                               first_word, 
                               first_phrase, next_word, next_any_word,
                               next_phrase, next_value,
                               pstream)

def unwrap(etok,s):
    if not(s):
        return etok
    if not(len(etok.etoks) == 1):
        raise DataProcess(f'singleton expected, found {etok.etoks}')
    if isinstance(s,str):
        if (not(etok.name)==s):
            raise DataProcess(f'{s} expected, found {etok}')
        return etok.etoks[0]
    return unwrap(unwrap(etok,s[0]),s[1:])

def drill(etok,f):
    pass

class Prim_data:
    """primitive dict has keys (ptype:str,ikey:str)
    # ptype is one of the primitives above,
    # ikey is the first word.
    # value in dict is the parser of the remaining segment.
    """
    prim_dict = {}
    
    def add(etok,key,value,s):
        if key in Prim_data.prim_dict:
            raise DataProcess((etok,key,value,s))
        Prim_data.prim_dict[key]=value
        
def prim_classifier_add(etok):
    """
    >>> e = pstream(Macro.classifier_def(),'Let function, symbol, object be classifiers')
    >>> unwrap(e,['classifier_def','classifier_word_pattern'])
    
    >>> prim_classifier_add(e)

    >>> Prim_data.prim_dict
    """
    for e in etok.etoks:
        Prim_data.add(etok,('prim_classifier',c.getvalue(e)),None,f'prim_classifier {e} already declared')
    pass

def prim_classifier_parse():
    def p(tok):
        return ('prim_classifier',c.getvalue(tok)) in Prim_data.prim_classifier
    return c.next_any_word().if_test(p).treat(Etok.etok,'prim_classifier_parse')

if __name__ == "__main__":
    import doctest
    doctest.testmod(optionflags=doctest.ELLIPSIS | doctest.NORMALIZE_WHITESPACE)
