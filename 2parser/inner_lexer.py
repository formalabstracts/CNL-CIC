#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jan  9 13:38:26 2021

@author: thales
"""

import ply.lex as lex
#import regex

#ID identifier, REPS rep count on parser, ESCAPED \..., LABEL assign a var
tokens = (
    'ID',
    'TY',
    'ESCAPED',
    'LABEL'
)

t_ignore = ' \t\r\f\v'

def t_error(t):
     print("Illegal inner lexer character '%s'" % t.value[0])
     t.lexer.skip(1)

literals = ['(',')','[',']','|','!','.','+','*','$','/','?']

def t_LABEL(t):
    r'[a-zA-Z_0-9]+:'
    t.value = t.value[:-1]
    return t

def t_ID(t):
    r'[a-zA-Z_0-9]+'
    if t.value.isupper():
        t.type = 'TY'
    return t

def t_ESCAPED(t):
    r'\\([a-zA-Z]+|[]:(){}[|!.+*$/])'
    t.value = t.value[1:]
    return t

tokenizer = lex.lex()

