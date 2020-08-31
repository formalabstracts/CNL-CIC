#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug  4 09:24:36 2020

@author: thales
"""

import ply.lex as lex
import msg
import word_lists
import re

# XX we should be able to collapse keyword symbol into a single token class'
# MID TMID ASSIGN ARROW BLANK ALT COLON APPLYSUB etc.
tokens = (
    'STRING',
    'CONTROLSEQ',
    'DECIMAL',
    'INTEGER',
    'SYMBOL',
#    'KEY_SYMBOL',
    'SYMBOL_QED',
    'MAPSTO',
    'MID',
    'TMID',
    'ASSIGN',
    'ARROW',
    'BLANK',
    'ALT',
    'PERIOD',
    'COLON',
    'APPLYSUB',
    #'SLASH',
    #'SLASHDASH',
    'COERCION',
    'LAMBDA',
    'PITY',
    'QUANTIFIER',
    'VAR',
    #'METAVAR',
    'WORD',
    'ATOMIC_IDENTIFIER',
    'HIERARCHICAL_IDENTIFIER',
    'FIELD_ACCESSOR',
    'UNKNOWN',
    'TEX_ERROR'
)

# literals:'L_PAREN','R_PAREN','L_BRACK','R_BRACK','L_BRACE','R_BRACE','PERIOD','COMMA','SEMI',

#output must not terminate in 's'; mangle with 'sz'
singularize_patterns = [
    (re.compile(s),e) for s,e in [
        (r'(.*[^s])ss+','sz'),
        (r'(.*[^aeiou]o)es',''),
        (r'(.*[^aeiou])ies','y'),
        (r'(.*i[sz]e)s$',''),
        (r'(.*ch|.*sh|.*x|.*z)es',''),
        (r'(.*[^s])s+es','sz'),
        (r'(.*[^s])s+',''),
        (r'(.*)','')
        ]
    ]

def singularize(s):
    s = s.lower()
    if len(s) <= 3 or not(s.endswith('s')) or s in word_lists.singular:
        return s
    for (p,e) in singularize_patterns:
        match = p.fullmatch(s)
        if match:
            return match.group(1)+e


t_ignore_COMMENT = '%.*'
 
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Compute column.
 #     input is the input text string
 #     token is a token instance
def find_column(input, token):
     line_start = input.rfind('\n', 0, token.lexpos) + 1
     return (token.lexpos - line_start) + 1
 
t_ignore = ' \t\r\f\v'

def t_error(t):
     print("Illegal character '%s'" % t.value[0])
     t.lexer.skip(1)
     
def t_TEX_ERROR(t): 
    r'\[TeX2Cnl(Error|Warning)\s*"([^"]*)"\s*\]'
    msg.error("Error in TeX file: %s" % t.value)
    pass

literals = ['(',')','{','}','[',']',',',';',':']

t_STRING = r'"[^"]*"' # no escaping.

def t_DECIMAL(t):
    r'[+-]?\d+\.\d+'
    return t

def t_INTEGER(t):
    r'[+-]?\d+'
    return t

def t_FIELD_ACCESSOR(t):
    "[.][a-zA-Z0-9'][a-zA-Z0-9_']*"
    return t

def t_HIERARCHICAL_IDENTIFIER(t):
    r"[a-zA-Z0-9'][a-zA-Z0-9_']*([.][a-zA-Z0-9'][a-zA-Z0-9_']*)+"
    return t

is_varlong = re.compile(r"__[a-zA-Z0-9']*")
is_var = re.compile(r"[a-zA-Z][0-9_']*")
is_word = re.compile(r"[A-Za-z][a-z]+")

def t_ATOMIC_IDENTIFIER(t):
    r"[a-zA-Z0-9'][a-zA-Z0-9_']*"
    if (t.value == '_'):
        t.type = 'BLANK'
    elif is_varlong.fullmatch(t.value):
        t.type = 'VAR'
    elif is_var.fullmatch(t.value):
        t.type = 'VAR'
    elif is_word.fullmatch(t.value):
        t.type = 'WORD'
        t.rawvalue = t.value
        t.value = singularize(t.value)
    return t

def rawvalue(tok):
    if tok.value == 'WORD':
        return tok.rawvalue
    return tok.value

def token_length(t):
    return len(rawvalue(t))

reserved_symbols = {
    '.'  : 'PERIOD', 
    ':'  : 'COLON', 
    ':=' : 'ASSIGN', 
    '->' : 'ARROW', 
    '|->': 'MAPSTO', 
    '|'  : 'ALT', 
    '/'  : 'SLASH', 
    #'/-' : 'SLASHDASH',
    '_'  : 'BLANK'
    }

def t_SYMBOL(t):
    r'([-*+_~^!|/=<>@#$&?`]|[.:])+'
    t.type = reserved_symbols.get(t.value,'SYMBOL')
    return t

reserved_control = {
    r'\qed': 'SYMBOL_QED',
    r'\mid': 'MID',
    r'\tmid': 'TMID',
    r'\alt': 'ALT',
    r'\sub': 'APPLYSUB',
    r'\^': 'COERCION',
    r'\to': 'ARROW',
    r'\mapsto': 'MAPSTO',
    r'\blank': 'BLANK',
    r'\\': 'LAMBDA',
    r'\lambda': 'LAMBDA',
    r'\lam': 'LAMBDA',
    r'\Pity': 'PITY',
    r'\forall': 'QUANTIFIER',
    r'\exists': 'QUANTIFIER',
    r'\existsunique': 'QUANTIFIER'
    }

def t_CONTROLSEQ(t):
    #first group is t_SYMBOL
    r'\\([-*+_~^!|/=<>@#$&?`|[.:]|[;\\,]|[a-zA-Z]+)'
    t.type = reserved_control.get(t.value,'CONTROLSEQ')
    return t 

tokenizer = lex.lex()












