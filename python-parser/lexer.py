#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug  4 09:24:36 2020

@author: thales
"""

import ply.lex as lex

tokens = (
    'STRING',
    'CONTROLSEQ',
    'DECIMAL',
    'INTEGER',
    'SYMBOL',
    'SYMBOL_QED',
    'MAPSTO',
    'MID',
    'TMID',
    'ASSIGN',
    'ARROW',
    'BLANK',
    'ALT',
    'APPLYSUB',
    'SLASH',
    'SLASHDASH',
    'COERCION',
    'LAMBDA',
    'PITY',
    'QUANTIFIER',
    'VAR',
    'METAVAR',
    'WORD',
    'ATOMIC_IDENTIFIER',
    'HIERARCHICAL_IDENTIFIER',
    'FIELD_ACCESSOR',
    'EOF',
    'UNKNOWN'
)

#    'L_PAREN',
#    'R_PAREN',
#    'L_BRACK',
#    'R_BRACK',
#    'L_BRACE',
#    'R_BRACE',
#    'PERIOD',
#    'COMMA',
#    'SEMI',
#   'COLON',

import re

#output must not terminate in 's'; mangle with 'sz'
singularize_patterns = [
    (re.compile(s),e) for s,e in [
        (r'(.*[^s])ss+','sz'),
        (r'(.*[^aeiou]o)es',''),
        (r'(.*[^aeiou])ies','y'),
        (r'(.*i[sz]e)s$',''),
        (r'(.*ch|.*sh|.*x|.*z)es',''),
        #(r'(.*[^s]e)s',''),
        (r'(.*[^s])s+es','sz'),
        (r'(.*[^s])s+',''),
        (r'(.*)','')
        ]
    ]

def singularize(s):
    if len(s) <= 3 or not(s.endswith('s')):
        return s
    for (p,e) in singularize_patterns:
        match = p.fullmatch(s)
        if match:
            return match.group(1)+e

print([singularize(s) for s in ['boss','bosses','runner','does','bodies','body',
                                'redress','potatoes','tomatoes',
                                'daisies','carries',
                                'realizes','crunches','squashes',
                                'misses','miss','cos','goes','writes',
                                'gas','gasses','arccos']
       ])

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
 
t_ignore = r' \t\n\r\f\v'

def t_error(t):
     print("Illegal character '%s'" % t.value[0])
     t.lexer.skip(1)

literals = ['(',')','{','}','[',']','.',',',';',':']

t_STRING = r'"([^\"]|\[\"nt])*"' 

t_CONTROLSEQ = r'\[a-zA-Z]+'

def t_DECIMAL(t):
    r'[+-]?\d+.\d+'
    return t

def t_INTEGER(t):
    r'[+-]?\d+'
    return t

def t_FIELD_ACCESSOR(t):
    ".[a-zA-Z0-9'][a-zA-Z0-9_']*"
    return t


    

reserved_symbols = {
    '.'  : 'PERIOD', 
    ':'  : 'COLON', 
    ':=' : 'ASSIGN', 
    '->' : 'ARROW', 
    '|->': 'MAPSTO', 
    '|'  : 'ALT', 
    '/'  : 'SLASH', 
    '/-' : 'SLASHDASH',
    '_'  : 'BLANK'
    }

def t_SYMBOL(t):
    r'[*+^=<>/!@#$&_-|:.?~`]+'
    t.type = reserved_symbols.get(t.value,default='SYMBOL')
    return t

reserved_control = {
    r'\qed' : 'SYMBOL_QED',
    r'\mid' : 'MID',
    r'\tmid': 'TMID',
    r'\alt' : 'ALT',
    r'\sub' : 'APPLYSUB',
    r'\^'   : 'COERCION',
    r'\to'  : 'TO',
    r'\mapsto' : 'MAPSTO',
    r'\blank' : 'BLANK',
    r'\\' : 'LAMBDA',
    r'\lambda' : 'LAMBDA',
    r'\lam' : 'LAMBDA',
    r'\Pity' : 'PITY',
    r'\forall' : 'QUANTIFIER',
    r'\exists' : 'EXISTS',
    r'\existsunique' : 'QUANTIFIER'
    }

def t_CONTROLSEQ(t):
    r'\([*+^=<>/!@#$&_-|:.?~`,;|\]|[a-zA-Z]+)'
    t.type = reserved_control.get(t.value,default='CONTROLSEQ')
    return t 

