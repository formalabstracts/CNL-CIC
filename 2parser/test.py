#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 17 12:03:18 2021

@author: thales
"""

import inner_lexer

#test lexer

inner_lexer.tokenizer.input(r"hello( ) my:hi [ ] | ! . ? ?3 ?44 + * $ : / my_a/:\abc!\[$")
'(',')','[',']','|','!','.','+','*','$',':','/'

#while True:
#    tok = inner_lexer.tokenizer.token()
#    if not tok:
#        break
#    print(tok)
    
print('the'.upper())
    
