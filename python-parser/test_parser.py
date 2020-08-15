#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 14 10:35:50 2020

@author: radix
"""
import lexer
import parser

def test_hello():
    print('hi')
    assert 1 == 1
    
print(parser.__dict__)
    
def mk_item_stream(s:str):
    l = lexer.tokenizer.input(s)
    print(l)
    return parser.init_item(l)

def test_hello_world():
    it = mk_item_stream("Hello World!")
    vals = ['Hello','World','!']
    for i in range(3):
       it = parser.next_item(it)
       print(it)
       assert it.token.value == vals[i]
       
test_hello_world()
        


    




