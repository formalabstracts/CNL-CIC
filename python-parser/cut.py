#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 15 12:21:52 2020

@author: thales
"""

# cut test_parser.py

input1 = "Hello World"

tok = lexer.tokenizer

tokst = tuple(tok.input(input1))

def next_token(pos):
    if pos >= len(tokst):
        raise StopIteration
    return (tokst[pos],pos+1)    

class stream:
    def __init__(self,st,pos=0):
        self.tup = tuple(st)
        self.pos = pos
        
    def __iter__(self):
        return self
        
    def __next__(self):
        if self.pos >= len(self.tup):
            raise StopIteration
        t = self.tup[self.pos]
        self.pos += 1
        return (t,self.pos)

tok_input = stream(tok)

for i in tok_input:
    print(i)

#(a1,tok_input2) = tok_input.next()

for i in range(5):
    print(next(tok_input))

print(a1)
print('.')

for i in tok:
    print(i)
    
