#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 11 08:51:02 2020

@author: thales
"""
#import lib 

def assert_true(m,r):
    if (r!=True):
        print(f'Error: {m}')
 
file='lib'

def test_flatten_compress():
    assert_true('flatten',lib.flatten([[1,2],[3,4],[5,6]])==[1,2,3,4,5,6])
    assert_true('compress',lib.compress([10,11,12,13,14],[0,2,4])==[10,12,14])

def f(a,b):
    return a+100*b

def test_lib_list():    
    assert_true('swap',lib.swap(f)(3,7)== 7+100*3)
    assert_true('curry',lib.curry(f)(3)(7) == f(3,7))
    assert_true('part',lib.part([0,2,4])([10,11,12,13,14])==[10,12,14])    
    assert_true('fst',lib.fst((3,4))==3)
    assert_true('snd',lib.snd((3,4))==4)
    assert_true('prepend',lib.prepend((0,[1,2]))==[0,1,2])

import lexer
file='lexer'

singular = {
    'bosses':'bosz',
    'boss':'bosz',
    'runner':'runner',
    'does':'do',
    'bodies':'body',
    'body':'body',
    'redress':'redresz',
    'potatoes':'potato',
    'tomatoes':'tomato',
    'daisies':'daisy',
    'carries':'carry',
    'realizes':'realize',
    'crunches':'crunch',
    'squashes':'squash',
    'misses':'misz',
    'miss':'misz',
    'cos':'cos',
    'goes':'go',
    'writes':'write',
    'gas':'gas',
    'gasses':'gasz',
    'is':'is',
    'arccos':'arcco',
    'raises':'raise'
    }

def test_singular():
    for key in singular:
        assert_true('singular.'+key,lexer.singularize(key)==singular[key])

def print_tokens(s:str):
    lexer.lex.lineno=1
    lexer.tokenizer.input(s)
    p = ''
    for tok in lexer.tokenizer:
        p += "('{ty}',r'{val}'),".format(ty=tok.type,val=tok.value)
    print(s+' -> ['+p+']')
    print('*'*10)
    print('\n')
    
#print_tokens("hello world")
    
def token_ok(t,c):
    (ty,v) = c
    return t.type == ty and t.value == v
    
def assert_token_list(ts,ls):
    z = zip(ts,ls)
    for (t,c) in z:
        assert_true(t.value,token_ok(t,c))
    
raw_lex = {
    "hello":[('WORD','hello')],
    r'A B C hello\\alpha33\alpha[1]there !ready! \begin':[('VAR',r'A'),('VAR',r'B'),('VAR',r'C'),('WORD',r'hello'),('LAMBDA',r'\\'),('ATOMIC_IDENTIFIER',r'alpha33'),('CONTROLSEQ',r'\alpha'),('[',r'['),('INTEGER',r'1'),(']',r']'),('WORD',r'there'),('SYMBOL',r'!'),('WORD',r'ready'),('SYMBOL',r'!'),('CONTROLSEQ',r'\begin')],
    r'\\ \n Riemann-Hilbert %comment \n\n more ':[('LAMBDA',r'\\'),('CONTROLSEQ',r'\n'),('WORD',r'riemann'),('SYMBOL',r'-'),('WORD',r'hilbert')],
    "some-pre %comment \n more":[('WORD',r'some'),('SYMBOL',r'-'),('WORD',r'pre'),('WORD',r'more')],
    "and \n\nmore":[('WORD',r'and'),('WORD',r'more')],
    """multiline
     here it is
     moreover""":[('WORD',r'multiline'),('WORD',r'here'),('WORD',r'it'),('WORD',r'is'),('WORD',r'moreover')],
    r'#4 # 5  $ _id ))))))"hello"':[('SYMBOL',r'#'),('INTEGER',r'4'),('SYMBOL',r'#'),('INTEGER',r'5'),('SYMBOL',r'$'),('BLANK',r'_'),('WORD',r'id'),(')',r')'),(')',r')'),(')',r')'),(')',r')'),(')',r')'),(')',r')'),('STRING',r'"hello"')],
    r'v__3 a__77 33 33.2 alpha.beta alpha.2 .33 bye. ':[('VAR',r'v__3'),('VAR',r'a__77'),('INTEGER',r'33'),('DECIMAL',r'33.2'),('HIERARCHICAL_IDENTIFIER',r'alpha.beta'),('HIERARCHICAL_IDENTIFIER',r'alpha.2'),('FIELD_ACCESSOR',r'.33'),('WORD',r'bye'),('PERIOD',r'.')],
    r'* + _ - ~ ^ ! | / = < > @ # $ & ? ` .. ; \\ ,':[('SYMBOL',r'*'),('SYMBOL',r'+'),('BLANK',r'_'),('SYMBOL',r'-'),('SYMBOL',r'~'),('SYMBOL',r'^'),('SYMBOL',r'!'),('ALT',r'|'),('SLASH',r'/'),('SYMBOL',r'='),('SYMBOL',r'<'),('SYMBOL',r'>'),('SYMBOL',r'@'),('SYMBOL',r'#'),('SYMBOL',r'$'),('SYMBOL',r'&'),('SYMBOL',r'?'),('SYMBOL',r'`'),('SYMBOL',r'..'),(';',r';'),('LAMBDA',r'\\'),(',',r',')],
    r'\* \+ \_ \- \~ \^ \! \| \/ \= \< \> \@ \# \$ \& \? \` \.. \; \\ \,':[('CONTROLSEQ',r'\*'),('CONTROLSEQ',r'\+'),('CONTROLSEQ',r'\_'),('CONTROLSEQ',r'\-'),('CONTROLSEQ',r'\~'),('COERCION',r'\^'),('CONTROLSEQ',r'\!'),('CONTROLSEQ',r'\|'),('CONTROLSEQ',r'\/'),('CONTROLSEQ',r'\='),('CONTROLSEQ',r'\<'),('CONTROLSEQ',r'\>'),('CONTROLSEQ',r'\@'),('CONTROLSEQ',r'\#'),('CONTROLSEQ',r'\$'),('CONTROLSEQ',r'\&'),('CONTROLSEQ',r'\?'),('CONTROLSEQ',r'\`'),('CONTROLSEQ',r'\.'),('PERIOD',r'.'),('CONTROLSEQ',r'\;'),('LAMBDA',r'\\'),('CONTROLSEQ',r'\,')],
    r'\** \++ \__ \-- \~~ \^^ \!! \|| \// \== \<< \>> \@@ \## \$$ \&& \?? \`` \.. \;; \\ \,,':[('CONTROLSEQ',r'\*'),('SYMBOL',r'*'),('CONTROLSEQ',r'\+'),('SYMBOL',r'+'),('CONTROLSEQ',r'\_'),('BLANK',r'_'),('CONTROLSEQ',r'\-'),('SYMBOL',r'-'),('CONTROLSEQ',r'\~'),('SYMBOL',r'~'),('COERCION',r'\^'),('SYMBOL',r'^'),('CONTROLSEQ',r'\!'),('SYMBOL',r'!'),('CONTROLSEQ',r'\|'),('ALT',r'|'),('CONTROLSEQ',r'\/'),('SLASH',r'/'),('CONTROLSEQ',r'\='),('SYMBOL',r'='),('CONTROLSEQ',r'\<'),('SYMBOL',r'<'),('CONTROLSEQ',r'\>'),('SYMBOL',r'>'),('CONTROLSEQ',r'\@'),('SYMBOL',r'@'),('CONTROLSEQ',r'\#'),('SYMBOL',r'#'),('CONTROLSEQ',r'\$'),('SYMBOL',r'$'),('CONTROLSEQ',r'\&'),('SYMBOL',r'&'),('CONTROLSEQ',r'\?'),('SYMBOL',r'?'),('CONTROLSEQ',r'\`'),('SYMBOL',r'`'),('CONTROLSEQ',r'\.'),('PERIOD',r'.'),('CONTROLSEQ',r'\;'),(';',r';'),('LAMBDA',r'\\'),('CONTROLSEQ',r'\,'),(',',r',')],
    '(){}[];:;.;,.;':[('(',r'('),(')',r')'),('{',r'{'),('}',r'}'),('[',r'['),(']',r']'),(';',r';'),('COLON',r':'),(';',r';'),('PERIOD',r'.'),(';',r';'),(',',r','),('PERIOD',r'.'),(';',r';')],
    r'\qed\mid\tmid\alt\sub\^\to\mapsto\blank\\\lambda\lam\Pity\forall\exists\existsunique':[('SYMBOL_QED',r'\qed'),('MID',r'\mid'),('TMID',r'\tmid'),('ALT',r'\alt'),('APPLYSUB',r'\sub'),('COERCION',r'\^'),('ARROW',r'\to'),('MAPSTO',r'\mapsto'),('BLANK',r'\blank'),('LAMBDA',r'\\'),('LAMBDA',r'\lambda'),('LAMBDA',r'\lam'),('PITY',r'\Pity'),('QUANTIFIER',r'\forall'),('QUANTIFIER',r'\exists'),('QUANTIFIER',r'\existsunique')]
    }

print('hello')

def test_tok():
    for key in raw_lex:
        lexer.tokenizer.input(key)
        print(key)
        assert_token_list(list(lexer.tokenizer),raw_lex[key])
        
test_tok()

#print(lexer.tokenizer.__dict__)

#lexer.tokenizer.input("this is it.")
#for i in range(4):
#    print(lexer.tokenizer.next())


        
