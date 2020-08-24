#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 14 10:35:50 2020

@author: hales
"""
import copy
import ply
import lib
import lexer
import parser_combinator as pc

# test Item

def test_hello():
    print('hi')
    assert 1 == 1
    
print('test-parser')
    
tokenizer = lexer.tokenizer 
#print(f'tokenizer={tokenizer}')
tokenizer.input('hello')
#print(tokenizer.__dict__)
t = next(tokenizer)
#print(t)
tokenizer.input('and again')
t = next(tokenizer)
#print(t)
#print('-'*4)
for t in tokenizer:
    #print(t)
    pass
    
def test_lex():
    tokenizer.input('new string')
    for tok in tokenizer:
        print(tok.__dict__)
        print(type(tok))
        print(ply.lex.LexToken.__dict__)
        assert tok.value in ['new','string']

test_lex()

def mk_item_stream(s:str):
    tokenizer.input(s) #re-initialization
    return pc.init_item([tok for tok in tokenizer])

def test_hello_world():
    it = mk_item_stream("Hello World!")
    vals = ['hello','world','!']
    for i in range(3):
       it = pc.next_item(it)
       #print(it.acc)
       assert it.acc.value == vals[i]
       
test_hello_world()

#### 

def test_update():
    its = mk_item_stream('Hello')
    it = pc.next_item(its)
    assert it.acc.value == 'hello'
    tok = copy.copy(it.acc)
    tok.value = 'bye'
    it1 = pc.update(tok,its)
    assert it1.acc.value == 'bye'
    
def test_next_token():  #call as well.
    its = mk_item_stream('Hello there!')
    #print(f'its={its}')
    p = pc.Parse.next_token().process
    its1 = p(its)
    #print(f'its={its} -')
    #print(f'its1={its1}')
    #its2 = p.process(its)
    #print(f'its2={its2}')
    assert its1.acc.value == 'hello'
    
test_update()
test_next_token()

def test_add():
    its = mk_item_stream('Hello there')
    p = pc.Parse.next_token()
    p2 = p + p
    its1 = p2.process(its)
    #print(its1)
    (a,b) = its1.acc
    assert ('hello','there') == (a.value,b.value)

test_add()

def test_or():
    its = mk_item_stream('Hello there')
    p = pc.Parse.next_token()
    q = pc.Parse.identity().if_test(lambda _ : False)
    pq = p | q
    its1 = pq.process(its)
    assert its1.acc.value == 'hello'
    qp = q.if_test(lambda _: False) | p
    its2 = qp.process(its)
    #print(its2)
    assert its2.acc.value == 'hello'
    
test_or()

#def test_compose():
#    its = mk_item_stream('Hello there')
#    p = pc.Parse.next_token()
#    p2 = p.compose(p)
#    assert p2.process(its).acc.value == 'there'
    
#test_compose()

def test_nocatch():
    its = mk_item_stream('Hello there')
    p = pc.Parse.next_token().if_test(lambda _ : False)
    try:
        p.process(its)
        assert False
    except pc.ParseError:
        assert True
    try:
        p.nocatch('msg').process(its)
        assert False
    except pc.ParseNoCatch:
        assert True
         
test_nocatch()

def test_treat():
    its = mk_item_stream('Hello there')
    p = pc.Parse.next_token()
    def t(_):
        return 0
    its1 = p.treat(t).process(its)
    assert its1.acc == 0
    
test_treat()

def test_many():
    its = mk_item_stream('Hello there # and more')
    p = pc.Parse.next_token().if_type('WORD').many()
    its1 = p.process(its)
    #print(its1)
    vals = [t.value for t in its1.acc]
    assert vals == ['hello','there']
    
test_many()

def test_plus():
    its = mk_item_stream('Hello there # and more')
    p = pc.Parse.next_token().if_type('WORD').plus()
    its1 = p.process(its)
    #print(its1)
    vals = [t.value for t in its1.acc]
    assert vals == ['hello','there']
    try:
        q = pc.Parse.next_token().if_type('SYMBOL').plus()
        its1= q.process(its)
        assert False
    except pc.ParseError:
        assert True
        
test_plus()

def test_possibly():
    its = mk_item_stream('Hello there # and more')
    p = pc.Parse.next_token().if_type('WORD').possibly()
    its1 = p.process(its)
    #print(its1)
    vals = [t.value for t in its1.acc]
    assert vals == ['hello']
    q = pc.Parse.next_token().if_type('SYMBOL').possibly()
    its1= q.process(its)
    vals = [t.value for t in its1.acc]
    assert vals == []
    
test_possibly()

def test_identity():
    its = mk_item_stream('Hello there # and more')
    p = pc.Parse.identity()
    its1 = p.process(its)
    assert its1 == its
    
test_identity()

def test_nil():
    its = mk_item_stream('Hello there # and more')
    p = pc.Parse.next_token().nil()
    its1 = p.process(its)
    assert its1.acc == []
    
test_nil()

def test_separated_nonempty_list():
    its = mk_item_stream('Hello, there,   and more')
    p = pc.Parse.next_token().if_type('WORD')
    sep = pc.Parse.next_token().if_value(',')
    its1 = p.separated_nonempty_list(sep).process(its)
    vals = [t.value for t in its1.acc]
    #print(its1)
    assert vals == ['hello','there','and']
    its1 = p.separated_list(sep).process(its)
    vals = [t.value for t in its1.acc]
    assert vals == ['hello','there','and']
    
test_separated_nonempty_list()

def test_if_test_treat():
    pass

def test_all():
    its = mk_item_stream('Hello there#and more')
    p = pc.Parse.next_token().if_type('WORD')
    q = pc.Parse.next_token().if_type('SYMBOL')
    its1 = pc.Parse.all([p,p,q,p]).process(its)
    vals = [t.value for t in its1.acc]
    #print(its1)
    assert vals == ['hello','there','#','and']

test_all()

def test_first():
    its = mk_item_stream('#Hello there#and more')
    p = pc.Parse.next_token().if_type('WORD')
    q = pc.Parse.next_token().if_type('SYMBOL')
    its1 = pc.Parse.first([p,p,q,p]).process(its)
    assert its1.acc.value == '#'
    
test_first()

def test_wordify():
    its = mk_item_stream('x')
    p = pc.Parse.next_token()
    tok = p.process(its).acc
    tok1 = pc.wordify(p.process(its).acc)
    assert pc.can_wordify(tok)
    assert tok1.value == 'x' and tok1.type == 'WORD'
    
test_wordify()

#print (pc.synonym)
def test_synonym():
    pc.synonym_add(['hi there','hithere']) # error
    pc.synonym_add(['such']) # error
    pc.synonym_add(['hi']) # error
    pc.synonym_add(['alpha3']) #error
    pc.synonym_add(['roundtrip','journey','stars'])
    s = pc.synonymize('star')
    #print(f's={s}')
    assert s == 'journey roundtrip star'
    assert pc.synonymize('yes') == 'yes'
    its = mk_item_stream('X roundtrips')
    p = pc.Parse.next_token()
    its1 = p.process(its)
    its2 = p.process(its1)
    assert pc.synw(its1.acc) == 'x'
    assert pc.synw(its2.acc) == 'journey roundtrip star'
    
test_synonym()

def test_next_word():
    its = mk_item_stream('Hello X journey there   now  and then')
    p = pc.next_word('hello') 
    its1 = p.process(its)
    assert its1.acc.value == 'hello'
    
    #next_any_word
    p = pc.next_any_word()
    its2 = p.process(its1)
    assert its2.acc.value == 'x'
    
    #next_any_word_except
    p = pc.next_any_word_except(['star']) # synonym with journey.
    try:
        its3 = p.process(its2)
        assert False
    except pc.ParseError:
        assert True
        
    #first_word
    p = pc.first_word('X journey there hello now')
    #print(f'its={its}')
    itp = p.process(its)
    #print(f'itp={itp}')
    assert itp.acc.value == 'hello'
    
    #next_phrase
    p = pc.next_phrase('star there now')
    #print(its2)
    its3 = p.process(its2)
    val = [t.value for t in its3.acc]
    #print(val)
    assert val == ['journey roundtrip star','there','now']
    
    #first_phrase
    p = pc.first_phrase(['yes','not ever','and even','and then'])
    its4 = p.process(its3)
    #print(its4)
    val = [t.value for t in its4.acc]
    #print(val)
    assert val == ['and','then']

test_next_word()

def test_until():
    pass

def test_delimit():
    its = mk_item_stream('(hello){there}')
    its1 = pc.paren(pc.next_any_word()).process(its)
    #tok = its1.acc
    #print(tok)
    vs = [t.value for t in its1.acc]
    assert vs == ['hello']
    its2 = pc.brace(pc.next_any_word()).process(its1)
    assert its2.acc[0].value == 'there'
    
test_delimit()

def test_balanced_condition():
    its = mk_item_stream('(Hi and (yet[+]) .) there (Bud) {#}.2 ')
    p = pc.balanced_condition(pc.can_wordify)
    its1 = p.process(its)
    #print(its1.acc)
    vs = [t.value for t in its1.acc]
    #print(vs)
    assert vs == ['(', 'hi', 'and', '(', 'yet', '[','+',']',')', '.', ')', 'there', '(', 'bud', ')', '{', '#', '}']
    
#test_balanced_condition()

#def show(toks):
    

#print('starting test_brace_semi')

def test_brace_semi():
    #its = mk_item_stream('{a1 ; a2 ; a3 ; a4; a5}')
    its = mk_item_stream('{Hi and ; (yet[+]) . ;there (Bud) ; {#;}.1}')
    p = pc.brace_semi()
    #print("starting_process")
    its1 = p.process(its)
    toks = its1.acc
    #print('stopping_process')
    #print(its1)
    toks = toks[::-1][2]
    #print(f'toks={toks}')
    vs = [t.value for t in toks]
    #print(vs)
    assert vs == ['(', 'yet', '[', '+', ']', ')', '.']
    
test_brace_semi()  
    


    




    
    

    
    
        
        
        


        


    




