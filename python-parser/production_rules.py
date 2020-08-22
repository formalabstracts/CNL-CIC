#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 22 17:00:38 2020

@author: thales
"""

"""production rules for Colada"""

import copy
import word_lists
import lib
import parser_combinator as c
from parser_combinator import (Parse, ParseError, 
                               first_word, 
                               first_phrase, next_word,
                               next_phrase, next_value)

def cs_brace(cs_parse:Parse,brace_parse:Parse) -> Parse:
    """control sequence parser including arguments in braces"""
    return cs_parse + c.brace(brace_parse).many()

def phrase_list_transition():
    """parser for transition phrases"""
    prs = [Parse.phrase(s) for s in word_lists.transition]
    return (Parse.first(prs) + Parse.word('that').possibly()).nil()

def phrase_list_filler():
    """parser for filler words"""
    return (Parse.word('we').possibly() + first_word('put write have know see') + 
            Parse.word('that').possibly()).nil()

def phrase_list_proof_statement():
    """parser for canned proof statements"""
    return (Parse.phrase("we proceed as follows") |
            (Parse.word('the') + 
             first_word('result lemma theorem proposition corollary') +
             Parse.word('now').possibly() +
             Parse.word('follows')) |
            Parse.phrase('the other cases are similar') |
            (Parse.phrase('the proof is')+ first_word('obvious trivial easy routine'))).nil().expect('canned')

# case_sensitive_word -> use next_value(s)

# Atomic identifiers cannot be a single letter (a short var)
# wordlike atomic identifiers are case insensitive and can have synonym.
#  but hierarchical identifiers are always case sensitive.

def atomic():
    #I forget why I am converting integers.
    """parser for atomic identifiers, converting words and integers as needed"""
    def f(item):
        item1 = Parse.next_token().process(item)
        result = item1.tok
        if result.type == 'INTEGER' or result.type == 'WORD':
            tok = copy.copy(result)
            if tok.type == 'WORD':
                tok.value = c.synonymize(tok.value)
            tok.type = 'ATOMIC_IDENTIFIER'
            return (tok,item1)
        if result.type == 'ATOMIC_IDENTIFIER':
            return result
        raise ParseError(item)
    return Parse(f).expect('atomic')

def var():
    """parser for variables"""
    return Parse.next_token().if_type(['VAR']).expect('var')

def var_or_atomic():
    """parser for a var or atomic"""
    return (var() | atomic()).expect('var_or_atomic')

def var_or_atomics():
    """parser for a sequence of one or more var or atomics"""
    return Parse.plus(var_or_atomic())

def hierarchical_identifier():
    """parser for hierarchical identifiers"""
    return Parse.next_token().if_type(['HIERARCHICAL_IDENTIFIER'])

def identifier():
    """parser for hierarchical or atomic identifier"""
    return (atomic() | hierarchical_identifier()).expect('identifier')

# canned phrases that have small variants
# lit[w] gives parser for w-like words or phrases
    
lit_dict = {
    'a' : first_word('a an'), #indefinite
    'article' : first_word('a an the'),
    'defined-as' : first_phrase(['said to be','defined as','defined to be']),
    'is' : first_phrase(['is','are','be','to be']),
    'iff':  (first_phrase(['iff','if and only if']) | 
             (first_phrase(['is','are','be','to be']) + next_word('the').possibly() + next_word('predicate'))),
    'denote': first_phrase(['denote','stand for']),
    'do': first_word('do does'),
    'equal': next_phrase('equal to'),
    'has': first_word('has have'),
    'with': first_word('with of having'),
    'true': first_word('on true yes'),
    'false': first_word('off false no'),
    'wrong': next_phrase('it is wrong that'),
    'exist': next_word('exist'),
    'lets': first_phrase(['let','let us','we','we can']),
    'fix': first_word('fix let'),
    'assume': first_word('assume suppose'),
    'then': first_word('then therefore hence'),
    'choose': first_word('take choose pick'),
    'prove': first_word('prove show'),
    'say': first_word('say write'),
    'we-say': (next_word('we').possibly() +
            first_word('say write') +
            next_word('that').possibly()
            ),
    'assoc': first_word('left right no'),
    'field-key': first_word('coercion notationless notation parameter type call'),
    'qed': first_word('end qed obvious literal'),
    'document': first_word('document article section subsection subsubsection subdivision division'),
    'end-document': first_word('endsection endsubsection endsubsubsection enddivision endsubdivision'),
    'def': first_word('def definition'),
    'axiom': first_word('axiom conjecture hypothesis equation formula'),
    'with-property': next_phrase('with property'),
    'param': next_phrase('with parameter'),
    'theorem': first_word('proposition theorem lemma corollary'),
    # type proposition property classsifier atomic 
    }

def lit(s):
    """parser generator for 's'-like words or phrases"""
    if s =='record':
        return (Parse.word('we').possibly() +
                first_word('record register') +
                Parse.word('identification').possibly() +
                Parse.word('that').possibly())
    if s == 'doc':
        return lit['document'] | lit['end-document']
    if s == 'location':
        return Parse.first([lit['document'],lit['theorem'],lit['axiom']])
    return lit_dict[s]

#others:
#label = atomic
#period Parse.value('.')
#renamed map -> call

# instructions do nothing except store for now

instruct = {}

def param_value(ls):
    if ls == []:
        return ''
    tok = ls[0]
    if tok.type == 'INTEGER':
        return int(tok.value)
    if tok.value.lower() in ['yes','true','on']:
        return True
    if tok.value.lower() in ['no','false','off']:
        return False
    return tok.value
  
def expand_slashdash(vs):
    """expanding synonyms
    e.g. word/-ing is short for word/wording"""
    for i in range(len(vs)):
        if vs[i]== '/-':
            vs[i]= '/'
            vs[i+1]= vs[i-1]+vs[i+1]
    return [v for v in vs if v != '/']
#test 
#print(expand_slashdash(['work','/-','ing','/','effort','workaround']))
#['work', 'working', 'effort', 'workaround']

def syn():
    """parsing synonyms"""
    def p(tok):
        tok.value in ['/','/-'] or c.can_wordify(tok)
    synlist = Parse.next_token().if_test(p).plus()
    return c.comma_nonempty_list(synlist)

def instruction():
    """parsing and processing of synonyms and other instructions"""
    def treat_syn(acc):
        for ac in acc:
            vs = [t.value for t in ac]
            v_expand = expand_slashdash(vs)
            c.synonym_add(v_expand)
            return ()
    def treat_instruct(acc):
        keyword,ls = acc
        instruct[keyword.value] = param_value(ls)
        return ()
    keyword_instruct = (first_word("""exit timelimit printgoal dump 
                     ontored read library error warning""") + 
                     Parse.next_token().possibly())
    return (c.bracket(next_word('synonym') + syn().treat(treat_syn) |
         c.bracket(keyword_instruct.treat(treat_instruct))))
 
def this_exists():
    """parsing of 'this'-directives.
    DEBUG: Remove this feature. Deprecated Unfinished"""
    def adjective(tok):
        s1 = tok.value.lower.replace('_','')
        return s1 in ['unique','canonical','welldefined','wellpropped','total','exhaustive']
    def this_directive_right_attr():
        return next_phrase('by recursion')
    def this_directive_pred():
        return c.andcomma_nonempty_list(Parse.next_token().if_test(adjective))
    return first_phrase(['this exist','this is'])

def post_colon_balanced():
    def p(token):
        return token.value not in ['end','with',':=',';','.',',','|',':']
    return c.balanced_condition(p)

def opt_colon_type():
    """parsing ': A'.  No treatment applied"""
    return (next_value(':') + post_colon_balanced()).treat(lib.snd).possibly().treat(lib.flatten)

def meta_tok():
    tok = copy.copy(c.init_item.tok)
    tok.value = str(meta_tok.count)
    tok.type = 'META'
    meta_tok.count += 1
    return tok 

def opt_colon_type_meta():
    def trt(acc):
        if acc == []:
            return meta_tok()
        return acc
    return opt_colon_type().treat(trt)

annotated_var = c.paren(var() + opt_colon_type())

annotated_sort_vars = c.paren(var().plus() + opt_colon_type_meta())

annotated_vars = c.paren(var().plus() + opt_colon_type_meta())

def let_annotation_prefix():
    return (next_word('let') + c.comma_nonempty_list(var()) +
     next_word('be') + lit['a'].possibly() +
     next_word('fixed').possibly())
    
def let_annotation():
    return ((first_word( 'fix let') + c.comma_nonempty_list(annotated_sort_vars)) |
     let_annotation_prefix() + post_colon_balanced())
    
then_prefix = lit['then'].possibly()

def assumption():
    assumption_prefix = lit['lets']+ lit['assume'] + next_word('that').possibly()
    return ((assumption_prefix + c.balanced() + next_value('.')) |
            let_annotation() + next_value('.'))

possibly_assumption = (assumption().many() + then_prefix)

axiom_preamble = lit['axiom']+ atomic() + next_value('.')

moreover_statement = next_word('moreover') + c.balanced() + next_value('.')

axiom = possibly_assumption + c.balanced() + next_value('.') + moreover_statement.many()

#ref_item = and_comma_nonempty_list(lit['location'].possibly() + atomic())

#by_ref = paren(next_word('by') + ref_item).possibly()


    
    
    

    
    


#def op_colon_type_meta():

            

  
            
            