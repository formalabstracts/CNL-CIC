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
import lexer
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
    prs = [next_phrase(s) for s in word_lists.transition]
    return (Parse.first(prs) + next_word('that').possibly()).nil()

def phrase_list_filler():
    """parser for filler words"""
    return (Parse.word('we').possibly() + first_word('put write have know see') + 
            Parse.word('that').possibly()).nil()


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
        return Parse.first([lit_dict['document'],lit_dict['theorem'],lit_dict['axiom']])
    return lit_dict[s]

#others:
#label = atomic

period = next_value('.').clear_history()
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

# 

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

# differ only in treatment
opt_colon_sort  = opt_colon_type()
opt_colon_sort_meta = opt_colon_type_meta



annotated_var = c.paren(var() + opt_colon_type())

annotated_sort_vars = c.paren(var().plus() + opt_colon_type_meta())

annotated_vars = c.paren(var().plus() + opt_colon_type_meta())

def let_annotation_prefix():
    return (next_word('let') + c.comma_nonempty_list(var()) +
     next_word('be') + lit('a').possibly() +
     next_word('fixed').possibly())
    
def let_annotation():
    return ((first_word( 'fix let') + c.comma_nonempty_list(annotated_sort_vars)) |
     let_annotation_prefix() + post_colon_balanced())

# PROOFS

class Proof:
    """Parser constructors for proof statements"""
    
    def canned():
        """parser for canned proof statements"""
        return (next_phrase("we proceed as follows") |
                (next_word('the') + 
                 first_word('result lemma theorem proposition corollary') +
                 next_word('now').possibly() +
                 next_word('follows')) |
                next_phrase('the other cases are similar') |
                (next_phrase('the proof is')+ first_word('obvious trivial easy routine'))).nil().expect('canned')

    then_prefix = lit('then').possibly()
    
    def assumption():
        assumption_prefix = lit('lets')+ lit('assume') + next_word('that').possibly()
        return ((assumption_prefix + c.balanced() + period) |
                let_annotation() + period)
    
    possibly_assumption = (assumption().many() + then_prefix)
    
    axiom_preamble = lit('axiom')+ atomic().possibly() + period
    
    moreover_statement = next_word('moreover') + c.balanced() + period
    
    axiom = axiom_preamble + possibly_assumption + c.balanced() + period + moreover_statement.many()
    
    ref_item = c.andcomma_nonempty_list(lit('location').possibly() + atomic())
    
    by_ref = c.paren(next_word('by') + ref_item).possibly()
    
    def by_method():
        def no_that(tok):
            return tok.value == 'that' # exclude avoids goal_prefix ambig.
        return (next_word('by') + 
                 (first_phrase(['contradiction','case analysis']) |
                  (next_word('induction') + 
                   (next_word('on') + c.balanced_condition(no_that)).possibly())) +
                 Parse.probe(next_word('that')| period))
    
    choose_prefix = then_prefix + lit('lets').possibly() + lit('choose')
    
    canned_prefix = c.andcomma_nonempty_list(phrase_list_transition())
    
    goal_prefix = ((lit('lets').possibly() + lit('prove') + next_word('that')) |
                   (by_method() + next_word('that')).possibly())
    
    preamble = ((next_word('proof') + by_method().possibly() + period) |
                      next_word('indeed'))
    
    def affirm():
        return Proof.statement() | Proof.goal()
    
    def statement():
        return Proof.then_prefix + c.balanced() + Proof.by_ref + period + Proof.moreover_statement.many() + Proof.script.possibly()
    
    def goal():
        return Proof.goal_prefix + c.balanced() + Proof.by_ref + period + Proof.script()

    def script():
        return (Proof.preamble + (Proof.canned_prefix + Proof.body + Proof.canned_prefix + Proof.tail).many() + lit('qed') + period)
    
    def body():
        return (Proof.tail() |  Proof.assumption())
    
    def tail():
        return (Proof.affirm() | Proof.canned | Proof.case() | Proof.choose())
    
    def case():
        return (next_word('case') + c.balanced() + period + Proof.choose_justify())
    
    def choose():
        return (Proof.choose_prefix + c.balanced() + period + Proof.justify())
    
    def justify():
        return (Proof.script().possibly())


# patterns 

class Pattern:
    """Parser generators for patterns"""
    
    def next_any_unbanned():
        """Parser for any word except for pattern keywords.  
        The token must be a WORD."""
        pattern_banned = [
            'is','be','are','denote','define','enter','namespace','stand',
            'if','iff','inferring','the','a','an','we','say','write',
            'assume','suppose','let','said','defined','or','fix','fixed'
            ]
        def not_banned(s):
            return not (lexer.singularize(s.lower()) in pattern_banned)
        def p(tok):
            return tok.type == 'WORD' and not_banned(tok.value)
        return next_word().if_test(p)
    
    def next_any_unbanned_extended():
        """parser for 'word (or word) (paren stuff)'.
        (or word) gives a synonym as a parenthetical within
        a word pattern.  Side effect is a new global synonym."""
        p = ((Pattern.next_any_unbanned() + 
             c.paren((next_word('or') + Pattern.next_any_unbanned()).treat(lib.snd).plus()).possibly()) +
             c.paren(Pattern.next_any_unbanned().plus()).many())
        def f(item):
            item1 = p.process(item)
            ((a,bs),cs) = item1.acc
            vals = [a.value]+ [i.value for i in bs]
            c.synonym_add(vals)
            return c.update((a,cs),item1)
        return Parse(f)
    
    def var():
        """parser for a variable appearing in a pattern"""
        return var() | c.paren(var() + opt_colon_sort)
    
    words = next_any_unbanned_extended().plus()
    
    def word_pattern():
        """Parser for a word pattern, consisting of variables, 
        words, and pattern parentheticals"""
        return Pattern.words + (Pattern.var() + Pattern.words).many() + Pattern.var().possibly()
        
    type_word_pattern = lit('a').possibly() + word_pattern()
    
    function_word_pattern = next_word('the') + word_pattern()
    
    notion_pattern = var() + next_word('is') + lit('a') + word_pattern()
    
    adjective_pattern = var() + next_word('is') + next_word('called').possibly() + word_pattern()
    
    var_multisubsect_pattern = (
        (var() + next_value(',') + var()) |
        c.paren(var() + next_value(',') + var() + opt_colon_type_meta())
        )
    
    adjective_multisubject_pattern = (
        var_multisubsect_pattern + next_word('are') + next_word('called').possibly() + word_pattern()
        )
        
    verb_pattern = (
        var() + word_pattern()
        )
        
    verb_multisubject_pattern = (
        )
        
               
               




    
    
    

    
    


#def op_colon_type_meta():

            

  
            
            