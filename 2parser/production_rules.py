#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 22 17:00:38 2020

@author: thales
"""

"""production rules for Colada"""

import copy
import msg
import word_lists
import lib
import lexer
import parser_combinator as c
from parser_combinator import (Parse, ParseError, 
                               first_word, 
                               first_phrase, next_word,
                               next_phrase, next_value)

# globals
# parse_level determines how complete the parsing is.
# not in use
# 
production_dict = {
    'level_enum' : {'reparse','reparse','minimal'},
    'parse_level' : 'full'
    }

reparser = {
    'type' : (lambda item : item),
    'expr' : (lambda item : item)
    }

def reparse(s:str) -> Parse:
    try: 
        return reparser[s]
    except KeyError as k:
        msg.error(f'Bad reparse key: {k}')
        raise k
        

def cs_brace(cs_parse:Parse,brace_parse:Parse) -> Parse:
    """control sequence parser including arguments in braces.
    cs_parse is used to parse cs and brace_parse to parse each braced arg."""
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

def expr():
    """parse for expression (term, type, or prop)."""
    def p(tok):
        # commas can appear in quantified variables
        return not(tok.value in [';','.'])
    return reparse('expr').process(c.balanced_condition(p))

def assign_expr():
    """parser for := followed by an expression
    The output is the expression
    """
    return (next_value(':=') + expr()).treat(lib.snd)

def var():
    """parser for a single variable.
    Accepts a single token that is a variable."""
    return Parse.next_token().if_type(['VAR']).expect('var')

def var_or_atomic():
    """parser for a var or atomic identifier.
    Output of parser is a single token of one of those types."""
    return (var() | atomic()).expect('var_or_atomic')

def var_or_atomics():
    """parser for a sequence of one or more var or atomics"""
    return Parse.plus(var_or_atomic())

def var_or_atomic_or_blank():
    """parser for var or atomic or _.
    The parser output is a single token that is one of those types."""
    return var_or_atomic() | next_value('_')

def hierarchical_identifier():
    """parser for hierarchical identifiers.
    Parser output is a single token."""
    return Parse.next_token().if_type(['HIERARCHICAL_IDENTIFIER'])

def identifier():
    """parser for hierarchical or atomic identifier.
    Parser output is a single token"""
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
    if s in ['record','doc','location']:
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
comma = next_value(',')
semicolon = next_value(';')
colon = next_value(':')

#renamed map -> call

# instructions do nothing except store for now

instruct = {}

class Instruction:

    def _param_value(ls):
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
      
    def _expand_slashdash(vs):
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
    
    def _syn():
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
                v_expand = Instruction._expand_slashdash(vs)
                c.synonym_add(v_expand)
                return ()
        def treat_instruct(acc):
            keyword,ls = acc
            instruct[keyword.value] = Instruction._param_value(ls)
            return ()
        keyword_instruct = (first_word("""exit timelimit printgoal dump 
                         ontored read library error warning""") + 
                         Parse.next_token().possibly())
        return (c.bracket(next_word('synonym') + Instruction._syn().treat(treat_syn) |
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

def meta_tok():
    tok = c.mk_token({'type':'META','value':str(meta_tok.count)})
    meta_tok.count += 1
    return tok
#    tok = copy.copy(c.init_item.tok)
#    tok.value = str(meta_tok.count)
#    tok.type = 'META'
#    meta_tok.count += 1
#    return tok 

def colon_annotation(prs):  #was opt_colon_type, opt_colon_sort
    """Parser for ': A', discarding the colon. 
    A is parsed by prs.
    Parser returns an empty list if there is no annotation."""
    #def trt1(toks):
    #    if len(toks)==0:
    #        return toks
    #    return prs.process(toks)
    prs1= (next_value(':') + post_colon_balanced()).treat(lib.snd).possibly().treat(lib.fflatten)
    return prs1.reparse(prs)

def colon_annotation_or_meta(prs): #was opt_colon_type_meta, opt_colon_sort_meta
    """Parser for annotation ': A', discarding the colon.
    If no annotation, parser returns a meta-variable.
    A is parsed using prs."""
    def trt(acc):
        if acc == [] or acc == None:
            return meta_tok()
        return acc
    return colon_annotation(prs).treat(trt)

# differ only in treatment
#def opt_colon_sort():
#    return opt_colon_type()

#def opt_colon_sort_meta():
#    return opt_colon_type_meta()

def annotated_var(prs):
    """
    Parser for annotated variable in parentheses.  
    Annotation is parsed with prs.
    Parser output is a var token 
    annotation is stored in attribute 'annotation' of var token.
    
    Sample input to parser:
        (x : A)
    """
    def trt(acc):
        v,ann = acc 
        if len(ann) > 0:
            return c.copy_token(v,{'annotation':ann[0]})
        return v
    return c.paren(var() + colon_annotation(prs)).treat(trt)

#def annotated_sort_vars():
#    return c.paren(var().plus() + opt_colon_type_meta())

def annotated_vars(prs):
    """Parser for list of annotated variables, parsing annotation with prs
    
    Missing annotation added as a metavariable.
    Variables within the parentheses are constrained to have the same type.
    
    Sample input:
        (x y z : A)
        (u v)
        
    Output is a list of annotated vars using attribute 'annotation'
    """
    def trt(acc):
        vs,ann = acc
        if len(ann)==0:
            return vs
        assert(len(ann)==1)
        return [c.copy_token(v,ann[0]) for v in vs]
    return c.paren(var().plus() + colon_annotation_or_meta()).treat(trt)

def let_annotation_prefix():
    return (next_word('let') + c.comma_nonempty_list(var()) +
     next_word('be') + lit('a').possibly() +
     next_word('fixed').possibly())
    
def let_annotation():
    """Parser for let_annotations. Terminating punctuation not included.
    
    Sample parser inputs:
        Let G be a group
        Let G be a fixed group
        Let (H G : group)
        Fix (x : R)
        
    Issues: No treatment for now, but return to this later.   
    """
    return ((first_word( 'fix let') + c.comma_nonempty_list(annotated_sort_vars)) |
     let_annotation_prefix() + post_colon_balanced())

def brace_assign():
    def brace_assign_item():
        return (var_or_atomic_or_blank()+ opt_colon_sort() + assign_expr().possibly())
    return c.brace_semi().reparse_list(brace_assign_item)

def brace_noassign():
    def brace_noassign_item():
        return (var_or_atomics() + opt_colon_sort_meta())
    return c.brace_semi().reparse_list(brace_noassign_item())

def nonkey(): #was not_banned
    keyword = [
        'is','be','are','denote','define','enter','namespace','stand',
        'if','iff','inferring','the','a','an','we','say','write',
        'assume','suppose','let','said','defined','or','fix','fixed'
        ]
    def p(token):
        return not(c.singularize(token.value) in keyword)
    return c.next_type(['VAR','WORD','ATOMIC_IDENTIFIER']).if_test(p)

def args_template():
    """Form of arguments to a function declaration"""
    def required_arg_template_pat():
        return ( 
            (c.paren(var_or_atomics() + opt_colon_sort_meta())) |
            var_or_atomic()
            )
    return (brace_noassign().possibly() + required_arg_template_pat().many())

def any_controlseq(): #was controlseq
    return c.next_type(['CONTROLSEQ'])

def controlseq(s): #was the_controlseq
    """Parser for a particular control sequence 's'.
    s includes the backslash."""
    return any_controlseq().if_value(s)

def any_symbol(): # was symbol
    return c.next_type(['SYMBOL'])

def symbol(s): # was the_symbol
    return any_symbol().if_value(s)



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

    def then_prefix():
        return lit('then').possibly()
    
    def assumption():
        assumption_prefix = lit('lets')+ lit('assume') + next_word('that').possibly()
        return ((assumption_prefix + c.balanced() + period) |
                let_annotation() + period)
    
    def possibly_assumption():
        return (Proof.assumption().many() + Proof.then_prefix())
    
    def axiom_preamble():
        return lit('axiom')+ atomic().possibly() + period
    
    def moreover_statement():
        return next_word('moreover') + c.balanced() + period
    
    def axiom():
        return Proof.axiom_preamble() + Proof.possibly_assumption() + c.balanced() + period + Proof.moreover_statement.many()
    
    def ref_item():
        return c.andcomma_nonempty_list(lit('location').possibly() + atomic())
    
    def by_ref():
        return c.paren(next_word('by') + Proof.ref_item()).possibly()
    
    def by_method():
        def no_that(tok):
            return tok.value == 'that' # exclude avoids goal_prefix ambig.
        return (next_word('by') + 
                 (first_phrase(['contradiction','case analysis']) |
                  (next_word('induction') + 
                   (next_word('on') + c.balanced_condition(no_that)).possibly())) +
                 Parse.probe(next_word('that')| period))
    
    def choose_prefix():
        return Proof.then_prefix() + lit('lets').possibly() + lit('choose')
    
    def canned_prefix():
        return c.andcomma_nonempty_list(phrase_list_transition())
    
    def goal_prefix():
        return ((lit('lets').possibly() + lit('prove') + next_word('that')) |
                   (Proof.by_method() + next_word('that')).possibly())
    
    def preamble():
        return ((next_word('proof') + Proof.by_method().possibly() + period) |
                      next_word('indeed'))
    
    def affirm():
        return Proof.statement() | Proof.goal()
    
    def statement():
        return Proof.then_prefix() + c.balanced() + Proof.by_ref() + period + Proof.moreover_statement.many() + Proof.script.possibly()
    
    def goal():
        return Proof.goal_prefix + c.balanced() + Proof.by_ref() + period + Proof.script()

    def script():
        return (Proof.preamble + (Proof.canned_prefix() + Proof.body() + Proof.canned_prefix + Proof.tail).many() + lit('qed') + period)
    
    def body():
        return (Proof.tail() |  Proof.assumption())
    
    def tail():
        return (Proof.affirm() | Proof.canned() | Proof.case() | Proof.choose())
    
    def case():
        return (next_word('case') + c.balanced() + period + Proof.choose_justify())
    
    def choose():
        return (Proof.choose_prefix + c.balanced() + period + Proof.justify())
    
    def justify():
        return (Proof.script().possibly())


# patterns 

pattern_key = ["is","be","are","denote","define"
   "enter","namespace",
   "stand","if","iff","inferring","the","a","an",
   "we","say","write",
   "assume","suppose","let",
   "said","defined","or","fix","fixed" # and (need in 'resultant of f and g')
  ]

class Pattern:
    """Parser generators for patterns"""
    
    def _nonkey():
        """Parser for any word except for keywords.  
        The token must be a WORD."""
        def not_key(s):
            return not (lexer.singularize(s.lower()) in pattern_key)
        def p(tok):
            return tok.type == 'WORD' and not_key(tok.value)
        return next_word().if_test(p)
    
    def _nonkey_extended():
        """parser for 'word (or word) (paren stuff)'.
        (or word) gives a synonym as a parenthetical within
        a word pattern.  Side effect is a new global synonym."""
        p = ((Pattern._nonkey() + 
             c.paren((next_word('or') + Pattern._nonkey()).treat(lib.snd).plus()).possibly()) +
             c.paren(Pattern._nonkey().plus()).many())
        def f(item):
            item1 = p.process(item)
            ((a,bs),cs) = item1.acc
            vals = [a.value]+ [i.value for i in bs]
            c.synonym_add(vals)
            return c.update((a,cs),item1)
        return Parse(f)
    
    def _var():
        """parser for a variable appearing in a pattern"""
        return var() | c.paren(var() + opt_colon_sort)
    
    def _nonkey_words():
        return Pattern._nonkey_extended().plus()
    
    def word_pattern():
        """Parser for a word pattern, consisting of variables, 
        words, and pattern parentheticals"""
        return Pattern._nonkey_words() + (Pattern._var() + Pattern._unkey_words()).many() + Pattern._var().possibly()
        
    def type_word_pattern():
        return lit('a').possibly() + Pattern.word_pattern()
    
    def function_word_pattern():
        return next_word('the') + Pattern.word_pattern()
    
    def notion_pattern(): 
        return Pattern._var() + next_word('is') + lit('a') + Pattern.word_pattern()
    
    def adjective_pattern():
        return Pattern._var() + next_word('is') + next_word('called').possibly() + Pattern.word_pattern()
    
    def var_multisubsect_pattern():
        return (
        (Pattern._var() + next_value(',') + Pattern._var()) |
        c.paren(Pattern._var() + next_value(',') + Pattern._var() + opt_colon_type_meta())
        )
    
    def adjective_multisubject_pattern():
        return (
        Pattern.var_multisubsect_pattern() + next_word('are') + next_word('called').possibly() + Pattern.word_pattern()
        )
        
    def verb_pattern(): 
        return  Pattern._var() + Pattern.word_pattern()
        
    def verb_multisubject_pattern():
        return (Pattern.var_multisubsect_pattern() + Pattern.word_pattern())
        
    def predicate_word_pattern():
        return (
            Pattern.notion_pattern() |
            Pattern.adjective_pattern() |
            Pattern.adjective_multisubject_pattern() |
            Pattern.verb_pattern() |
            Pattern.verb_multisubject_pattern()
            )
    
    def controlseq_pattern():
        return any_controlseq() + c.brace(Pattern._var()).many()
    
    def binary_controlseq_pattern():
        return Pattern._var() + Pattern.controlseq_pattern() + Pattern._var()
    
  
    def precedence_level(): #was paren_precedence_level
        """parser for the precedence level.
        
        sample input:
            'with precedence 10 and left associativity'
        output: (integer,assoc), where assoc in ['left','right','no']."""

        def _precedence_level():
            def f(raw):
                ((_,i),pos) = raw
                if len(pos)== 0:
                    l = 'no'
                else:
                    l = pos[0][0][1]
                return (int(i),l)
            return (
                (next_phrase('with precedence') + c.next_type('INTEGER')) +
                (next_word('and') + lit('assoc') + next_word('associtivity')).possibly()
                ).treat(f)

        return Pattern._precedence_level() | c.paren(Pattern._precedence_level())


    def symbol_pattern():
        return (
            Pattern._var().possibly() + symbol() +
            (Pattern._var() + symbol()).many() + 
            Pattern._var().possibly() + Pattern.precedence_level().possibly()
            )
    
    def binary_symbol_pattern():
        """Parser for binary symbol pattern.
        
        Sample inputs:
            x ^^ y with precedence 10 and right associativity
            x ## y"""
        return (
            Pattern._var() + symbol() + Pattern._var() +
            Pattern.precedence_level().possibly()
            )
    

class Macro:
    
    def insection():
        """Parser for in-section scoping.
        
        Sample inputs:
            In this section,
            In this document 
        Output:
            a single token whose value is the location keyword.
        """
        prs = (next_phrase('in this') + lit('document')) + comma.possibly()
        def tr(acc):
            return acc[0][1]
        return prs.treat(tr)
        
    def we_record_def():
        """Parser for registered facts.
        
        XX not finished. We need to reparse the balanced tokens
        """
        def p(tok):
            return (tok.value not in [',','.',';'])
        return lit('we-record') + c.balanced_condition(p)
            
    def copula():
        """Parser for copula in macro declarations.
        """
        return (
            (lit('is') + lit('defined-as').possibly()) |
            (next_value(':=')) |
            (lit('denote'))
            )
               
    def function_copula():
        return (
            Macro.copula() |
            opt_colon_type() + next_value(':=')
            )

    def iff_junction():
        return lit('iff')
    
    def opt_say():
        return lit('we-say').possibly()
    
    def opt_record():
        return lit('we-record').possibly()
    
    def opt_define():
        return (
            (lit('lets') + next_word('define').possibly()) |
            Macro.opt_record()
            )
    
    def macro_inferring():
        return c.paren(next_word_inferring() + var().plus() + opt_colon_sort_meta())
    
    def classifier_word_pattern():  # was classifier_words
        return c.comma_nonempty_list(c.next_any_word_except(['is','are','be']).plus())
    
    def classifier_def():
        return (
            next_word('let') + Pattern.classifier_word_pattern() +
            lit('is') + lit('a').possibly() + lit('classifier')
            )
    
    def symbol_type_pattern():
        return Pattern.symbol_pattern()
    
    def identifier_pattern():
        ##XX
        return (lit('a') + identifier().if_test(p)) | next_value('_')
    
    #def 



    
    
    

    
    


#def op_colon_type_meta():

            

  
            
            