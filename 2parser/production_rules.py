#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 22 17:00:38 2020

@author: thales


production rules for Colada

Output of parsers will generally be an Etok.


Parser rules ending in _ produce a list of Etoks rather than one.

Inner functions f(acc) are treatments.
Inner functions f(item)->item are item transformers.

The parsers are generally implemented as function calls
rather than values.  This gives uniform style and helps
to prevent infinite recursive expansion of parsers.
"""

#import copy
#import msg
from exception import ParseError, ParseNoCatch, DataProcess, ErrorItem
import copy, lib, msg, word_lists
import tokenlib
import inner_lexer
import lexer
from ply.lex import LexToken #import ply.lex.LexToken as LexToken


import parser_combinator as c

from parser_combinator import (Parse, LazyParse,
                               first_word, 
                               first_phrase, next_word, next_any_word,
                               next_phrase, next_value,
                               mk_stream, pstream)

class Etok:
    """
    This is the class that represents the nodes 
    in the abstract syntax tree.  I'm not using subclasses for now.
    So the representation is rather basic.  Subclasses can be simulated
    by the name of the Etok.
    
    name: convention- uppercase if the 'type' of a terminal token
        This means that the rule acts as the value
          lowercase if the name of the nonterminal.
        
    rule = '': optional production_rule string
    raw = [LexToken]: flat token list from the original text, for error messages
    misc = None: optional additional data, not belong anywhere else.
        For ease of traversal,
        misc should not contain any Etoks at any level of nesting.
    altrepr = '': optional string reprsentation of Etok, can be value of a Tok
    etoks: (nested) Etok list, children in AST
    
    We generally expect a production rule to output a single Etok.
    
    We should try to avoid having to extract essential data from raw.
    Otherwise, there should be no redundant information in Etok.
    
    >>> try:
    ...     Etok(['hi'],[],[])
    ... except TypeError as e:
    ...     print(e) 
    Etok string expected in name:['hi']
    """
    def __init__(self,name,etoks,raw,rule='',misc=None,altrepr='',):
        self.name = name
        self.etoks = etoks
        if isinstance(etoks,Etok):
            self.etoks = [etoks]
        self.raw = Etok.get_raw(raw)
        self.rule = rule
        self.altrepr = altrepr
        self.misc = misc
        self.validate()
        
    def __repr__(self):
        if self.altrepr:
            return f"Etok({self.altrepr})"
        name2 = self.name
        if self.rule:
            name2 += ','+self.rule
        return f"Etok({name2},'{self.rawstring()}')"
    
    def s_expression(self):
        def s2(es):
            if not(es):
                return ''
            if isinstance(es,Etok):
                return es.s_expression()
            if lib.iterable(es):
                return '['+' '.join([s2(e) for e in es if e ])+']'
        name2 = self.name 
        if self.rule:
            name2 += '-'+self.rule 
        re = ''
        if lib.fflatten(self.etoks):
            re = s2(self.etoks)
        #' '.join([e.s_expression() for e in lib.fflatten(self.etoks) if e])
        if re:
            re = ' '+re
        return (f"({name2}"+re+')')
    
    def rawstring(self):
        return ' '.join([tok.value for tok in self.raw])
    
    def validate(self):
        if not(isinstance(self.name,str)):
            raise TypeError('Etok string expected in name:'+str(self.name))
        if not(isinstance(self.altrepr,str)):
            raise TypeError('Etok string expected in altrepr:'+str(self.altrepr))
        if not(isinstance(self.rule,str)):
            raise TypeError('Etok string expected in rule:'+str(self.rule))
        for e in self.raw:
            if not(isinstance(e,LexToken)):
                raise TypeError(f'raw must be list of Tok in {self.name}, not {str(e)}')
        for e in lib.fflatten(self.etoks):
            if e and not(isinstance(e,Etok)): 
                raise TypeError(f'etoks must be a list of Etok in {self.name}, not {e}:{type(e).__name__}')
        pass
    
    @property
    def type(self):
        """Augment Etok with fields of token"""
        return self.name 
    
    @property 
    def value(self):
        return self.rule 
    
    @property 
    def lineno(self):
        return self.raw[0].lineno 
    
    @property 
    def lexpos(self):
        return self.raw[0].lexpos
        
    def etok(tok):
        """convert a token to Etok"""
        return Etok(tok.type,[],[tok],rule=tok.value)
    
    def update(self,d:dict ={}):
        """Use a dictionary d to update self.
        If the dictionary is empty, a copy of self is made.
        """
        e1 = Etok(name=self.name,etoks=self.etoks,raw=self.raw,rule=self.rule,misc=self.misc,altrepr=self.altrepr)
        for key,value in d.items():
            if key=='raw':
                value = Etok.get_raw(value)
            setattr(e1,key,value)
        e1.validate()
        return e1
    
    def _get_raw1(tok):
        if (isinstance(tok,Etok)):
            return tok.raw
        if isinstance(tok,LexToken):
            return [tok]
        return []
    
    def get_raw(etoks):

        """extract the raw fields from a 
        nested list of Etoks and LexTokens"""
        return lib.flatten([Etok._get_raw1(e) for e in lib.fflatten(etoks)])

    def parse(p : Parse):
        """Promote a LexToken parser to a Etok parser"""
        def f(acc):
            return Etok.etok(acc)
        return p.treat(f)

def strip_delim(acc):
    """treatment to remove outer delimiters
    We assume inner material is an Etok.
    """
    (_,b,_) = acc
    return b.update({'raw':acc}) 



lookup_parse = {}  # dictionary of parses, used for forward refs
    
def add_lookup_parse(nonterminal,value):
    """Add a new production rule to the lookup_parse dictionary.
    The key is the nonterminal string.
    The value is the parser associated with it.
    
    value.nonterminal should
    """
    v = value.name(nonterminal)
    if nonterminal in lookup_parse:
        lookup_parse[nonterminal].append(v)
    else:
        lookup_parse[nonterminal]= [v]
        
get_lookup_parse_history = {}
#get_lookup_parse_history is here to keep tract of 
#    parsers that have been called but not implemented.
#    Keys are all none. It functions as a set.

        
def get_lookup_parse(nonterminal):
    """The grammar is highly mutually recursive.
    To make the implementation simpler, some of the recursion
    has been relegated to a dictionary of parses: lookup_parse,
    with keys given as nonterminal strings.
    
    The parsers should return an Etok.
    
    As new production rules are implemented, they are
    added to the dictionary. The key is the nonterminal.
    
    This function looks up the production rules 
    for a given key and forms them
    into a parser according to the first successful rule.
    
    There is a last resort production rule of letting each nonterminal
    string represent a parser that parses the nonterminal as a literal
    word string.  This last resort rule might give strange behavior but it
    is probably quite harmless.  The last resort helps with debugging.
    
    >>> pstream(get_lookup_parse('hello'),'hello and')
    Etok(hello,default,'hello')
    """
    def f(acc):
        return Etok.etok(acc).update({'name':nonterminal,'rule':'default'})
    ps = lookup_parse.get(nonterminal,[])
    ps.append(Etok.parse(Parse.next_token().if_value(nonterminal)).treat(f))
    get_lookup_parse_history['nonterminal']=None
    return Parse.first(ps)

    
def build_word_net(phrase_list):
    """build a word net (discrimination net) from a list of phrases.
    No normalization of words is performed, except case.
    
    >>> build_word_net(['this and','this or','that and','this or that'])
    {'this': {'and': {'': {}}, 'or': {'': {}, 'that': {'': {}}}}, 'that': {'and': {'': {}}}}
    """
    def to_dict(ls):
        if not(ls):
            return {}
        return {ls[0] : to_dict(ls[1:])}
    def one_dict(phrase):
        od = to_dict(phrase.lower().split()+[''])
        return od
    def add_dict(d1,od): 
        if not(od):
            return d1
        key = [*od][0] #first key
        if key in d1:
            # N.B. avoid equality to preserve scoping
            d1.__setitem__(key,add_dict(d1[key],od[key]))
        else:
            d1.setdefault(key,od[key])
        return d1
    acc = {}
    for phrase in phrase_list:
        acc = add_dict(acc,one_dict(phrase))
    return acc

def next_word_net(wn):
    """construct a parser for a word net. 
    Take the longest match.
    
    >>> pstream(next_word_net(build_word_net(['aa bb cc','bb cc','aa bb cc dd'])),'aa bb cc dd ee')
    [LexToken(WORD,'aa',1,0), LexToken(WORD,'bb',1,3), LexToken(WORD,'cc',1,6), LexToken(WORD,'dd',1,9)]
    """
    def f(item):
        try:
            item1 = next_any_word().process(item)
            if not(item1.acc.value in wn):
                raise ParseError(ErrorItem(item,'next_word_net',''))
        except (StopIteration, ParseError) as pe:
            if '' in wn:
                return tokenlib.update([],item)
            raise pe
            
        acc1 = item1.acc
        wn1 = wn[acc1.value]
        item2 = next_word_net(wn1).process(tokenlib.update(None,item1))
        return tokenlib.update([acc1]+item2.acc,item2)
    
    return Parse(f,'next_word_net')
        
#print(build_word_net(word_lists.transition))


def phrase_list_transition():
    """parser for transition phrases
    
    >>> pstream(phrase_list_transition(),'therefore')
    []
    """
    return (next_word_net(build_word_net(word_lists.transition)) + next_word('that').possibly()).nil()

def phrase_list_filler():
    """parser for filler words.
    
    Examples:
    'We know that'
    'We see'
    'See that'
    """
    return (Parse.word('we').possibly() + first_word('put write have know see') + 
            Parse.word('that').possibly()).nil()

lit_dict = {
    'a' : first_word('a an'), #indefinite
    'article' : first_word('a an the'),
    'defined_as' : first_phrase(['said to be','defined as','defined to be']),
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
    'qed': first_word('end qed obvious literal'),
    'def': first_word('def definition'),
    'axiom': first_word('axiom conjecture hypothesis equation formula'),
    'with_property': next_phrase('with property'),
    'param': next_phrase('with parameter'),
    'theorem': first_word('proposition theorem lemma corollary'),
    # type proposition property classsifier atomic 
    }

def lit(s):
    """parser generator for 's'-like words or phrases
    
    canned phrases that have small variants
    lit[w] gives parser for w-like words or phrases
    
    Output Etok(name='lit', rule=s, value=None)
    
    >>> pstream(lit('qed'),'obvious')
    Etok(LIT,qed,'obvious')
    """
    def f(acc):
        return Etok('LIT',[],[acc],s)
    if s =='record':
        return (Parse.word('we').possibly() +
                first_word('record register') +
                Parse.word('identification').possibly() +
                Parse.word('that').possibly()).treat(f)
    else:
        return lit_dict[s].treat(f)

def lit_read(s):
    """parser generator for s-like words or phrases.
    
    Output is an etok with name = s, rule = response, 
    
    >>> pstream(lit_read('assoc'),'right')
    Etok(ASSOC,right,'right')
    """
    def f(acc):
        return Etok(s.upper(),[],[acc],acc.value)
    local_lit_dict = {
            'any':(first_phrase(['each and every','some and every']) | first_word('every each all any some no')),
            'sort': (next_any_word().if_rawvalue('Type') | next_any_word().if_rawvalue('Sort')),
            'assoc': first_word('left right no'),
            'field_key': first_word('coercion notationless notation parameter type call'), #renamed map -> call
            'document': first_word('document article section subsection subsubsection subdivision division'),
            'end_document': first_word('endsection endsubsection endsubsubsection enddivision endsubdivision')
        }
    if s == 'doc':
        return (local_lit_dict['document'] | local_lit_dict['end_document']).treat(f)
    if s == 'location':
        return Parse.first([local_lit_dict['document'],lit_dict['theorem'],lit_dict['axiom']]).treat(f)
    return local_lit_dict[s].treat(f)

#others:
#label = atomic

period = next_value('.')
comma = next_value(',')
semicolon = next_value(';')
colon = next_value(':')

def option_paren(pr):
    """
    Parse an expression optionally in parentheses, 
    discarding parentheses.  
    
    Input pr : A parser.
    Output: A parser.
    
    >>> pstream(option_paren(Parse.next_token()),'(hello)')
    LexToken(WORD,'hello',1,1)
    """
    def f(acc):
        return acc[1]   # (left,acc[1],right)
    return (c.paren(pr).treat(f) | pr )



def cs_brace(cs_parse):
    """control sequence parser including arguments in braces.
    Etok cs_parse is used to parse cs and 
    Etok expr to parse each braced arg.
    
    Output: Etok(name='cs_brace') etoks=(cs_brace,braces).
    
    
    >>> pstream(cs_brace(Etok.parse(next_any_word())),'cs {term} {term} c')
    Etok(cs_brace,word,'cs { term } { term }')
    """
    n_acc = None
    def f(acc):
        nonlocal n_acc
        n_acc = acc
        return Etok(name='cs_brace',etoks=acc,raw=[acc],rule=cs_parse.nonterminal)      
    return (cs_parse + c.brace(expr()).treat(strip_delim).many()).treat(f)

# case_sensitive_word -> use next_value(s)



def atomic():
    """parser for atomic identifiers, 
    converting words and integers as needed
    
    Atomic identifiers cannot be a single letter (a short var)
    wordlike atomic identifiers are modulo case-sing-syn.
    but hierarchical identifiers are always case sensitive.
    
    Integers are included for section numbers in labels.
    
    output Etok
    
    >>> pstream(atomic(),'HELLO')
    Etok(ATOMIC,HELLO,'HELLO')
    
    >>> pstream(atomic(),'the')
    Etok(ATOMIC,the,'the')
    """
    def f(acc):
        if acc.type == 'WORD':
            rule = c.synonymize(acc.value)
        else:
            rule = acc.value
        return Etok(name='ATOMIC',etoks=[],raw=[acc],rule=rule)
    return Parse.next_token().if_type(['INTEGER','WORD','ATOMIC_IDENTIFIER']).name('atomic').treat(f)


def label():
    return atomic()

def primitive(primitive_nonterminal):
    def f(item):
        if not(primitive_nonterminal in word_lists.prim_list):
            raise(ParseNoCatch(ErrorItem(item,primitive_nonterminal,'undeclared primitive')))
        return get_lookup_parse(primitive_nonterminal).process(item)

    return Parse(f,primitive_nonterminal,'!')

def _add_prim1():
    def equal():
        return next_value('=')
    add_lookup_parse('prim_binary_relation_op',equal())
    
    def bool():
        return (Parse.next_token().if_rawvalue('True') | Parse.next_token().if_rawvalue('False'))
    add_lookup_parse('prim_relation', bool().name('prim_relation','True-False'))
    pass

_add_prim1()

def section_preamble():
    """Section label.
    
    Output Etok.etoks = [section,label?]
    
    >>> pstream(section_preamble(),'Section 3.')
    Etok(section_preamble,'section 3 .')
    """
    def f(acc):
        (e,_) = acc
        return Etok(name='section_preamble',etoks=e,raw=acc)
    def section_tag():
        return (lit_read('doc'))
    return (section_tag() + label().possibly() + period).name('section_preamble').treat(f)

class Instruction:
    """Construct a parser that creates an Etok for a given instruction.
    
    There are misc data types: synonym, string, bool, int.
    dictionary keys:
        
        name : 'instruction'
        rule : instruction keyword
        misc : None, synlist, str, bool, int depending on the type
        
        production : 'instruction'
        rawvalue : input tokens,
        keyword : string indicating instruction,
        value : None, synlist, str, bool, int depending on the type.
    """

    def _param_misc(tok):
        if not(tok):
            return None
        if tok.type == 'INTEGER':
            return int(tok.value)
        if tok.value.lower() in ['yes','true','on']:
            return True
        if tok.value.lower() in ['no','false','off']:
            return False
        return tok.value
      
    def _expand_slashdash(vs):
        """expanding synonyms
        e.g. word/-ing is short for word/wording
        
        >>> Instruction._expand_slashdash('work /- ing effort / workaround'.split())
        ['work', 'working', 'effort', 'workaround']
        """
        for i in range(len(vs)-1):
            if vs[i]== '/-':
                vs[i]= '/'
                vs[i+1]= vs[i-1]+vs[i+1]
        return [v for v in vs if v != '/']
    
    def _syn():
        """parsing synonyms
        
        >>> pstream(Instruction._syn(),'aa/bb,cc/-dd,ee/ff')
        [[LexToken(WORD,'aa',1,0), LexToken(SYMBOL,'/',1,2), LexToken(WORD,'bb',1,3)], ...
        """
        def f(acc):
            return acc[0::2]
        def p(tok):
            return tok.value in ['/','/-'] or c.can_wordify(tok)
        synlist = Parse.next_token().if_test(p).plus()
        return c.comma_nonempty_list(synlist).treat(f)
    
    def _treat_syn(acc):
        """build dict for synonyms
        """
        acc11 = acc[1][1]
        tt=[Instruction._expand_slashdash([t.value for t in ac]) for ac in acc11]
        return Etok(name='instruction',etoks=[],raw=acc,rule='synonym',misc=tt)
        #d = {'production':'instruction',
        #     'raw':lib.flatten(acc),
        #     'keyword':'synonym'
        #     }
        #acc11 = acc[1][1]
        #d['value']=[Instruction._expand_slashdash([t.value for t in ac]) for ac in acc11]
        #return d
     
    def _treat_instruct(acc):
        (_,(keyword,ls),_) = acc
        return Etok(name='instruction',etoks=[],raw=acc,rule=keyword.value,misc=Instruction._param_misc(ls))
    
    #{'production':'instruction',
    #            'raw':lib.flatten(acc),
    #            'keyword':keyword.value,
    #            'value':Instruction._param_misc(ls)}
    
    _keywords = """exit timelimit printgoal dump 
                         ontored read library error warning"""
                         
    _keyword_instruct = (first_word(_keywords) + 
                         Parse.next_token().possibly())
    
    def instruction():
        """parsing instructions
        
        >>> pstream(Instruction.instruction(),'[exit 1]')
        Etok(instruction,exit,'[ exit 1 ]')
 
        >>> pstream(Instruction.instruction(),'[read filename]')
        Etok(instruction,read,'[ read filename ]')
       
        >>> pstream(Instruction.instruction(),'[synonym another/extras, yet/-s]')
        Etok(instruction,synonym,'[ synonym another / extra yet /- s ]')
        """
        
        return (c.bracket(next_word('synonym') + Instruction._syn()).treat(Instruction._treat_syn) |
             c.bracket(Instruction._keyword_instruct).treat(Instruction._treat_instruct))


def expr():
    """parse for expression (term, type, or prop).
    
    Output Etok(expr,...)
    
    >>> pstream(expr(),'term')
    Etok(expr,term,'term')
    """
    def f1(nonterminal):  #currying
        def f(acc):
            return Etok('expr',etoks=acc.etoks,raw=acc.raw,rule=nonterminal,misc=acc.misc,altrepr=acc.altrepr)
        return f
    def get(nonterminal):
        return get_lookup_parse(nonterminal).treat(f1(nonterminal))
    return (get('general_type') |
            get('term') |
            get('prop') |
            get('proof_expr') |
            get('sort_expr'))
  
def colon_sort():
    def f(acc):
        (_,e) = acc
        return e.update({'raw':acc})    
    return (colon + get_lookup_parse('sort_expr')).treat(f)

def opt_colon_sort():
    return colon_sort().possibly()

def colon_type():
    """Parse a colon then a post_colon_type.
    
    output Etok
    
    >>> pstream(colon_type(),':post_colon_type')
    Etok(post_colon_type,default,': post_colon_type')
    """
    def f(acc):
        (_,e) = acc
        return Etok.update(e,{'raw':acc})
    return (colon + get_lookup_parse('post_colon_type')).treat(f)

def opt_colon_type():
    return colon_type().possibly()

def var():
    """parser for a single variable.
    Accepts a single token that is a variable.
    
    >>> pstream(var(),'x')
    Etok(VAR,x,'x')
    """
    return c.next_type('VAR').name('VAR').treat(Etok.etok)

def annotated(p):
    """
    Parser for annotated p in parentheses.
    p must output an Etok.
    
    Input is wrapped in parentheses.
    
    Annotation is colon_type or None
    
    Parser output Etok('annotated'...)
    etoks:(p,colon_type)  
    
    Sample input to parser:
        (x : A)
        
    >>> pstream(annotated(var()),'(x:post_colon_type)')
    Etok(annotated,VAR,'( x : post_colon_type )')   
    """
    def f(acc):
        (_,(v,ann),_)  = acc
        if not ann:
            return v.update({'raw':acc})
        return Etok('annotated',etoks=(v,ann),raw=acc,rule=p.nonterminal)
    return c.paren(p + opt_colon_type()).treat(f)

def annotated_var():
    return annotated(var())

def annotateds(p):
    """
    Parser for annotated list
    p must output a list of Etoks.
    
    Input is wrapped in parentheses.
    
    Output Etok.etoks:([p],post_colon_type or None)
    
    Sample input:
        (x y z : A)
        (u v)

    >>> pstream(annotateds(var().plus()),'(x y:post_colon_type)')
    Etok(annotateds,VAR+,'( x y : post_colon_type )')
    """
    def f(acc):
        (_,(vs,ann),_) = acc
        return Etok('annotateds',etoks=(vs,ann),raw=acc,rule=p.nonterminal)
    return c.paren(p + opt_colon_type()).treat(f)

def annotated_vars():
    return annotated(var().plus())

def tvar():
    """
    >>> pstream(tvar(),'x')
    Etok(VAR,x,'x')
    
    >>> pstream(tvar(),'(x : post_colon_type)')
    Etok(annotated,VAR,'( x : post_colon_type )')
    """
    return var() | annotated_var()

def assign_expr():
    """parser for := followed by an expression
    The output is the expression at Etok
    
    >>> pstream(assign_expr(),':= general_type')
    Etok(expr,general_type,':= general_type')
    """
    def f(acc):
        (_,e) = acc
        return e.update({'raw':acc})
    return (next_value(':=') + expr()).name('assign_expr').treat(f)

def var_or_atomic(omit=[]):
    """parser for a var or atomic identifier.
    The value is not allowed to lie in omit.
    Output of parser is a single Etok of one of those types."""
    def p(tok):
        return not(tok.value in omit)
    return (var() | atomic()).if_test(p).name('var_or_atomic')

def var_or_atomics_(omit=[]):
    """parser for a sequence of one or more var or atomics
    
    >>> pstream(var_or_atomics_(),'x uu vv THE RUN.TO')
    [Etok(VAR,x,'x'), ... Etok(ATOMIC,THE,'THE')]
    """
    return var_or_atomic(omit=[]).plus()

def var_or_atomic_or_blank():
    """parser for var or atomic or _.
    The parser output is a single token that is one of those types."""
    return var_or_atomic() | next_value('_').treat(Etok.etok)

def brace_assign():
    """
    input semi-separated list of assignments within braces.

    output is a Etok(brace_assign)
    
    Etok.etoks: list of (lhs,type annotation,assigned expr)
    the last two can be None.
    
    >>> pstream(brace_assign(),'{ x := term ; y : post_colon_type := term }')
    Etok(brace_assign,'{ x := term ; y : post_colon_type := term }')
    """
    def f_item(acc):
        ((v,o),p) = acc
        return (v,o,p)
    n_acc = []
    def f_brace(acc):
        nonlocal n_acc
        (_,b,_) = acc
        n_acc = acc # keep full list of tokens
        return b[0::2]
    def f_final(acc):
        return Etok(name='brace_assign',etoks=acc,raw=n_acc)
    def brace_assign_item():
        return (var_or_atomic_or_blank()+ opt_colon_type() + assign_expr().possibly()).name('brace_assign_item').treat(f_item)
    return c.brace_semif().treat(f_brace).reparse_list(brace_assign_item()).treat(f_final)

def brace_noassign():
    """
    input semi-separated list of var_or_atomics with possible typing
    
    output is an Etok(brace_noassign)
    
    Etok.etoks list of (lhs,typ annotation or None)
    
    >>> pstream(brace_noassign(),'{x:post_colon_type;y}')
    Etok(brace_noassign,'{ x : post_colon_type ; y }')
    """
    n_acc = []
    def f_brace(acc):
        nonlocal n_acc
        (_,b,_) = acc
        n_acc = acc
        return b[0::2] #remove semi
    def f_final(acc):
        return Etok(name='brace_noassign',etoks=acc,raw=n_acc)
    def brace_noassign_item():
        return (var_or_atomics_() + opt_colon_type())
    return c.brace_semif().treat(f_brace).reparse_list(brace_noassign_item()).treat(f_final)

def app_args():
    """ 
    parses the arguments of a function application.
    
    output Etok.toks (brace_assign?,[expr])
    
    >>> pstream(app_args(),'{ x:= term } tightest_expr tightest_expr ...')
    Etok(app_args,'{ x := term } tightest_expr tightest_expr')
    """
    def f(acc):
        return Etok(name='app_args',etoks=acc,raw=acc)
    return (brace_assign().possibly() + get_lookup_parse('tightest_expr').many()).treat(f)

def casemark(s):
    """Used to mark different cases in parsing, for later 
    case-based treatment"""
    def f(acc):
        return (s,acc)
    return f

def annotated_args(omit=[]):
    """
    parse formal parameters of a function 
    
    input variables
    omit = list of banned names for variables and atomics
    output Etok(annotated_args) , etoks = man
    
    
       
    >>> pstream(annotated_args(),'x vv tt')
    Etok(annotated_args,'x vv tt')
    
    >>> pstream(annotated_args(),'x (uu v w : post_colon_type) y')
    Etok(annotated_args,'x ( uu v w : post_colon_type ) y')
    """
    def f(acc):
        return Etok(name='annotated_args',etoks=acc,raw=acc)
    return (var_or_atomic(omit) | annotateds(var_or_atomics_(omit))).many().treat(f)

def args_template(omit=[]):
    """
    input parse braced, annotated arguments of 
    formal function args
    output Etok(args_template), can be devoid of data
    
    >>> pstream(args_template(),'{ x ; y ; z} r (s t) v')
    Etok(args_template,'{ x ; y ; z } r ( s t ) v')
    
    >>> pstream(args_template(),'')
    Etok(args_template,'')
    """
    def f(acc):
        return Etok(name='args_template',etoks=acc,raw=acc)
    return (brace_noassign().possibly() + annotated_args(omit)).treat(f)

def nonempty_args_template(omit=[]):
    """This is the same as args_template, except it must
    contain data.
    
    >>> pstream(nonempty_args_template(),'{ x ; y ; z} r (s t) v')
    Etok(args_template,'{ x ; y ; z } r ( s t ) v')
    
    >>> try:
    ...     pstream(nonempty_args_template(),'')
    ... except:
    ...     'invalid'
    'invalid'
    
    >>> pstream(nonempty_args_template(omit=['y']),'x y')
    Etok(args_template,'x')
    """
    def p(etok):
        return (etok.rawstring())
    return args_template(omit).if_test(p)
        
def tightest_arg():
    """
        
    This allows too much.  We should restrict to admissible patterns.

    >>> pstream(tightest_arg(),'tightest_expr')
    Etok(tightest_expr,default,'tightest_expr')
    
    >>> pstream(tightest_arg(),'(x uu : sort_expr)')
    Etok(tightest_arg,'( x uu : sort_expr )')
    """
    def f(acc):
        (_,(vs,o),_)=acc
        return Etok(name='tightest_arg',etoks=(vs,o),raw=acc)
    return (get_lookup_parse('tightest_expr') | 
        c.paren(var_or_atomic().atleast(2) + 
                (colon_sort() | colon_type()).possibly()).treat(f))

def tightest_args():
    return brace_noassign().possibly() + tightest_arg().many()

def holding_vars():
    """ input
    This is experimental, used to indicate unbound (free) variables in
    a sum or list comprehensive. 
    
    This is inspired by Harrison's {a | b | c} set comprehension notation.
    
    >>> pstream(holding_vars(),', holding x,y,z')
    Etok(holding_vars,', holding x , y , z')
    """
    def f(acc):
        ((_,_),cs) = acc
        return Etok(name='holding_vars',etoks=cs[0::2],raw=acc)
    return (comma + next_word('holding') + c.comma_nonempty_list(var())).treat(f)

def proof_expr():
    r"""parser for the QED symbol
    
    >>> pstream(proof_expr(),r'\qed')
    Etok(SYMBOL_QED,\qed,'\qed')
    """
    return c.next_type('SYMBOL_QED').treat(Etok.etok)

add_lookup_parse('proof_expr',proof_expr())

def tightest_expr():
    """
    Parser for expressions in which the boundaries are clear.
    """
    return (get_lookup_parse('tightest_term') | 
            get_lookup_parse('tightest_prop') | 
            get_lookup_parse('tightest_type') | proof_expr())

def sort_expr():
    """Parser for arrows ending in rawvalue Sort or Type
    
    >>> pstream(sort_expr(),'binder_type -> Type')
    Etok(sort_expr,'binder_type -> type')
    """
    def f(acc):
        (m,s) = acc
        m1 = [a for (a,_) in m]
        return Etok(name='sort_expr',etoks=(m1,s),raw=acc)
    return c.LazyParse((lambda s:((get_lookup_parse(s) + c.next_type('ARROW')).many() + lit_read('sort')).treat(f)),'binder_type')

add_lookup_parse('sort_expr',sort_expr())

# colon_sort above

# opt_colon_sort above

def paren_type():
    """Parser for a type wrapped in parentheses
    
    >>> pstream(paren_type(),'(general_type)')
    Etok(general_type,default,'general_type')
    """
    def f(acc):
        (_,a,_) = acc 
        return a
    return c.paren(get_lookup_parse('general_type')).treat(f)

def annotated_type():
    """Parser for an annotated type
    
    >>> pstream(annotated_type(),'(general_type : Type)')
    Etok(general_type,default,'general_type')
    """
    def f(acc):
        (_,((a,_),_),_)=acc
        return a
    return c.paren(get_lookup_parse('general_type') + colon + Parse.next_token().if_rawvalue('Type')).treat(f)

def controlseq_type():
    """Parser for a control sequence type
    
    >>> pstream(controlseq_type(),'prim_type_controlseq { term }')
    Etok(cs_brace,first,'prim_type_controlseq { term }')
    """
    return cs_brace(get_lookup_parse('prim_type_controlseq'))

def const_type():
    """Parser for an identifier representing a type"""
    return get_lookup_parse('prim_identifier_type')

def field_type():
    """Parser for a field of a structure"""
    def f(acc):
        return Etok('field_type',etoks=acc,raw=acc)
    return (get_lookup_parse('tighteset_term') + get_lookup_parse('prim_field_type_accessor')).treat(f)

def over_args():
    """Parser for the experimental feature of using keyword over to
    unbundle structures
    
    input (there are three different forms, shown in examples)
    
    output Etok(over_args,1 2 or 3)
    
    >>> pstream(over_args(),'over { a := term }')
    Etok(over_args,1,'over { a := term }')
    
    >>> pstream(over_args(),'over tightest_term')
    Etok(over_args,2,'over tightest_term')
    
    >>> pstream(over_args(),'(over tightest_term,tightest_term)')
    Etok(over_args,3,'( over tightest_term , tightest_term )')
    """
    over = next_word('over')

    def over_args1():
        n_acc = []
        def f_brace(acc):
            nonlocal n_acc
            (_,(_,b,_)) = acc
            n_acc = acc
            return b[0::2] #remove semi
        def f1(acc):
            return Etok(name='over_args',etoks=acc[0::2],raw=n_acc,rule='1')
        return ((over + c.brace_semif()).treat(f_brace).reparse_list(var_or_atomic() + assign_expr())).treat(f1)

    def over_args2():
        def f2(acc):
            (_,b)=acc
            return Etok(name='over_args',etoks=b,raw=acc,rule='2')
        return (over + get_lookup_parse('tightest_term')).treat(f2)

    def over_args3():
        def f3(acc):
            (_,(_,b),_)=acc
            return Etok(name='over_args',etoks=b[0::2],raw=acc,rule='3')
        return (c.paren(over + c.comma_nonempty_list(tightest_expr()))).treat(f3)
    return (over_args1() | over_args2() | over_args3())

def overstructure_type():
    """Parser for overstructure.
    The structure name must be a primitive identitifer.
    
    >>> pstream(overstructure_type(),'prim_structure { x:= term } tightest_expr over tightest_term')
    Etok(overstructure_type,'prim_structure { x := term } tightest_expr over tightest_term')
    """
    def f(acc):
        return Etok(name='overstructure_type',etoks=acc,raw=acc)
    return (get_lookup_parse('prim_structure') + app_args() + over_args().possibly()).treat(f)

def var_type():
    """
    Parser for a type variable.
    If not annotated, the var should be 
    previously annotated (v : Type) in the context.
    
    Output: Etok(VAR_TYPE,v,'v') (in LexToken format)
    
    >>> pstream(var_type(),'(v:Type)')
    Etok(VAR_TYPE,v,'( v : type )')
    """
    def f(acc):
        return acc.update({'name':'VAR_TYPE'})
    def f2(acc):
        (_,((v,_),_),_) = acc
        return v.update({'raw':acc})
    return (var() | c.paren(var() + colon + Parse.next_token().if_rawvalue('Type')).treat(f2)).treat(f)

def subtype():
    r""" 
    Parser for a subtype comprehension { x // P(x)}
    
    >>> pstream(subtype(),r'{ term, holding x \tmid statement }')
    Etok(subtype,'{ term , holding x \tmid statement }')
    """
    def f(acc):
        (_,(((t,h),_),s),_)=acc
        return Etok(name='subtype',etoks=(t,h,s),raw=acc)
    return c.brace(get_lookup_parse('term') + holding_vars().possibly() + c.next_type('TMID') + get_lookup_parse('statement')).treat(f)

def app_type():
    """Parser for the application of a type to its arguments 
    
    >>> pstream(app_type(),'tightest_type tightest_expr')
    Etok(app_type,tightest_type,'tightest_type tightest_expr')
    """
    def f(acc):
        return Etok(name='app_type',etoks=acc,raw=acc,rule='tightest_type')
    return ((get_lookup_parse('tightest_type') + app_args()).treat(f) |
            overstructure_type())

binder_comma = comma 

def binder_type():
    """Recursive parser for type binders (Pi-types, etc.)
    
    >>> pstream(binder_type(),'prim_pi_binder tightest_expr, tightest_type')
    Etok(binder_type,'prim_pi_binder tightest_expr , tightest_type')
    """
    def f(acc):
        (((p,a),_),b)=acc
        return Etok(name='binder_type',etoks=(p,a,b),raw=acc)
    return (app_type() | 
            (get_lookup_parse('prim_pi_binder') + tightest_args() + binder_comma + c.lazy_call(binder_type)).treat(f)
            )

def agda_vars():
    """
    Agda style dependent type variables (a : A ) -> B(a)
    >>> pstream(agda_vars(),'(x : post_colon_type) (z u : post_colon_type)')
    Etok(agda_vars,'( x : post_colon_type ) ( z u : post_colon_type )')
    """
    def f(acc):
        return Etok(name='agda_vars',etoks=acc,raw=acc)
    return annotated_vars().plus().treat(f)

def _type_operand():
    """
    Parser for argument of a binary type operation.
    """
    return binder_type() | agda_vars() 

def _type_op():
    """Parser for a binary type operator
    
    >>> pstream(_type_op(),'prim_type_op')
    Etok(prim_type_op,default,'prim_type_op')
    
    >>> pstream(_type_op(),'prim_type_op_controlseq { term }')
    Etok(cs_brace,first,'prim_type_op_controlseq { term }')
    """
    return (get_lookup_parse('prim_type_op') |
            cs_brace(get_lookup_parse('prim_type_op_controlseq')))
    
def binop_type():
    """Parser for binary operation on types.

 
    for product types A * B, sum types A + B, 
    including arrows A -> B,
    including Agda style dependent arrows (x:A) -> B x.
    all type operators are right assoc with the same precedence

    N.B. binder_types is tighter than binop_type, which might be non-intuitive.
    Operators appear in etoks[1] odd positions.
    
    >>> pstream(binop_type(),'tightest_type prim_type_op tightest_type')
    Etok(binop_type,'tightest_type prim_type_op tightest_type')
    """
    def f(acc):
        ((p,m),b) = acc
        return Etok(name='binop_type',etoks=(p,m+[b]),raw=acc)
    return (brace_noassign().possibly() + (_type_operand() + _type_op()).many() + binder_type()).treat(f)

def quotient_type():
    """parser for quotient types
    
    >>> pstream(quotient_type(),'quotient of general_type by term')
    Etok(quotient_type,'quotient of general_type by term')
    """
    def f(acc):
        ((((_,_),g),_),t) = acc
        return Etok(name='quotient_type',etoks=(g,t),raw=acc)
    return (next_word('quotient') + next_word('of').possibly() + 
            get_lookup_parse('general_type') + next_word('by') + 
            get_lookup_parse('term')).treat(f)

def coercion_type():
    r"""parser for coercion of a term to type
    
    >>> pstream(coercion_type(),r'\^term')
    Etok(coercion_type,'\^ term')
    """
    def f(acc):
        (_,t)=acc
        return Etok(name='coercion_type',etoks=[t],raw=acc)
    return (c.next_type('COERCION') + get_lookup_parse('term')).treat(f)

def coerced_type():
    """parser for (possibly implicit) coercion from term to type
    
    >>> pstream(coerced_type(),'term')
    Etok(coercion_type,'term')
    """
    def f(acc):
        return Etok(name='coercion_type',etoks=[acc],raw=acc)
    return (coercion_type() | get_lookup_parse('term').treat(f))
      
def opentail_type():
    """Parser for binop, quotient, or coercion type"""
    return binop_type() | quotient_type() | coercion_type()

def post_colon_type():
    """parser for type appearing after a colon
    
    >>> pstream(post_colon_type(),'prim_relation')
    Etok(post_colon_type,2,'prim_relation')
    """
    def f2(acc):
        return Etok(name='post_colon_type',etoks=acc,raw=acc,rule='2')
    return (get_lookup_parse('general_type') |
     (get_lookup_parse('prim_relation') + app_args()).treat(f2) |
     coerced_type())
  
# general_type - implement after attribute

add_lookup_parse('post_colon_type', c.lazy_call(post_colon_type))

def hierarchical_identifier():
    """
    Parser for hierarchical identifiers.
    Output is a Etok.
    """
    return c.next_type('HIERARCHICAL_IDENTIFIER').treat(Etok.etok)

def identifier():
    """parser for hierarchical or atomic identifier.
    Output is a single Etok"""
    return (atomic() | hierarchical_identifier()).name('identifier')

def _opt_alt_constructor():
    """Parser for a single constructor in an inductive type declaration.
    
    >>> pstream(_opt_alt_constructor(),'| id : general_type')
    Etok(alt_constructor,'| id : general_type')
    """
    def f(acc):
        (((_,i),a),t)=acc
        return Etok(name='alt_constructor',etoks=(i,a,t),raw=acc)
    return (c.next_type('ALT') + identifier() + args_template() + opt_colon_type()).treat(f)

def not_end(tok):
    """boolean token test for not keyword 'end'"""
    return not(tok.value == 'end')

def inductive_type():
    """Parser for declaration of induction types.
    It terminates with 'end' keyword.
    
    Identifier must be located internally because of recursion.
        
    >>> pstream(inductive_type(),'inductive integer | id : general_type end')
    Etok(inductive_type,'inductive integer | id : general_type end')
    """
    def f(acc):
        (((((_,i),a),s),c1),_)=acc
        #print(f'c1={c1}')
        c1 = c.retreat_list(_opt_alt_constructor().many(),[lib.fflatten(c1)])
        return Etok(name='inductive_type',etoks=(i,a,s,c1),raw=acc)
    return (c.next_word('inductive') + identifier() + args_template() + opt_colon_sort() +
            c.balanced_condition(not_end) + c.next_word('end')).treat(f)
    
def field_prefix():
    """
    parser for field prefixes: 
    coercion notationless notation parameter type call
    These are decorators or attributes.
    
    coercion - structure coerces to this field
    parameter - field can float to unbundled position
    type - objects can coerce to this type.
    call - objects can be used as a function.
    notation - field is for notational type classing.
    notationless - ??
    
    >>> pstream(field_prefix(),' random ')
    Etok(field_prefix,'')
    
    >>> pstream(field_prefix(),'a type,call,notation')
    Etok(field_prefix,'a type , call , notation')
    """ 
    def f(acc):
        keys = []
        if acc:
            (_,keys) = acc
            keys = keys[0::2]
        return Etok(name='field_prefix',etoks=keys,raw=acc)
    return (lit('a').possibly() + 
            c.comma_nonempty_list(lit_read('field_key'))).possibly().treat(f)

def field_identifier():
    """Parser for identifier in one field of structure declaration

    The word 'proof' or '_' can be used as 
    anonymous field identifiers for props.
    
    >>> pstream(field_identifier(),'x : Type')
    Etok(field_identifier,'x : type')
    
    >>> pstream(field_identifier(),'proof')
    Etok(PROOF,proof,'proof')
    """
    def fp(acc):
        return Etok(name='PROOF',etoks=[],raw=acc,rule='proof')
    def f(acc):
        return Etok(name='field_identifier',etoks=acc,raw=acc)
    return (get_lookup_parse('prim_structure') |
            (next_word('proof')|c.next_value('_')).treat(fp) | 
            ((var_or_atomic() + opt_colon_sort()) |
            (var_or_atomic() + opt_colon_type())).treat(f)
            )

def field():
    """Parser for one field of a structure
    
    >>> pstream(field(),'a call,type,parameter x := term')
    Etok(field,'a call , type , parameter x := term')
    """
    def f(acc):
        ((a,b),c)=acc
        return Etok(name='field',etoks=(a,b,c),raw=acc)
    return (field_prefix() + field_identifier() + assign_expr().possibly()).treat(f)

def structure():
    """Parser for a structure declaration
    
    >>> pstream(structure(),'notational structure with parameters { x : post_colon_type } with { parameter y := term }')
    Etok(structure,'notational structure with parameter { x : post_colon_type } with { parameter y := term }')
    """
    def f(acc):
        ((((n,_),(_,t)),_),(_,b,_))=acc
        b = c.retreat_list(field(),b[0::2]) #remove semi
        if n:
            n = Etok.etok(n)
        return Etok(name='structure',etoks=(n,t,b),raw=acc)
    # Prohibit identifiers named 'with' to avoid grammar ambiguity.
    return (next_word('notational').possibly() +
            next_word('structure') +
            (lit('param').possibly() + nonempty_args_template(omit=['with'])).possibly() +
            next_word('with').possibly() +
            c.brace_semif()).treat(f)

proof_expr # implemented above

def controlseq_term():
    """parser for terms expressed as control sequences
    
    >>> pstream(controlseq_term(),'prim_term_controlseq { term }')
    Etok(cs_brace,first,'prim_term_controlseq { term }')
    """
    return cs_brace(get_lookup_parse('prim_term_controlseq'))

def tightest_prefix():
    """Parser for very tightly bound terms.
    This prefix is the stem of the term, to which suffixes are added.
    
    >>> pstream(tightest_prefix(),'33.456')
    Etok(DECIMAL,33.456,'33.456')
    
    >>> pstream(tightest_prefix(),'1799')
    Etok(INTEGER,1799,'1799')
    """
    return (Parse.next_token().if_type(['DECIMAL','INTEGER','STRING','BLANK','VAR']).treat(Etok.etok) |
            get_lookup_parse('prim_identifier_term') |
            controlseq_term() |
            get_lookup_parse('delimited_term') |  #future reference
            get_lookup_parse('alt_term')) #future reference
    
## TO HERE

def tightest_suffix():
    """Recursive parser for suffix to a tightly bound term.
    
    The suffix can be a .field (field accessor) or subscript
    """
    return (get_lookup_parse('prim_field_term_accessor') |
            (c.lazy_call(tightest_subscript))
            )

def tightest_term():
    r"""Parser for a tightly bound term
    
    >>> pstream(tightest_term(),r'33.456 prim_field_term_accessor\sub(3)')
    Etok(tightest_term,'33.456 prim_field_term_accessor \sub ( 3 )')
    """
    def f(acc):
        return Etok(name='tightest_term',etoks=acc,raw=acc)
    return (tightest_prefix() + tightest_suffix().many()).treat(f)

def tightest_subscript():
    """Parser for subscript

    APPLYSUB handles subscripts coming from a TeX file.
    The braces have been converted to ()

    In brief, 
    x_1 is an identifier.
    x APPLYSUB (1) is equivalent to x 1 and is the de-TeXed form of x_{1}.
    x APPLYSUB (i j) is equivalent to x i j.  (This is perhaps a surprise.) 
    x APPLYSUB ((f j)) is equivalent to x (f j). 
    """
    def f(acc):
        (_,(_,t,_))=acc
        return Etok(name='apply_sub',etoks=t,raw=acc)
    return (c.next_type('APPLYSUB') + c.paren(tightest_term().plus())).treat(f)

controlseq_term # defined above

var_or_atomic_or_blank # defined above

def annotated_term():
    return annotated(get_lookup_parse('term'))

def set_enum_term():
    """parser for set enumeration
    
    >>> pstream(set_enum_term(),'{ plain_term, plain_term, plain_term }')
    Etok(set_enum_term,'{ plain_term , plain_term , plain_term }')
    """
    def f(acc):
        (_,t,_)=acc
        t = t[0::2]
        return Etok(name='set_enum_term',etoks=t,raw=acc)
    return c.brace(c.comma_list(get_lookup_parse('plain_term'))).treat(f)

def set_comprehension_term():
    """Parser for set comprehension
    
    >>> pstream(set_comprehension_term(),'{ plain_term, holding u,v \mid statement}')
    Etok(set_comprehension_term,'{ plain_term , holding u , v \mid statement }')
    """
    def f(acc):
        (_,(((p,h),_),s),_)=acc 
        return Etok(name='set_comprehension_term',etoks=(p,h,s),raw=acc)
    return c.brace(get_lookup_parse('plain_term') + holding_vars() + c.next_type('MID') + get_lookup_parse('statement')).treat(f)

def tuple_term():
    """Parser for n=tuples.  
    There must be at least one comma.
    (x) is parsed as x in parentheses.
    
    >>> pstream(tuple_term(),'(plain_term,plain_term,plain_term)')
    Etok(tuple_term,'( plain_term , plain_term , plain_term )')
    """
    def f(acc):
        (_,((p,_),ps),_)=acc
        ps = [p]+ps[0::2]
        return Etok(name='tuple_term',etoks=ps,raw=acc)
    return c.paren(get_lookup_parse('plain_term') + comma + c.comma_nonempty_list(get_lookup_parse('plain_term'))).treat(f)

def list_term():
    """Parser for lists: [a;b;c], possibly empty []

    >>> pstream(list_term(),'[plain_term;plain_term;plain_term]')
    Etok(list_term,'[ plain_term ; plain_term ; plain_term ]')
    """
    def f(acc):
        (_,ps,_)=acc
        ps = ps[0::2]
        return Etok(name='list_term',etoks=ps,raw=acc)
    return c.bracket(get_lookup_parse('plain_term').separated_list(semicolon)).treat(f)

def make_term():
    """parser for make statement (structure constructor).
    
    DEBUG: I forget the purpose of the tightest_type.
    >>> pstream(make_term(),'make { it : post_colon_type := term }')
    Etok(make_term,'make { it : post_colon_type := term }')
    """
    def fp(acc):
        ((a,b),c)=acc
        return (a,b,c)
    def f(acc):
        ((_,t),(_,b,_)) = acc
        p = (var_or_atomic_or_blank() + opt_colon_type() + 
             assign_expr().possibly()).treat(fp)
        b = c.retreat_list(p,b)
        return Etok('make_term',etoks=(t,b),raw=acc)
    return (next_word('make') + get_lookup_parse('tightest_type').possibly() +
            c.brace_semif()).treat(f)

def paren_term():
    """parser for term in parentheses
    
    >>> pstream(paren_term(),'(term)')
    Etok(term,default,'( term )')
    """
    def f(acc):
        (_,t,_)=acc
        return t.update({'raw':acc})
    return c.paren(get_lookup_parse('term')).treat(f)

def delimited_term():
    """Parser for terms that are delimited:
      
    (x), (x : A), make { x := 3 }, [1;2], 
    {3,4}, (5,6), {x : f(x)}
    
    >>> pstream(delimited_term(),'(term)')
    Etok(term,default,'( term )')
    """
    return (paren_term() |
            annotated_term() |
            make_term() |
            list_term() |
            tuple_term() |
            set_enum_term() |
            set_comprehension_term())

def alt_case():
    """Parser for a single case of a case term
    
    >>> pstream(alt_case(),'| prop := plain_term')
    Etok(alt_case,'| prop := plain_term')
    """
    def f(acc):
        (((_,p),_),t)=acc
        return Etok(name='alt_case',etoks=(p,t),raw=acc)
    return (c.next_type('ALT') + get_lookup_parse('prop') + c.next_type('ASSIGN') + get_lookup_parse('plain_term')).treat(f)

def case_term():
    """Parser for a case term
    
    >>> pstream(case_term(),'case | prop := plain_term end')
    Etok(case_term,'case | prop := plain_term end')
    """
    def f(acc):
        ((_,a),_)=acc
        a= c.retreat_list(alt_case().plus(),[lib.fflatten(a)])
        return Etok(name='case_term',etoks=a[0],raw=acc)
    return (c.next_word('case')+ c.balanced_condition(not_end) +c.next_word('end')).treat(f)

def app_term():
    """Parser for a function applied to arguments
    """
    def f(acc):
        return Etok(name='app_term',etoks=acc,raw=acc)
    return (tightest_term() + app_args()).treat(f)

def match_pats():
    return c.comma_nonempty_list(get_lookup_parse('plain_term'))

def alt_match():
    """Parser for a single alternative in match term"""
    def f(acc):
        (((_,p),_),p2)=acc
        return Etok(name='alt_match',etoks=(p,p2),raw=acc)
    return (c.next_type('ALT')+match_pats()+c.next_type('ASSIGN')+get_lookup_parse('plain_term')).treat(f)

def match_term():
    """Parser for a match term
    
    >>> pstream(match_term(),'match plain_term with | plain_term := plain_term end')
    Etok(match_term,'match plain_term with | plain_term := plain_term end')
    """
    def f(acc):
        ((((_,mp),_),b),_)=acc
        b = c.retreat_list(alt_match().plus(),[lib.fflatten(b)])
        return Etok(name='match_term',etoks=(mp,b[0]),raw=acc)
    return (next_word('match') + match_pats() + next_word('with') +
            c.balanced_condition(not_end) + next_word('end')
            ).treat(f)

def match_function():
    """parser for a function with match statement
    
    >>> pstream(match_function(),'function | plain_term := plain_term end')
    Etok(match_function,'function | plain_term := plain_term end')
    """
    def f(acc):
        ((((_,t),o),b),_)=acc
        b = c.retreat_list(alt_match().plus(),[lib.fflatten(b)])
        return Etok(name='match_function',etoks=(t,o,b),raw=acc)
    return (next_word('function') + args_template() + 
            opt_colon_type() + c.balanced_condition(not_end) +
            next_word('end')).treat(f)

def alt_term():
    """Parser for term following the '| ... end' template"""
    return (case_term() | match_term() | match_function())

# opentail_term - later

def lambda_term():
    """Parser for lambda abstraction
    
    >>> pstream(lambda_term(),'tdop_term \mapsto opentail_term')
    Etok(mapsto,'tdop_term \mapsto opentail_term')
    
    >>> pstream(lambda_term(),'fun tightest_expr := opentail_term')
    Etok(fun_term,'fun tightest_expr := opentail_term')
    """
    def f1(acc):
        ((t,_),o)=acc
        return Etok(name='mapsto',etoks=(t,o),raw=acc)
    def f2(acc):
        (((p,a),_),o)=acc
        return Etok(name='lambda_term',etoks=(p,a,o),raw=acc) 
    def f3(acc):
        (((_,t),_),o)=acc
        return Etok(name='fun_term',etoks=(t,o),raw=acc)
    return ((get_lookup_parse('tdop_term') + c.next_type('MAPSTO') + get_lookup_parse('opentail_term')).treat(f1) |
            (get_lookup_parse('prim_lambda_binder') + tightest_args() + binder_comma + get_lookup_parse('opentail_term')).treat(f2) |
            (next_word('fun')+ tightest_args() + c.next_type('ASSIGN') + get_lookup_parse('opentail_term')).treat(f3)
            )

def let_term():
    """Parser for let .... 
    
    >>> pstream(let_term(),'let x := plain_term in opentail_term')
    Etok(let,'let x := plain_term in opentail_term')
    """
    def f(acc):
        (((((_,p),_),t),_),o)=acc
        return Etok(name='let',etoks=(p,t,o),raw=acc)
    return (next_word('let') + tightest_prefix() + 
            c.next_type('ASSIGN') + get_lookup_parse('plain_term') + next_word('in') + get_lookup_parse('opentail_term')).treat(f)
    
def if_then_else_term():
    """Parse 'if bool then A else B' 
    
    >>> pstream(if_then_else_term(),'if prop then plain_term else opentail_term')
    Etok(if_then_else_term,'if prop then plain_term else opentail_term')
    """
    def f(acc):
        (((((_,p),_),t),_),f)=acc
        return Etok(name='if_then_else_term',etoks=(p,t,f),raw=acc)
    return (next_word('if') + get_lookup_parse('prop') + 
            next_word('then') + get_lookup_parse('plain_term') + next_word('else') + get_lookup_parse('opentail_term')).treat(f)
    
def opentail_term():
    """Recursive parser for terms with open tails.
    
    These are terms that can be iterated as in
    let x := y in let u:= v in tail
    if b then t else if b2 then t2 else tail
    
    Specifically, this includes lambdas, let, if_then, tdop
    
    >>> pstream(opentail_term(),'let x := plain_term in opentail_term')
    Etok(let,'let x := plain_term in opentail_term')
    """
    return (c.lazy_call(lambda_term) |
            c.lazy_call(let_term) |
            c.lazy_call(if_then_else_term) |
            get_lookup_parse('tdop_term')
            )

def where_suffix():
    """suffix to Haskell 'where'
    
    >>> pstream(where_suffix(),'where { x : post_colon_type := term ; y := term }')
    Etok(where_suffix,'where { x : post_colon_type := term ; y := term }')
    """
    def f_inner(acc):
        ((a,b),c)=acc
        return (a,b,c)
    def f(acc):
        (_,(_,b,_))=acc 
        b=c.retreat_list((var()+opt_colon_type()+assign_expr().possibly()).treat(f_inner),b[0::2])
        return Etok(name='where_suffix',etoks=b,raw=acc)
    return (next_word('where') + c.brace_semif()).treat(f)

def where_term():
    """Parser for term with (possible) Haskell style where suffix
    
    >>> pstream(where_term(),'tdop_term where {x : post_colon_type := term }')
    Etok(where_term,'tdop_term where { x : post_colon_type := term }')
    """
    def f(acc):
        return Etok('where_term',etoks=acc,raw=acc)
    return (opentail_term() + where_suffix().possibly()).treat(f)

def term_op():
    """Parser for symbolic operators
    
    >>> pstream(term_op(),'prim_term_op_controlseq { term } {term }')
    Etok(cs_brace,first,'prim_term_op_controlseq { term } { term }')
    """
    return (get_lookup_parse('prim_term_op') | 
            cs_brace(get_lookup_parse('prim_term_op_controlseq'))
            )

def term_ops():
    return term_op().plus()

def definite_term():
    """term with a definite article, subsuming where_term
    
    >>> pstream(definite_term(),'the prim_definite_noun')
    Etok(prim_definite_noun,default,'the prim_definite_noun')
    """
    def f(acc):
        (_,t)=acc
        return t.update({'raw':acc})
    return (where_term() | 
            (next_word('the') + get_lookup_parse('prim_definite_noun')).treat(f)
            )

def any_args():
    def f(acc):
        b = acc[0::2]
        return Etok(name='any_args',etoks=b,raw=acc)
    return c.comma_nonempty_list(var() | annotated_vars()).treat(f)

def any_name():
    """Parse for terms with forthel 
    natural language quantification
    
    every x, each x, all x, no x, some x,...
    
    >>> pstream(any_name(),'every x, y, z')
    Etok(any_name,'every x , y , z')
    """
    def f(acc):
        return Etok(name='any_name',etoks=acc,raw=acc)
    return (lit_read('any') +
            (any_args() |
             get_lookup_parse('pseudoterm') |
             get_lookup_parse('general_type'))).treat(f)
    
def term():
    """parser for terms, subsuming all other terms (through definite_term)"""
    def f(acc):
        (_,t)=acc 
        return t.update({'raw':acc})
    return ((get_lookup_parse('prim_classifier').possibly() + definite_term()).treat(f) |
            any_name())

def terms():
    def f(acc):
        return Etok(name='terms',etoks=acc[0::2],raw=acc)
    return c.and_comma_nonempty_list(term()).treat(f)

def isplain(etok):
    """Test if an Etok is plain."""
    if etok.name=='any_name':
        return False
    return all(isplain(e) for e in lib.fflatten(etok.etoks))

def plain_term():
    """
    Following Forthel 1.3.3,
    a plain_term contains no any_name recursively within it.
    We implement this with a separate check that the term is plain,
    rather than build plain terms as a separate nonterminal. 

    We require plain terms on the right-hand-side of definitions.
    Also, in dependent types, the terms should be plain.
    """
    return term().if_test(isplain)

def tdop_term():
    """Parser for an expression involving symbolic operators.
    tdop = top down operator precedence,
    which is how such expressions will eventually be handled.
    
    Here we just collect together the tokens in the expression
    for later handling.
    
    In the expression, there are no adjacent non-symbolic terms.
    That is, f x + y is interpreted as function application of f to x...

    There can be adjacent symbols: 3! + 1. 
    The expression can optionally begin or end with a symbol.
    
    The expression might be a solitary symbol or app_term.

    There are three general precedence categories built into 
    the grammar.  
    * prop operators; (precedence < 0) 
    * binary relation operators such as "="; (precedence=0)
    * term operators.  (precedence > 0) (this case).
    This allows us to distinguish terms from props and types.
    
    >>> pstream(tdop_term(),'x prim_term_op y')
    Etok(tdop_term,'x prim_term_op y')
    """
    def f(acc):
        (((p,o),ao),tp)=acc
        r=[o]+ao
        if p:
            r =[p]+r
        if tp:
            r=r+[tp]
        return Etok('tdop_term',etoks=r,raw=acc)
    return ((app_term().possibly() + term_ops() + 
            (app_term() + term_ops()).many() + 
            app_term().possibly()).treat(f) |
            app_term()
            )

def adjective_left_attribute():
    def f(acc):
        return Etok(name='adjective_left_attribute',etoks=acc,raw=acc)
    return (next_word('non').possibly() + get_lookup_parse('prim_simple_adjective')).treat(f)

def multisubject_left_attribute():
    return (get_lookup_parse('prim_simple_adjective_multisubject'))

def left_attribute():
    return (adjective_left_attribute() |
            multisubject_left_attribute())

def is_right_attribute():
    def f(acc):
        return Etok(name='is_right_attribute',etoks=acc[0::2],raw=acc)
    return c.andcomma_nonempty_list(get_lookup_parse('is_pred')).treat(f)

def does_right_attribute():
    def f(acc):
        (_,t)=acc
        return Etok(name='does_right_attribute',etoks=t[0::2],raw=acc)
    return (next_word('that') + c.andcomma_nonempty_list(get_lookup_parse('does_pred'))).treat(f)

def such_that_right_attribute():
    def f(acc):
        (_,t)=acc
        return t.update({'raw':acc})
    return (c.next_phrase('such that') + get_lookup_parse('statement')).treat(f)

def right_attribute():
    return (is_right_attribute() | does_right_attribute() | such_that_right_attribute())

def attribute(p):
    """Parser for a term with left and right attributes
    """
    def f(acc):
        return Etok(name='attribute',etoks=acc,raw=acc)
    return (left_attribute().many() + p + right_attribute().possibly()).treat(f)

def general_type():
    """parser for a general type. 
    This is one of the main nonterminals.
    It subsumes all specialized type nonterminals.    
    """
    return attribute(opentail_type())

# return later to pseudoterms

def binary_relation_op():
    """binary relation symbols"""
    return (get_lookup_parse('prim_binary_relation_op') | 
            cs_brace(get_lookup_parse('prim_binary_relation_controlseq'))
            )

# deprecated, now part of tdop_rel_prop
#def tdop_terms():
#    def f(acc):
#        return Etok(name='tdop_terms',etoks=acc[0::2],raw=acc)
#    return c.comma_nonempty_list(tdop_term).treat(f)

def tdop_rel_prop():
    """Parser for terms chained by binary relation symbols.
    
    All symbols have the same precedence 0.
    We allow x,y < z < w. The first arg can be a list of terms.
    The chain expands as x < z and y < z and z < w.
    output contains the list [x<z,y<z,z<w] (coded as Etoks)
    No parentheses allowed in chain.
    
    >>> pstream(tdop_rel_prop(),'x,y,z prim_binary_relation_op u prim_binary_relation_op x')
    Etok(tdop_rel_prop,'x , y , z prim_binary_relation_op u prim_binary_relation_op x')
    """
    def f(acc):
        (t,ls)=acc 
        #expand chain
        op0 = [(a,r0,t0) for a in t[0::2] for (r0,t0) in ls[0:1]] #chain comma
        op1 = [(a,r0,t0) for ((_,a),(r0,t0)) in zip(ls[:-1],ls[1:])]        
        return Etok(name='tdop_rel_prop',etoks=op0+op1,raw=acc)
    return (c.comma_nonempty_list(tdop_term()) + (binary_relation_op() + tdop_term()).plus()).treat(f)

def prop_op():
    """Parser for propositional connectives
    
    >>> pstream(prop_op(),'prim_propositional_op')
    Etok(prim_propositional_op,default,'prim_propositional_op')
    """
    return (get_lookup_parse('prim_propositional_op') | 
            cs_brace(get_lookup_parse('prim_propositional_op_controlseq'))
            )
    
def tdop_prop():
    """Parser for operators among props, such
    as and, or, implies.
    
    precedence is negative.
    It must be infix (possibly with multiple ops).
    For example, a symbolic negation is not included.
    
    subsumes binder_prop
    
    output etoks: binder_props in even positions, ops in odd positions
    
    >>> pstream(tdop_prop(),'binder_prop prim_propositional_op binder_prop')
    Etok(tdop_prop,'binder_prop prim_propositional_op binder_prop')
    """
    def f(acc):
        (b,m)=acc
        return Etok(name='tdop_prop',etoks=[b]+lib.flatten(m),raw=acc)
    return (get_lookup_parse('binder_prop') +
            (prop_op().plus() + get_lookup_parse('binder_prop')).many()).treat(f)
            
def identifier_prop():
    """Parser for identifiers of type prop"""
    return get_lookup_parse('prim_relation')

def annotated_prop():
    """Parser for prop, annotated as prop
    
    >>> pstream(annotated_prop(),'(prop : Prop)')
    Etok(annotated_prop,'( prop : prop )')
    """
    def f(acc):
        (_,((p,_),_),_) =acc
        return Etok('annotated_prop',etoks=[p],raw=acc)
    return c.paren(get_lookup_parse('prop')+colon + Parse.next_token().if_rawvalue('Prop')).treat(f)

def field_prop():
    """
    Parser for prop obtained as dotted c.f, where the field f has type prop

    Debug: should we add app_args (and move to app_args): c.f (x)?
    """
    def f(acc):
        return Etok(name='field_prop',etoks=acc,raw=acc)
    return (tightest_term() + get_lookup_parse('prim_field_prop_accessor')).treat(f)

def prop_var():
    """parser for propositional var"""
    def f(acc):
        return Etok(name='prop_var',etoks=[acc],raw=acc)
    return var().treat(f)

def tightest_prop():
    """Parser for tightly bound propositional statements"""
    def f(acc):
        (_,s,_)=acc
        return s.update({'raw':acc})
    return (c.paren(get_lookup_parse('statement')).treat(f) |
            identifier_prop() |
            prop_var() |
            annotated_prop() |
            field_prop()
        )

def app_prop():
    """parser for predicate application"""
    def f(acc):
        return Etok(name='app_prop',etoks=acc,raw=acc)
    return (tightest_prop() + app_args()).treat(f)

def lambda_predicate():
    """parser for lambda term with values in prop
    
    >>> pstream(lambda_predicate(),'fun tightest_expr : Prop := (statement)')
    Etok(lambda_predicate,'fun tightest_expr : prop := ( statement )')
    """
    def f(acc):
        #return acc
        (((((_,t),_),_),_),p)=acc
        return Etok(name='lambda_predicate',etoks=(t,p),raw=acc)
    return (next_word('fun')+ tightest_args() + colon + 
            Parse.next_token().if_rawvalue('Prop') + 
            c.next_type('ASSIGN') + tightest_prop()
            ).treat(f)

def binder_prop():
    """Recursive parser for props with (optional) binders (universal, etc.)
    Subsumes various other kinds of props.
    
    >>> pstream(binder_prop(),'prim_binder_prop tightest_expr , A')
    Etok(binder_prop,'prim_binder_prop tightest_expr , A')
    """
    def f(acc):
        (((b,a),_),b2)=acc
        return Etok(name='binder_prop',etoks=(b,a,b2),raw=acc)
    return (app_prop() |
            tdop_rel_prop() |
            lambda_predicate() |
            (  get_lookup_parse('prim_binder_prop') + 
               args_template() + binder_comma + 
               c.lazy_call(binder_prop)
               ).treat(f)
            )

def prop():
    """Parser for prop.  
    This is one of the main nonterminals.
    It subsumes all specialized prop nonterminals.
    
    The classifier is a sort of meta sort, which is currently ignored.
    It might be a word such as 'predicate'
    """
    def f(acc):
        (_,t)=acc 
        return t
    (lit('classifier').possibly() + tdop_prop()).treat(f)
    
# install binder_prop,prop

# statements...

def possessed_noun():
    return (attribute(get_lookup_parse('prim_possessed_noun')))

def has_pred():
    """Parser for has_pred or its negation.
    
    Note that commas may appear in both attributes 
    and the list of has_pred, but the parse should be unambiguous
    because of the articles.
    
    >>> pstream(has_pred(),'the prim_possessed_noun and the prim_possessed_noun')
    Etok(has_pred,'the prim_possessed_noun and the prim_possessed_noun')
    """
    def f1(acc):
        t = [p for (_,p) in acc[0::2]] # drop commas, articles
        return Etok(name='has_pred',etoks=t,raw=acc)
    def f2(acc):
        return Etok(name='no_has_pred',etoks=acc,raw=acc)
    return (c.andcomma_nonempty_list(lit('article') + possessed_noun()).treat(f1) |
     (next_word('no') + possessed_noun()).treat(f2)
     )

enot = next_word('not').treat(Etok.etok)
     
def is_aPred():
    """Parser for nominal predicates
    
    >>> pstream(is_aPred(),'not a tightest_type')
    Etok(indefinite_pred,'not a tightest_type')
    """
    def f1(acc):
        ((n,_),g)=acc 
        return Etok(name='indefinite_pred',etoks=(n,g),raw=acc)
    def f2(acc):
        return Etok(name='definite_pred',etoks=acc,raw=acc)
    return ((enot.possibly() + lit('a').possibly() + general_type()).treat(f1) |
            (enot.possibly() + definite_term()).treat(f2)
            )

def is_pred():
    """Parser for adjectival predicates
    
    >>> pstream(is_pred(),'not prim_adjective')
    Etok(is_adjective,'not prim_adjective')
    
    >>> pstream(is_pred(),'not pairwise prim_adjective_multisubject')
    Etok(is_adjective_multisubject,'not pairwise prim_adjective_multisubject')
    
    >>> pstream(is_pred(),'having the prim_possessed_noun')
    Etok(is_with,'having the prim_possessed_noun')
    """
    def f1(acc):
        return Etok(name='is_adjective',etoks=acc,raw=acc)
    def f2(acc):
        ((n,p),m)=acc
        return Etok(name='is_adjective_multisubject',etoks=(n,p,m),raw=acc)
    def f3(acc):
        return Etok(name='is_with',etoks=acc[1:],raw=acc) ##
    return (
        (enot.possibly() + get_lookup_parse('prim_adjective')).treat(f1) |
        (enot.possibly() + next_word('pairwise').treat(Etok.etok).possibly() + get_lookup_parse('prim_adjective_multisubject')).treat(f2) |
        (lit('with') + has_pred()).treat(f3)
        )

def does_pred():
    """Parser for verbal predicates.
    Umbrella for various verbal, adjectival, nominal predicates.
    """
    def f1(acc):
        ((_,n),v)=acc
        return Etok(name='do_verb',etoks=(n,v),raw=acc)
    def f2(acc):
        ((_,n),v)=acc
        return Etok(name='do_verb_multisubject',etoks=(n,v),raw=acc)
    def f3(acc):
        (_,h)=acc
        return Etok(name='do_has_pred',etoks=[h],raw=acc)
    def f4(acc):
        (_,ps)=acc
        return Etok(name='does_is_adj',etoks=ps,raw=acc)
    def f5(acc):
        (_,ps)=acc
        return Etok(name='is_nominal',etoks=ps,raw=acc)
    return (
        (lit('do').possibly() + enot.possibly() + get_lookup_parse('prim_verb')).treat(f1) |
        (lit('do').possibly() + enot.possibly() + get_lookup_parse('prim_verb_multisubject')).treat(f2) |
        (lit('has') + has_pred()).treat(f3)
        (lit('is') + c.andcomma_nonempty_list(is_pred())).treat(f4)
        (lit('is') + c.andcomma_nonempty_list(is_aPred())).treat(f5)
        )

# insert pseudoterms here

def plain_pred_pseudoterm():
    """
    xxd to here.
    """
    return c.option_paren(tdop_rel_prop() + holding_vars()).treat(f)
    
    
def this_exists():
    """parsing of 'this'-directives.
    DEBUG: Remove this feature. Deprecated Unfinished"""
    def adjective(tok):
        s1 = tok.value.lower.replace('_','')
        return s1 in ['unique','canonical','welldefined','wellpropped','total','exhaustive']
    def this_directive_right_attr():
        return next_phrase('by recursion')
    def this_directive_pred():
        # debug, need to slice [0::2]
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


def let_annotation_prefix():
    # debug need to slice [0::2]
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
    return ((first_word( 'fix let') + c.comma_nonempty_list(annotated(sort_vars()))) |
     let_annotation_prefix() + post_colon_balanced())


def nonkey(): #was not_banned
    keyword = [
        'is','be','are','denote','define','enter','namespace','stand',
        'if','iff','inferring','the','a','an','we','say','write',
        'assume','suppose','let','said','defined','or','fix','fixed'
        ]
    def p(token):
        return not(c.singularize(token.value) in keyword)
    return c.next_type(['VAR','WORD','ATOMIC_IDENTIFIER']).if_test(p)

# deprecated version
#def args_template():
#    """Form of arguments to a function declaration"""
#    def required_arg_template_pat():
#        return ( 
#            (c.paren(var_or_atomics() + opt_colon_sort_meta())) |
#            var_or_atomic()
#            )
#    return (brace_noassign().possibly() + required_arg_template_pat().many())

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
        # debug need slice [0::2]
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
        # debug need slice [0::2]
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
        # debug need slice [0::2]
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



    
    
    

if __name__ == "__main__":
    import doctest
    doctest.testmod(optionflags=doctest.ELLIPSIS | doctest.NORMALIZE_WHITESPACE)


            

  
            
            