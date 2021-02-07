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
import traceback
from exception import ParseError, ParseNoCatch, ErrorItem
import lib, word_lists
import tokenlib
import lexer
from ply.lex import LexToken #import ply.lex.LexToken as LexToken



import parser_combinator as c

from parser_combinator import (Parse, 
                               first_word, 
                               first_phrase, next_word, next_any_word,
                               next_phrase, next_value,
                               pstream)

def memo(f):
    m = {}
    def wrapper():
        if f not in m:
            m[f]= f()
        return m[f]
    return wrapper # f to run tests.

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

    def etok(tok):
        """convert a token to Etok"""
        return Etok(tok.type,[],[tok],rule=tok.value)
    
    def parse(p : Parse):
        """Promote a LexToken parser to a Etok parser.
        This is the same as p.treat(Etok.etok)
        """
        return p.treat(Etok.etok)
    
def backdoor(pr):
    """Add a backdoor to parser to allow the
    nonterminal to be parsed as a terminal Etok."""
    def pre(item):
        if not(tokenlib.eof(item)):
            item1 = tokenlib.next_item(item)
            if item1.acc.value == pr.nonterminal:
                acc = Etok.etok(item1.acc)
                return tokenlib.update(acc,item1)
        raise ParseError(ErrorItem(item,'backdoor'))
    return pr.preprocess(pre)
    
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
        
#get_lookup_parse_history = {} #for debugging.
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
    ps.append(Etok.parse(Parse.next_token().if_value(nonterminal)).treat(f,nonterminal))
    #get_lookup_parse_history['nonterminal']=None
    return Parse.first(ps).name(nonterminal,production='lookup')

    
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
                raise ParseError(ErrorItem(item,'next_word_net'))
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
    ([LexToken(WORD,'therefore',1,0)], None)
    """
    return (next_word_net(build_word_net(word_lists.transition)) + next_word('that').possibly())

def phrase_list_filler():
    """parser for filler words.
    
    Examples:
    'We know that'
    'We see'
    'See that'
    """
    return (next_word('we').possibly() + first_word('put write have know see') + 
            next_word('that').possibly())


# case sensitive words
rawtype = next_any_word().if_rawvalue('Type')
rawsort = next_any_word().if_rawvalue('Sort')
rawprop = next_any_word().if_rawvalue('Prop')
rawtrue = next_any_word().if_rawvalue('True')
rawfalse = next_any_word().if_rawvalue('False')

period = next_value('.')
comma = next_value(',')
semicolon = next_value(';')
colon = next_value(':')

lit_dict = {
    'a' : first_word('a an'), #indefinite
    'article' : first_word('a an the'),
    'assume': first_word('assume suppose'),
    'axiom': first_word('axiom conjecture hypothesis equation formula'),
    'choose': first_word('take choose pick'),
    'contradiction' : first_word('contradiction contrary'),
    'def': first_word('def definition'),
    'defined_as' : first_phrase(['said to be','defined as','defined to be']),
    'denote': first_phrase(['denote','stand for']),
    'do': first_word('do does'),
    'equal': next_phrase('equal to'),
    'exist': (next_word('there').possibly() + next_word('exist')).treat(lib.snd,'lit_exist'),
    'false': first_word('off false no'),
    'forall': (next_word('forall') | next_phrase('for all')),
    'fix': first_word('fix let'),
    'has': first_word('has have had'),
    'is' : first_phrase(['is','are','be','to be']),
    'iff':  (first_phrase(['iff','if and only if']) | 
             (first_phrase(['is','are','be','to be']) + next_word('the').possibly() + next_word('predicate'))),
    'with': first_word('with of having'),
    'true': first_word('on true yes'),
    'wrong': next_phrase('it is wrong that'),
    'lets': first_phrase(['let','let us','we','we can']),
    'then': first_word('then therefore hence'),
    'prove': first_word('prove show'),
    'say': first_word('say write'),
    'satisfy' : first_phrase(['satisfy','give rise to','determine']),
    'we-say': (next_word('we').possibly() +
            first_word('say write') +
            next_word('that').possibly()
            ),
    'qed': first_word('end qed obvious literal'),
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
                Parse.word('that').possibly()).treat(f,'that')
    else:
        return lit_dict[s].treat(f,s)
    


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
            'sort': (rawtype | rawsort),
            'assoc': first_word('left right no'),
            'field_key': first_word('coercion notationless notation parameter type call'), #renamed map -> call
            'document': first_word('document article section subsection subsubsection subdivision division'),
            'end_document': first_word('endsection endsubsection endsubsubsection enddivision endsubdivision')
        }
    if s == 'doc':
        return (local_lit_dict['document'] | local_lit_dict['end_document']).treat(f,'doc')
    if s == 'location':
        return Parse.first([local_lit_dict['document'],lit_dict['theorem'],lit_dict['axiom']]).treat(f,'location')
    return local_lit_dict[s].treat(f,s)



def opt_paren(pr):
    """
    Parse an expression optionally in parentheses, 
    discarding parentheses.  
    
    Input pr : An etok parser.
    Output: An etok parser.
    
    >>> pstream(opt_paren(Parse.next_token().treat(Etok.etok)),'(hello)')
    Etok(WORD,hello,'( hello )')
    """
    def f(acc):
        (_,t,_)=acc
        return t.update({'raw':acc})   # (left,acc[1],right)
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
    return (cs_parse + c.brace(expr()).treat(strip_delim).many()).treat(f,'cs_brace')

# case_sensitive_word -> use next_value(s)


@memo
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
    return Parse.next_token().if_type(['INTEGER','WORD','ATOMIC_IDENTIFIER']).name('atomic').treat(f,'atomic')

@memo
def label():
    return atomic()

@memo
def primitive(primitive_nonterminal):
    def f(item):
        if not(primitive_nonterminal in word_lists.prim_list):
            raise(ParseNoCatch(ErrorItem(item,primitive_nonterminal,'undeclared primitive')))
        return get_lookup_parse(primitive_nonterminal).process(item)

    return Parse(f,primitive_nonterminal,'!')

def _add_prim1():
    def equal():
        return next_value('=').treat(Etok.etok)
    add_lookup_parse('prim_binary_relation_op',equal())
    
    def bool():
        return (rawtrue | rawfalse)
    add_lookup_parse('prim_relation', bool().name('prim_relation','True-False'))
    pass

_add_prim1()

@memo
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
    return (section_tag() + label().possibly() + period).name('section_preamble').treat(f,'section_preamble')

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
        return c.plus_andcomma(synlist).treat(f,'_syn')
    
    def _treat_syn(acc):
        """build dict for synonyms.
        input acc should contain the syn lists in the form
        output by _syn.
        
        This function will expand the slashes.
        Output Etok(instruction,synonym,...)
        """
        #acc11 = acc[1][1]
        tt=[Instruction._expand_slashdash([t.value for t in ac]) for ac in acc]
        return Etok(name='instruction',etoks=[],raw=acc,rule='synonym',misc=tt)
        #d = {'production':'instruction',
        #     'raw':lib.flatten(acc),
        #     'keyword':'synonym'
        #     }
        #acc11 = acc[1][1]
        #d['value']=[Instruction._expand_slashdash([t.value for t in ac]) for ac in acc11]
        #return d
      
    def syn():
        """Synonym parser, 
        output is a fully treated Etok(instruction,synonym,...)
        """
        return Instruction._syn().treat(Instruction._treat_syn)
     
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
        def f(acc):
            (_,(_,s),_)=acc 
            return s.update({'raw':acc})
        return (c.bracket(next_word('synonym') + Instruction.syn()).treat(f) |
             c.bracket(Instruction._keyword_instruct).treat(Instruction._treat_instruct))

@memo
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
        return get_lookup_parse(nonterminal).treat(f1(nonterminal),nonterminal)
    return (get('general_type') |
            get('term') |
            get('prop') |
            get('proof_expr') |
            get('sort_expr'))

@memo
def colon_sort():
    def f(acc):
        (_,e) = acc
        return e.update({'raw':acc})    
    return (colon + get_lookup_parse('sort_expr')).treat(f,'colon_sort')

@memo
def opt_colon_sort():
    return colon_sort().possibly()

@memo
def colon_type():
    """Parse a colon then a post_colon_type.
    
    output Etok
    
    >>> pstream(colon_type(),':post_colon_type')
    Etok(post_colon_type,default,': post_colon_type')
    """
    def f(acc):
        (_,e) = acc
        return Etok.update(e,{'raw':acc})
    return (colon + get_lookup_parse('post_colon_type')).treat(f,'colon_type')

@memo
def opt_colon_type():
    return colon_type().possibly()

@memo
def var():
    """parser for a single variable.
    Accepts a single token that is a variable.
    
    >>> pstream(var(),'x')
    Etok(VAR,x,'x')
    """
    return c.next_type('VAR').name('VAR').treat(Etok.etok,'VAR')

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
    return c.paren(p + opt_colon_type()).treat(f,'annotated')

@memo
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
    return c.paren(p + opt_colon_type()).treat(f,'annotateds')

@memo
def annotated_vars():
    return annotated(var().plus())

@memo
def tvar():
    """
    >>> pstream(tvar(),'x')
    Etok(VAR,x,'x')
    
    >>> pstream(tvar(),'(x : post_colon_type)')
    Etok(annotated,VAR,'( x : post_colon_type )')
    """
    return var() | annotated_var()

@memo
def assign_expr():
    """parser for := followed by an expression
    The output is the expression at Etok
    
    >>> pstream(assign_expr(),':= general_type')
    Etok(expr,general_type,':= general_type')
    """
    def f(acc):
        (_,e) = acc
        return e.update({'raw':acc})
    return (next_value(':=') + expr()).name('assign_expr').treat(f,'assign_expr')

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

@memo
def var_or_atomic_or_blank():
    """parser for var or atomic or _.
    The parser output is a single token that is one of those types."""
    return var_or_atomic() | next_value('_').treat(Etok.etok)

@memo
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
    return c.brace_semif().treat(f_brace).reparse_list(brace_assign_item()).treat(f_final,'brace_assign')

@memo
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
    return c.brace_semif().treat(f_brace).reparse_list(brace_noassign_item()).treat(f_final,'brace_noassign')

@memo
def app_args():
    """ 
    parses the arguments of a function application.
    
    output Etok.toks (brace_assign?,[expr])
    
    >>> pstream(app_args(),'{ x:= term } tightest_expr tightest_expr ...')
    Etok(app_args,'{ x := term } tightest_expr tightest_expr')
    """
    def f(acc):
        return Etok(name='app_args',etoks=acc,raw=acc)
    return (brace_assign().possibly() + get_lookup_parse('tightest_expr').many()).treat(f,'app_args')

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
    return (var_or_atomic(omit) | annotateds(var_or_atomics_(omit))).many().treat(f,'annotated_args')

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
    return (brace_noassign().possibly() + annotated_args(omit)).treat(f,'args_template')

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

@memo        
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
                (colon_sort() | colon_type()).possibly()).treat(f,'tightest_arg'))

@memo
def tightest_args():
    return brace_noassign().possibly() + tightest_arg().many()

@memo
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
    return (comma + next_word('holding') + c.plus_comma(var())).treat(f,'holding_vars')

@memo
def proof_expr():
    r"""parser for the QED symbol
    
    >>> pstream(proof_expr(),r'\qed')
    Etok(SYMBOL_QED,\qed,'\qed')
    """
    return c.next_type('SYMBOL_QED').treat(Etok.etok,'proof_expr')

add_lookup_parse('proof_expr',proof_expr())

@memo
def tightest_expr():
    """
    Parser for expressions in which the boundaries are clear.
    """
    return (get_lookup_parse('tightest_term') | 
            get_lookup_parse('tightest_prop') | 
            get_lookup_parse('tightest_type') | proof_expr())

@memo
def sort_expr():
    """Parser for arrows ending in rawvalue Sort or Type
    
    >>> pstream(sort_expr(),'binder_type -> Type')
    Etok(sort_expr,'binder_type -> type')
    """
    def f(acc):
        (m,s) = acc
        m1 = [a for (a,_) in m]
        return Etok(name='sort_expr',etoks=(m1,s),raw=acc)
    return c.LazyParse((lambda s:((get_lookup_parse(s) + c.next_type('ARROW')).many() + lit_read('sort')).treat(f,'sort_expr')),'binder_type')

add_lookup_parse('sort_expr',sort_expr())

# colon_sort above

# opt_colon_sort above

@memo
def paren_type():
    """Parser for a type wrapped in parentheses
    
    >>> pstream(paren_type(),'(general_type)')
    Etok(general_type,default,'general_type')
    """
    def f(acc):
        (_,a,_) = acc 
        return a
    return c.paren(get_lookup_parse('general_type')).treat(f,'paren_type')

@memo
def annotated_type():
    """Parser for an annotated type
    
    >>> pstream(annotated_type(),'(general_type : Type)')
    Etok(general_type,default,'general_type')
    """
    def f(acc):
        (_,((a,_),_),_)=acc
        return a
    return c.paren(get_lookup_parse('general_type') + colon + rawtype).treat(f,'annotated_type')

@memo
def controlseq_type():
    """Parser for a control sequence type
    
    >>> pstream(controlseq_type(),'prim_type_controlseq { term }')
    Etok(cs_brace,prim_type_controlseq,'prim_type_controlseq { term }')
    """
    return cs_brace(get_lookup_parse('prim_type_controlseq'))

@memo
def const_type():
    """Parser for an identifier representing a type"""
    return get_lookup_parse('prim_identifier_type')

@memo
def field_type():
    """Parser for a field of a structure"""
    def f(acc):
        return Etok('field_type',etoks=acc,raw=acc)
    return (get_lookup_parse('tighteset_term') + get_lookup_parse('prim_field_type_accessor')).treat(f,'field_type')

@memo
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
        return ((over + c.brace_semif()).treat(f_brace).reparse_list(var_or_atomic() + assign_expr())).treat(f1,'over_args1')

    def over_args2():
        def f2(acc):
            (_,b)=acc
            return Etok(name='over_args',etoks=b,raw=acc,rule='2')
        return (over + get_lookup_parse('tightest_term')).treat(f2,'over_args2')

    def over_args3():
        def f3(acc):
            (_,(_,b),_)=acc
            return Etok(name='over_args',etoks=b[0::2],raw=acc,rule='3')
        return (c.paren(over + c.plus_comma(tightest_expr()))).treat(f3,'over_args3')
    return (over_args1() | over_args2() | over_args3())

@memo
def overstructure_type():
    """Parser for overstructure.
    The structure name must be a primitive identitifer.
    
    >>> pstream(overstructure_type(),'prim_structure { x:= term } tightest_expr over tightest_term')
    Etok(overstructure_type,'prim_structure { x := term } tightest_expr over tightest_term')
    """
    def f(acc):
        return Etok(name='overstructure_type',etoks=acc,raw=acc)
    return (get_lookup_parse('prim_structure') + app_args() + over_args().possibly()).treat(f,'overstructure_type')

@memo
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
    return (var() | c.paren(var() + colon + rawtype).treat(f2)).treat(f,'var_type')

@memo
def subtype():
    r""" 
    Parser for a subtype comprehension { x // P(x)}
    
    >>> pstream(subtype(),r'{ term, holding x \tmid statement }')
    Etok(subtype,'{ term , holding x \tmid statement }')
    """
    def f(acc):
        (_,(((t,h),_),s),_)=acc
        return Etok(name='subtype',etoks=(t,h,s),raw=acc)
    return c.brace(get_lookup_parse('term') + holding_vars().possibly() + c.next_type('TMID') + get_lookup_parse('statement')).treat(f,'subtype')

@memo
def app_type():
    """Parser for the application of a type to its arguments 
    
    >>> pstream(app_type(),'tightest_type tightest_expr')
    Etok(app_type,tightest_type,'tightest_type tightest_expr')
    """
    def f(acc):
        return Etok(name='app_type',etoks=acc,raw=acc,rule='tightest_type')
    return ((get_lookup_parse('tightest_type') + app_args()).treat(f,'app_type') |
            overstructure_type())

@memo
def binder_comma():
    """Parser for a comma in a binder expression"""
    def f(acc):
        return Etok(name='binder_comma',etoks=[Etok.etok(acc)],raw=[acc])
    return comma.treat(f,'binder_comma')

@memo
def binder_type():
    """Recursive parser for type binders (Pi-types, etc.)
    
    >>> pstream(binder_type(),'prim_pi_binder tightest_expr, tightest_type')
    Etok(binder_type,'prim_pi_binder tightest_expr , tightest_type')
    """
    def f(acc):
        (((p,a),_),b)=acc
        return Etok(name='binder_type',etoks=(p,a,b),raw=acc)
    return (app_type() | 
            (get_lookup_parse('prim_pi_binder') + tightest_args() + binder_comma() + c.lazy_call(binder_type)).treat(f,'binder_type')
            )

@memo
def agda_vars():
    """
    Agda style dependent type variables (a : A ) -> B(a)
    >>> pstream(agda_vars(),'(x : post_colon_type) (z u : post_colon_type)')
    Etok(agda_vars,'( x : post_colon_type ) ( z u : post_colon_type )')
    """
    def f(acc):
        return Etok(name='agda_vars',etoks=acc,raw=acc)
    return annotated_vars().plus().treat(f,'agda_vars')

@memo
def _type_operand():
    """
    Parser for argument of a binary type operation.
    """
    return binder_type() | agda_vars() 

@memo
def _type_op():
    """Parser for a binary type operator
    
    >>> pstream(_type_op(),'prim_type_op')
    Etok(prim_type_op,default,'prim_type_op')
    
    >>> pstream(_type_op(),'prim_type_op_controlseq { term }')
    Etok(cs_brace,prim_type_op_controlseq,'prim_type_op_controlseq { term }')
    """
    return (get_lookup_parse('prim_type_op') |
            cs_brace(get_lookup_parse('prim_type_op_controlseq')))

@memo
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
    return (brace_noassign().possibly() + (_type_operand() + _type_op()).many() + binder_type()).treat(f,'binop_type')

@memo
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
            get_lookup_parse('term')).treat(f,'quotient_type')

@memo
def coercion_type():
    r"""parser for coercion of a term to type
    
    >>> pstream(coercion_type(),r'\^term')
    Etok(coercion_type,'\^ term')
    """
    def f(acc):
        (_,t)=acc
        return Etok(name='coercion_type',etoks=[t],raw=acc)
    return (c.next_type('COERCION') + get_lookup_parse('term')).treat(f,'coercion_type')

@memo
def coerced_type():
    """parser for (possibly implicit) coercion from term to type
    
    >>> pstream(coerced_type(),'term')
    Etok(coercion_type,'term')
    """
    def f(acc):
        return Etok(name='coercion_type',etoks=[acc],raw=acc)
    return (coercion_type() | get_lookup_parse('term').treat(f,'coerced_type'))

@memo      
def opentail_type():
    """Parser for binop, quotient, or coercion type"""
    return binop_type() | quotient_type() | coercion_type()

@memo
def post_colon_type():
    """parser for type appearing after a colon
    
    >>> pstream(post_colon_type(),'prim_relation')
    Etok(post_colon_type,2,'prim_relation')
    """
    def f2(acc):
        return Etok(name='post_colon_type',etoks=acc,raw=acc,rule='2')
    return (get_lookup_parse('general_type') |
     (get_lookup_parse('prim_relation') + app_args()).treat(f2,'post_colon_type-2') |
     coerced_type())
  
# general_type - implement after attribute

add_lookup_parse('post_colon_type', c.lazy_call(post_colon_type))

@memo
def hierarchical_identifier():
    """
    Parser for hierarchical identifiers.
    Output is a Etok.
    """
    return c.next_type('HIERARCHICAL_IDENTIFIER').treat(Etok.etok,'hierarchical_identifier')

@memo
def identifier():
    """parser for hierarchical or atomic identifier.
    Output is a single Etok"""
    return (atomic() | hierarchical_identifier()).name('identifier')

@memo
def _opt_alt_constructor():
    """Parser for a single constructor in an inductive type declaration.
    
    >>> pstream(_opt_alt_constructor(),'| id : general_type')
    Etok(alt_constructor,'| id : general_type')
    """
    def f(acc):
        (((_,i),a),t)=acc
        return Etok(name='alt_constructor',etoks=(i,a,t),raw=acc)
    return (c.next_type('ALT') + identifier() + args_template() + opt_colon_type()).treat(f,'_opt_alt_constructor')

def not_period(tok):
    """boolean token test for non-period."""
    return not(tok.type == 'PERIOD')

@memo
def not_end(tok):
    """boolean token test for not keyword 'end'"""
    return not(tok.value == 'end') and not_period(tok)

@memo
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
            c.balanced_condition(not_end) + c.next_word('end')).treat(f,'inductive_type')

@memo    
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
            c.plus_comma(lit_read('field_key'))).possibly().treat(f,'field_prefix')

@memo
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
            (var_or_atomic() + opt_colon_type())).treat(f,'field_identifier')
            )

@memo
def field():
    """Parser for one field of a structure
    
    >>> pstream(field(),'a call,type,parameter x := term')
    Etok(field,'a call , type , parameter x := term')
    """
    def f(acc):
        ((a,b),c)=acc
        return Etok(name='field',etoks=(a,b,c),raw=acc)
    return (field_prefix() + field_identifier() + assign_expr().possibly()).treat(f,'field')

@memo
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
            c.brace_semif()).treat(f,'structure')

proof_expr # implemented above

@memo
def controlseq_term():
    """parser for terms expressed as control sequences
    
    >>> pstream(controlseq_term(),'prim_term_controlseq { term }')
    Etok(cs_brace,prim_term_controlseq,'prim_term_controlseq { term }')
    """
    return cs_brace(get_lookup_parse('prim_term_controlseq'))

@memo
def tightest_prefix():
    """Parser for very tightly bound terms.
    This prefix is the stem of the term, to which suffixes are added.
    
    >>> pstream(tightest_prefix(),'33.456')
    Etok(DECIMAL,33.456,'33.456')
    
    >>> pstream(tightest_prefix(),'1799')
    Etok(INTEGER,1799,'1799')
    """
    return (Parse.next_token().if_type(['DECIMAL','INTEGER','STRING','BLANK','VAR']).treat(Etok.etok,'tightest_prefix') |
            get_lookup_parse('prim_identifier_term') |
            controlseq_term() |
            get_lookup_parse('delimited_term') |  #future reference
            get_lookup_parse('alt_term')) #future reference
    
@memo
def tightest_suffix():
    """Recursive parser for suffix to a tightly bound term.
    
    The suffix can be a .field (field accessor) or subscript
    """
    return (get_lookup_parse('prim_field_term_accessor') |
            (c.lazy_call(tightest_subscript))
            )

@memo
def tightest_term():
    r"""Parser for a tightly bound term
    
    >>> pstream(tightest_term(),r'33.456 prim_field_term_accessor\sub(3)')
    Etok(tightest_term,'33.456 prim_field_term_accessor \sub ( 3 )')
    """
    def f(acc):
        return Etok(name='tightest_term',etoks=acc,raw=acc)
    return (tightest_prefix() + tightest_suffix().many()).treat(f,'tightest_term')

@memo
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
    return (c.next_type('APPLYSUB') + c.paren(tightest_term().plus())).treat(f,'tightest_subscript')

controlseq_term # defined above

var_or_atomic_or_blank # defined above

@memo
def annotated_term():
    return annotated(get_lookup_parse('term'))

@memo
def set_enum_term():
    """parser for set enumeration
    
    >>> pstream(set_enum_term(),'{ plain_term, plain_term, plain_term }')
    Etok(set_enum_term,'{ plain_term , plain_term , plain_term }')
    """
    def f(acc):
        (_,t,_)=acc
        t = t[0::2]
        return Etok(name='set_enum_term',etoks=t,raw=acc)
    return c.brace(c.many_comma(get_lookup_parse('plain_term'))).treat(f,'set_enum_term')

@memo
def set_comprehension_term():
    """Parser for set comprehension
    
    >>> pstream(set_comprehension_term(),'{ plain_term, holding u,v \mid statement}')
    Etok(set_comprehension_term,'{ plain_term , holding u , v \mid statement }')
    """
    def f(acc):
        (_,(((p,h),_),s),_)=acc 
        return Etok(name='set_comprehension_term',etoks=(p,h,s),raw=acc)
    return c.brace(get_lookup_parse('plain_term') + holding_vars() + c.next_type('MID') + get_lookup_parse('statement')).treat(f,'set_comprehension_term')

@memo
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
    return c.paren(get_lookup_parse('plain_term') + comma + c.plus_comma(get_lookup_parse('plain_term'))).treat(f,'tuple_term')

@memo
def list_term():
    """Parser for lists: [a;b;c], possibly empty []

    >>> pstream(list_term(),'[plain_term;plain_term;plain_term]')
    Etok(list_term,'[ plain_term ; plain_term ; plain_term ]')
    """
    def f(acc):
        (_,ps,_)=acc
        ps = ps[0::2]
        return Etok(name='list_term',etoks=ps,raw=acc)
    return c.bracket(get_lookup_parse('plain_term').many(semicolon)).treat(f,'list_term')

@memo
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
            c.brace_semif()).treat(f,'make_term')

@memo
def paren_term():
    """parser for term in parentheses
    
    >>> pstream(paren_term(),'(term)')
    Etok(term,default,'( term )')
    """
    def f(acc):
        (_,t,_)=acc
        return t.update({'raw':acc})
    return c.paren(get_lookup_parse('term')).treat(f,'paren_term')

@memo
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

@memo
def alt_case():
    """Parser for a single case of a case term
    
    >>> pstream(alt_case(),'| prop := plain_term')
    Etok(alt_case,'| prop := plain_term')
    """
    def f(acc):
        (((_,p),_),t)=acc
        return Etok(name='alt_case',etoks=(p,t),raw=acc)
    return (c.next_type('ALT') + get_lookup_parse('prop') + c.next_type('ASSIGN') + get_lookup_parse('plain_term')).treat(f,'alt_case')

@memo
def case_term():
    """Parser for a case term
    
    >>> pstream(case_term(),'case | prop := plain_term end')
    Etok(case_term,'case | prop := plain_term end')
    """
    def f(acc):
        ((_,a),_)=acc
        a= c.retreat_list(alt_case().plus(),[lib.fflatten(a)])
        return Etok(name='case_term',etoks=a[0],raw=acc)
    return (c.next_word('case')+ c.balanced_condition(not_end) +c.next_word('end')).treat(f,'case_term')

@memo
def app_term():
    """Parser for a function applied to arguments
    """
    def f(acc):
        return Etok(name='app_term',etoks=acc,raw=acc)
    return (tightest_term() + app_args()).treat(f,'app_term')

@memo
def match_pats():
    return c.plus_comma(get_lookup_parse('plain_term'))

@memo
def alt_match():
    """Parser for a single alternative in match term"""
    def f(acc):
        (((_,p),_),p2)=acc
        return Etok(name='alt_match',etoks=(p,p2),raw=acc)
    return (c.next_type('ALT')+match_pats()+c.next_type('ASSIGN')+get_lookup_parse('plain_term')).treat(f,'alt_match')

@memo
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
            ).treat(f,'match_term')

@memo
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
            next_word('end')).treat(f,'match_function')

@memo
def alt_term():
    """Parser for term following the '| ... end' template"""
    return (case_term() | match_term() | match_function())

# opentail_term - later

@memo
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
    return ((get_lookup_parse('tdop_term') + c.next_type('MAPSTO') + get_lookup_parse('opentail_term')).treat(f1,'mapsto') |
            (get_lookup_parse('prim_lambda_binder') + tightest_args() + binder_comma() + get_lookup_parse('opentail_term')).treat(f2,'lambda_term') |
            (next_word('fun')+ tightest_args() + c.next_type('ASSIGN') + get_lookup_parse('opentail_term')).treat(f3,'fun_term')
            )

@memo
def let_term():
    """Parser for let .... 
    
    >>> pstream(let_term(),'let x := plain_term in opentail_term')
    Etok(let,'let x := plain_term in opentail_term')
    """
    def f(acc):
        (((((_,p),_),t),_),o)=acc
        return Etok(name='let',etoks=(p,t,o),raw=acc)
    return (next_word('let') + tightest_prefix() + 
            c.next_type('ASSIGN') + get_lookup_parse('plain_term') + next_word('in') + get_lookup_parse('opentail_term')).treat(f,'let_term')

@memo
def if_then_else_term():
    """Parse 'if bool then A else B' 
    
    >>> pstream(if_then_else_term(),'if prop then plain_term else opentail_term')
    Etok(if_then_else_term,'if prop then plain_term else opentail_term')
    """
    def f(acc):
        (((((_,p),_),t),_),f)=acc
        return Etok(name='if_then_else_term',etoks=(p,t,f),raw=acc)
    return (next_word('if') + get_lookup_parse('prop') + 
            next_word('then') + get_lookup_parse('plain_term') + next_word('else') + get_lookup_parse('opentail_term')).treat(f,'if_then_else_term')

@memo    
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

@memo
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
    return (next_word('where') + c.brace_semif()).treat(f,'where_suffix')

@memo
def where_term():
    """Parser for term with (possible) Haskell style where suffix
    
    >>> pstream(where_term(),'tdop_term where {x : post_colon_type := term }')
    Etok(where_term,'tdop_term where { x : post_colon_type := term }')
    """
    def f(acc):
        return Etok('where_term',etoks=acc,raw=acc)
    return (opentail_term() + where_suffix().possibly()).treat(f,'where_term')

@memo
def term_op():
    """Parser for symbolic operators
    
    >>> pstream(term_op(),'prim_term_op_controlseq { term } {term }')
    Etok(cs_brace,prim_term_op_controlseq,'prim_term_op_controlseq { term } { term }')
    """
    return (get_lookup_parse('prim_term_op') | 
            cs_brace(get_lookup_parse('prim_term_op_controlseq'))
            )

@memo
def term_ops():
    return term_op().plus()

@memo
def definite_term():
    """term with a definite article, subsuming where_term
    
    >>> pstream(definite_term(),'the prim_definite_noun')
    Etok(prim_definite_noun,default,'the prim_definite_noun')
    """
    def f(acc):
        (_,t)=acc
        return t.update({'raw':acc})
    return (where_term() | 
            (next_word('the') + get_lookup_parse('prim_definite_noun')).treat(f,'definite_term')
            )

@memo
def any_args():
    def f(acc):
        b = acc[0::2]
        return Etok(name='any_args',etoks=b,raw=acc)
    return c.plus_comma(var() | annotated_vars()).treat(f,'any_args')

@memo
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
             get_lookup_parse('general_type'))).treat(f,'any_name')

@memo    
def term():
    """parser for terms, subsuming all other terms (through definite_term)"""
    def f(acc):
        (_,t)=acc 
        return t.update({'raw':acc})
    return ((get_lookup_parse('prim_classifier').possibly() + definite_term()).treat(f,'term') |
            any_name())

@memo
def terms():
    def f(acc):
        return Etok(name='terms',etoks=acc[0::2],raw=acc)
    return c.plus_andcomma(term()).treat(f,'terms')

def isplains(etoks):
    """Boolean test if a (nested) list of Etok is plain.
    All elements must be Etok s (or test to False).
    """
    return all(isplain(e) for e in lib.fflatten(etoks) if e)

def isplain(etok):
    """Boolean test if an Etok is plain.
    Input must be an Etok.
    """
    if etok.name=='any_name':
        return False
    return isplains(etok.etoks)

@memo
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

@memo
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
            app_term().possibly()).treat(f,'tdop_term') |
            app_term()
            )

@memo
def adjective_left_attribute():
    def f(acc):
        return Etok(name='adjective_left_attribute',etoks=acc,raw=acc)
    return (next_word('non').possibly() + get_lookup_parse('prim_simple_adjective')).treat(f,'adjective_left_attribute')

@memo
def multisubject_left_attribute():
    return (get_lookup_parse('prim_simple_adjective_multisubject'))

@memo
def left_attribute():
    return (adjective_left_attribute() |
            multisubject_left_attribute())

@memo
def is_right_attribute():
    def f(acc):
        return Etok(name='is_right_attribute',etoks=acc[0::2],raw=acc)
    return c.plus_andcomma(get_lookup_parse('is_pred')).treat(f,'is_right_attribute')

@memo
def does_right_attribute():
    def f(acc):
        (_,t)=acc
        return Etok(name='does_right_attribute',etoks=t[0::2],raw=acc)
    return (next_word('that') + c.plus_andcomma(get_lookup_parse('does_pred'))).treat(f,'does_right_attribute')

@memo
def such_that_right_attribute():
    def f(acc):
        (_,t)=acc
        return t.update({'raw':acc})
    return (c.next_phrase('such that') + get_lookup_parse('statement')).treat(f,'such_that_right_attribute')

@memo
def right_attribute():
    return (is_right_attribute() | does_right_attribute() | such_that_right_attribute())

def attribute(p):
    """Parser for a term with left and right attributes
    """
    def f(acc):
        return Etok(name='attribute',etoks=acc,raw=acc)
    return (left_attribute().many() + p + right_attribute().possibly()).treat(f,'attribute')

@memo
def general_type():
    """parser for a general type. 
    This is one of the main nonterminals.
    It subsumes all specialized type nonterminals.    
    """
    return attribute(opentail_type())

@memo
def binary_relation_op():
    """binary relation symbols"""
    return (get_lookup_parse('prim_binary_relation_op') | 
            cs_brace(get_lookup_parse('prim_binary_relation_controlseq'))
            )

# deprecated, now part of tdop_rel_prop
#def tdop_terms():
#    def f(acc):
#        return Etok(name='tdop_terms',etoks=acc[0::2],raw=acc)
#    return c.plus_andcomma(tdop_term).treat(f,'tdop_terms')

@memo
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
    return (c.plus_andcomma(tdop_term()) + (binary_relation_op() + tdop_term()).plus()).treat(f,'tdop_rel_prop')

@memo
def prop_op():
    """Parser for propositional connectives
    
    >>> pstream(prop_op(),'prim_propositional_op')
    Etok(prim_propositional_op,default,'prim_propositional_op')
    """
    return (get_lookup_parse('prim_propositional_op') | 
            cs_brace(get_lookup_parse('prim_propositional_op_controlseq'))
            )

@memo    
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
            (prop_op().plus() + get_lookup_parse('binder_prop')).many()).treat(f,'tdop_prop')
 
@memo           
def identifier_prop():
    """Parser for identifiers of type prop"""
    return get_lookup_parse('prim_relation')

@memo
def annotated_prop():
    """Parser for prop, annotated as prop
    
    >>> pstream(annotated_prop(),'(prop : Prop)')
    Etok(annotated_prop,'( prop : prop )')
    """
    def f(acc):
        (_,((p,_),_),_) =acc
        return Etok('annotated_prop',etoks=[p],raw=acc)
    return c.paren(get_lookup_parse('prop')+colon + rawprop).treat(f,'annotated_prop')

@memo
def field_prop():
    """
    Parser for prop obtained as dotted c.f, where the field f has type prop

    Debug: should we add app_args (and move to app_args): c.f (x)?
    """
    def f(acc):
        return Etok(name='field_prop',etoks=acc,raw=acc)
    return (tightest_term() + get_lookup_parse('prim_field_prop_accessor')).treat(f,'field_prop')

@memo
def prop_var():
    """parser for propositional var"""
    def f(acc):
        return Etok(name='prop_var',etoks=[acc],raw=acc)
    return var().treat(f,'prop_var')

@memo
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

@memo
def app_prop():
    """parser for predicate application"""
    def f(acc):
        return Etok(name='app_prop',etoks=acc,raw=acc)
    return (tightest_prop() + app_args()).treat(f,'any_prop')

@memo
def lambda_predicate():
    """parser for lambda term with values in prop
    
    >>> pstream(lambda_predicate(),'fun tightest_expr : Prop := (statement)')
    Etok(lambda_predicate,'fun tightest_expr : prop := ( statement )')
    """
    def f(acc):
        #return acc
        (((((_,t),_),_),_),p)=acc
        return Etok(name='lambda_predicate',etoks=(t,p),raw=acc)
    return (next_word('fun')+ tightest_args() + colon + rawprop +
            c.next_type('ASSIGN') + tightest_prop()
            ).treat(f,'lambad_predicate')

@memo
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
               args_template() + binder_comma() + 
               c.lazy_call(binder_prop)
               ).treat(f,'binder_prop')
            )

@memo
def prop():
    """Parser for prop.  
    This is one of the main nonterminals.
    It subsumes all specialized prop nonterminals.
    
    The classifier is a sort of meta sort, which is currently ignored.
    It might be a word such as 'predicate'
    
    >>> pstream(prop(),'binder_prop')
    Etok(tdop_prop,'binder_prop')
    """
    def f(acc):
        (_,t)=acc 
        return t
    return (get_lookup_parse('prim_classifier').possibly() + tdop_prop()).treat(f,'prop')
    
# install binder_prop,prop

# statements...

@memo
def possessed_noun():
    return (attribute(get_lookup_parse('prim_possessed_noun')))

@memo
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
    return (c.plus_andcomma(lit('article') + possessed_noun()).treat(f1,'has_pred') |
     (next_word('no') + possessed_noun()).treat(f2,'has_no_pred')
     )

enot = next_word('not').treat(Etok.etok,'not')

@memo     
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
    return ((enot.possibly() + lit('a').possibly() + general_type()).treat(f1,'indefinite_pred') |
            (enot.possibly() + definite_term()).treat(f2,'definite_pred')
            )

@memo
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
        (enot.possibly() + get_lookup_parse('prim_adjective')).treat(f1,'is_adjective') |
        (enot.possibly() + next_word('pairwise').treat(Etok.etok).possibly() + get_lookup_parse('prim_adjective_multisubject')).treat(f2,'is_adjective_multisubject') |
        (lit('with') + has_pred()).treat(f3,'is_with')
        )

@memo
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
        (lit('do').possibly() + enot.possibly() + get_lookup_parse('prim_verb')).treat(f1,'do_verb') |
        (lit('do').possibly() + enot.possibly() + get_lookup_parse('prim_verb_multisubject')).treat(f2,'do_verb_multisubject') |
        (lit('has') + has_pred()).treat(f3,'do_has_pred') |
        (lit('is') + c.plus_andcomma(is_pred())).treat(f4,'does_is_adj') |
        (lit('is') + c.plus_andcomma(is_aPred())).treat(f5,'is_nominal')
        )

# pseudoterms here
@memo
def plain_pred_pseudoterm():
    """Parser for a pseudoterm.
    
    A pseudoterm is not a term in the grammar. 
    It is a term-like entity that can be 
    quantified over by extracting the
    free variables from the pseudoterm and
    quantifying over them.
    For example, 'for all x,y < 5'.
    
    The output is checked to be plain.
    
    >>> pstream(plain_pred_pseudoterm(),'x, y = u, holding x')
    Etok(plain_pred_pseudoterm,'x , y = u , holding x')
    """
    def f(acc):
        return Etok(name='plain_pred_pseudoterm',etoks=acc,raw=acc)
    return opt_paren(tdop_rel_prop() + holding_vars().possibly()).if_test(isplains).treat(f,'plain_pred_pseudo_term')
 
#def predicate_pseudoterm():
#    """Parse a plain_pred_pseudoterm with attribute"""
#    return attribute(plain_pred_pseudoterm())

#def attribute_pseudoterm():
#    """Parser for a pseudoterm with attribute"""
#    return attribute(pseudoterm_without_attribute())
@memo
def pseudoterm_without_attribute():
    """Recursive parser for various pseudoterms
    
    >>> pstream(pseudoterm_without_attribute(),'x of type tightest_type')
    Etok(annotated,'x of type tightest_type')
    
    >>> pstream(attribute(pseudoterm_without_attribute()),'x')
    Etok(attribute,'x')
    """
    def f2(acc):
        (_,t)=acc 
        return t.update({'raw':acc})
    def f3(acc):
        ((v,_),ann)=acc 
        return Etok('annotated',etoks=(v,ann),raw=acc)
    def f5(acc):
        (_,ps,_)=acc
        return ps.update({'raw':acc})
    return (get_lookup_parse('prim_typed_name') |
            (get_lookup_parse('prim_classifier') + tvar()).treat(f2,'pseudoterm-2') |
            (var() + (lit('with') + next_word('type')) + opentail_type()).treat(f3,'pseudoterm-3') |
            tvar() | #after: var with...
            c.paren(c.lazy_call(pseudoterm_without_attribute)).treat(f5,'pseudoterm-5')
            )

@memo
def pseudoterm():
    """
    Parser for pseudoterm.
    This is the principal nonterminal for pseudoterm,
    subsuming others.
    """
    def f(acc):
        return Etok(name='pseudoterm',etoks=acc,raw=acc)
    return (attribute(pseudoterm_without_attribute()) |
            attribute(plain_pred_pseudoterm()) ).treat(f,'pseudoterm')

# statements 

comma_and = comma + next_word('and')
comma_or = comma + next_word('or')

filler = phrase_list_filler().possibly()

@memo
def simple_statement():
    """Parser for simple statement"""
    def f(acc):
        return Etok(name='simple_statement',etoks=acc,raw=acc)
    return terms() + does_pred().plus(next_word('and'))

@memo
def there_is_statement():
    """Parser for pseudoterm existence"""
    def f(acc):
        (((_,_),n),p)=acc
        return Etok(name='there_is_statement',etoks=(n,p),raw=acc)
    return (next_word('there')+lit('exist')+next_word('no').possibly()+pseudoterm()).treat(f,'there_is_statement')

@memo
def const_statement():
    def f(acc):
        return Etok(name='const_statement',etoks=acc,raw=acc)
    return ((next_word('the').possibly() + next_word('thesis')) | 
            (lit('article').possibly() + lit('contradiction')))

@memo
def symbol_statement():
    """Recursive parser for first-order-logic like statements
    
    Debug: should parse blocks of binders in single pass.
    
    >>> pstream(symbol_statement(),'binder_prop')
    Etok(tdop_prop,'binder_prop')
    
    >>> pstream(symbol_statement(),'binder_prop')
    Etok(tdop_prop,'binder_prop')
    
    >>> pstream(symbol_statement(),'forall x, binder_prop')
    Etok(forall_symbol_statement,'forall x , binder_prop')
    """
    def f_forall(acc):
        (((_,a),_),s)=acc
        return Etok(name='forall_symbol_statement',etoks=(a,s),raw=acc)
    def f_exist(acc):
        (((_,a,_),s))=acc
        return Etok(name='exist_symbol_statement',etoks=(a,s),raw=acc)
    def f_not(acc):
        return Etok(name='not_symbol_statement',etoks=acc,raw=acc)
    def f(acc):
        (_,s,_)=acc
        return s.update({'raw':acc})
    return (
        prop() |
        (lit('forall') + pseudoterm() + binder_comma() + c.lazy_call(symbol_statement)).treat(f_forall,'forall_statement') | 
        (lit('exist') + pseudoterm() + binder_comma() + c.lazy_call(symbol_statement)).treat(f_exist,'exist') | 
        (next_word('not') + c.lazy_call(symbol_statement)).treat(f_not,'not') |
        (c.paren(c.lazy_call(symbol_statement))).treat(f,'symbol_statement') 
        )

@memo
def primary_statement():
    """Parser for primary statement"""
    return (
        simple_statement() |
        there_is_statement() |
        (filler + const_statement()).treat(lib.snd) |
        (filler + symbol_statement()).treat(lib.snd) 
        )

@memo
def head_primary():
    return (get_lookup_parse('head_statement') |
            primary_statement())

@memo
def or_chain():
    """Parser for chain of or statements"""
    def f(acc):
        ((p,_),h)=acc
        return Etok(name='or_chain',etoks=p[0::2]+[h],raw=acc)
    return (primary_statement().plus(comma_or) + comma_or + head_primary()).treat(f,'or_chain')

@memo
def and_chain():
    """Parser for chain of and statements"""
    def f(acc):
        ((p,_),h)=acc
        return Etok(name='and_chain',etoks=p[0::2]+[h],raw=acc)
    return (primary_statement().plus(comma_and) + comma_and + head_primary()).treat(f,'and_chain')

@memo
def andor_chain():
    """Parser for chain of and/or statements"""
    return (and_chain() | or_chain() | primary_statement())

@memo
def chain_statement():
    """Parser for chain of and/or/iff statements"""
    def f(acc):
        (((_,ao,_),_),s)=acc
        return Etok('iff_statement',etoks=(ao,s),raw=acc) 
    return (andor_chain () |
            (c.paren(andor_chain) + lit('iff') + get_lookup_parse('statement')).treat(f,'iff_statement')
            )

@memo
def head_statement():
    """Parser for if/then, negation, for ..., statements
    We distinguish between if-then statements and if-then terms.    
    """
    def f_for(acc):
        (((_,p),_),s)=acc
        return Etok(name='for_statement',etoks=(p,s),raw=acc)
    def f_ifthen(acc):
        ((((_,s),_),_),s2)=acc
        return Etok(name='if_then_statement',etoks=(s,s2),raw=acc)
    def f_wrong(acc):
        return Etok(name='wrong_statement',etoks=acc[1:],raw=acc)
    return (
        # DEBUG: use quasiterm instead of any_name?
        (next_word('for') + c.plus_andcomma(any_name()) + binder_comma + get_lookup_parse('statement')).treat(f_for,'for_statement') |
        (next_word('if')+ get_lookup_parse('statement') + comma + next_word('then') + get_lookup_parse('statement')).treat(f_ifthen,'if_then_statement') |
        (lit('wrong') + get_lookup_parse('statement')).treat(f_wrong,'wrong_statement')
        )

@memo
def statement():
    """Parser for statement.
    This subsumes other specialized statements."""
    return head_statement() | chain_statement()

# next texts
 
@memo           
def namespace():
    """Not implemented. Always fails."""
    return Parse.fail()

@memo
def synonym_item():
    """Parser for synonym item as text item
    
    >>> pstream(synonym_item(),'we introduce synonyms rough/-en, tilde/ tildilla.')
    Etok(instruction,synonym,'we introduce synonym rough /- en , tilde / tildilla .')
    """
    pre = next_word('we').possibly() + next_word('introduce').possibly() + next_word('synonyms')
    def f(acc):
        ((_,b),_)=acc
        b1 = c.retreat_list(Instruction.syn(),[b])
        return b1[0].update({'raw':acc})
    return (pre + c.balanced_condition(not_period) + period).commit(pre).treat(f)

@memo
def mutual_inductive_type_item():
    """DEBUG: not tested"""
    pre = lit('declare_mutual_inductive_type')
    def f(acc):
        (((_,w),pa),_)=acc 
        if pa:
            pa = pa[1]
        return Etok('mutual_inductive_type_item',etoks=(w[0::2],pa),raw=acc)
    return (pre + atomic().plus_comma() + 
            (lit('param') + args_template()).possibly() + period
            ).commit(pre).treat(f,'mutual_inductive_type_item')

@memo
def mutual_inductive_def_item():
    """DEBUG: not tested"""
    pre = lit('declare_mutual_inductive_def')
    def f(acc):
        (((_,w),pa),_)=acc 
        if pa:
            pa = pa[1]
        return Etok('mutual_inductive_def_item',etoks=(w[0::2],pa),raw=acc)
    return (pre + atomic().plus_comma() + 
            (lit('param') + args_template()).possibly() + period
            ).commit(pre).treat(f,'mutual_inductive_def_item')

@memo
def moreover_implements():
    """DEBUG: not tested.
    Deprecated. Add predicate satisfaction instead.
    Parser for an item that extends a structure or
    inductive type with """
    def f(acc):
        (((((_,_),g),_),b),_)=acc
        b = c.reparse_list(field(),b[0::2])
        return (g,b)
    return (next_word('moreover') + comma + general_type() +
            lit('implement') + c.brace_semif() + period).treat(f,'moreover_implements')

@memo
def satisfy_item():
    """
    Parser for item that extends a given type with 
    a unique existence statement, used in satisfaction-style
    structural typing of structures.
    
    This is used to define coercions say from a 
    metric space to topological space.
    
    The statement should be exists unique.
    
    DEBUG: A pseudoterm might be too general.
    We want expressions like (G:group) or group G ...
    
    >>> pstream(satisfy_item(),'Every (G: post_colon_type) satisfies binder_prop.')
    Etok(satisfy_item,'every ( G : post_colon_type ) satisfy binder_prop .')
    """
    def f(acc):
        (((((_,p),_),f),s),_)=acc 
        return Etok('satisfy_item',etoks=(p,f,s),raw=acc)
    pre = next_word('every') + pseudoterm() +lit('satisfy')  
    return (pre + opt_paren(field_prefix()).possibly() + statement() + period).commit(pre,'satisfy_item').treat(f,'satisfy_item')

@memo
def misc_text_item():
    """
    synonym_item ends in period!
    """
    return (
        synonym_item() |
        mutual_inductive_type_item() |
        mutual_inductive_def_item () |
        #moreover_implements() | # deprecated
        namespace() |
        satisfy_item()
        )

@memo
def text_item():
    """A text item is a major block of texts
    in a document.
    Every item must end with a '.' or be an
    instruction in [].
    """
    return (
        section_preamble() |
        Instruction.instruction() |
        get_lookup_parse('declaration') |
        get_lookup_parse('macro') |
        misc_text_item()
        )
  
def this_exists_deprecated():
    """parsing of 'this'-directives.
    DEBUG: Remove this feature. Deprecated Unfinished.
    """
    def adjective(tok):
        s1 = tok.value.lower.replace('_','')
        return s1 in ['unique','canonical','welldefined','wellpropped','total','exhaustive']
    def this_directive_right_attr():
        return next_phrase('by recursion')
    def this_directive_pred():
        # debug, need to slice [0::2]
        return c.plus_andcomma(Parse.next_token().if_test(adjective))
    return first_phrase(['this exist','this is'])

#def post_colon_balanced():
#    def p(token):
#        return token.value not in ['end','with',':=',';','.',',','|',':']
#    return c.balanced_condition(p)

#def meta_tok():
#    tok = c.mk_token({'type':'META','value':str(meta_tok.count)})
#    meta_tok.count += 1
#    return tok


#    tok = copy.copy(c.init_item.tok)
#    tok.value = str(meta_tok.count)
#    tok.type = 'META'
#    meta_tok.count += 1
#    return tok 

#def colon_annotation(prs):  #was opt_colon_type, opt_colon_sort
#    """Parser for ': A', discarding the colon. 
#    A is parsed by prs.
#    Parser returns an empty list if there is no annotation."""
#    #def trt1(toks):
#    #    if len(toks)==0:
#    #        return toks
#   #    return prs.process(toks)
#    prs1= (next_value(':') + post_colon_balanced()).treat(lib.snd).possibly().treat(lib.fflatten)
#    return prs1.reparse(prs)

#def colon_annotation_or_meta(prs): #was opt_colon_type_meta, opt_colon_sort_meta
#    """Parser for annotation ': A', discarding the colon.
#    If no annotation, parser returns a meta-variable.
#    A is parsed using prs."""
#    def trt(acc):
#        if acc == [] or acc == None:
#            return meta_tok()
#        return acc
#    return colon_annotation(prs).treat(trt)

# differ only in treatment
#def opt_colon_sort():
#    return opt_colon_type()

#def opt_colon_sort_meta():
#    return opt_colon_type_meta()

def then_prefix():
    return lit('then').possibly()

def axiom_preamble():
    def f(acc):
        ((a,l),_)=acc
        return Etok(name='axiom_preamble',etoks=(a,l),raw=acc)
    return (lit('axiom')+label().possibly() + period).treat(f)

# to here.
def axiom():
    return 0 # (axiom_preamble() + )

def let_annotation_prefix():
    # debug need to slice [0::2]
    return (next_word('let') + c.plus_comma(var()) +
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
    return ((first_word( 'fix let') + c.plus_comma(annotated(sort_vars()))) |
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
        return c.plus_andcomma(lit('location').possibly() + atomic())
    
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
        return c.plus_andcomma(phrase_list_transition())
    
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
                ).treat(f,'precedence_level')

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
        return prs.treat(tr,'insection')
        
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
        return c.plus_andcomma(c.next_any_word_except(['is','are','be']).plus())
    
    def classifier_def():
        return (
            next_word('let') + Pattern.classifier_word_pattern() +
            lit('is') + lit('a').possibly() + next_word('classifier')
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


            

  
            
            