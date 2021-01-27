#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 22 17:00:38 2020

@author: thales


production rules for Colada

We have two types of rule output.
Unprocessed collections of tokens from parser_combinator,
Etok

To do:
    Make parser_combinators lossless, to improve raw LexToken fidelity.
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
                               mk_stream)

#print( type(mk_stream('hi').stream[0]))


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
                raise TypeError(f'etoks must be a list of Etok in {self.name}, not {type(e).__name__}')
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
    
    def update(e:'Etok',d={}):
        """Use a dictionary d to update e:Etok.
        If the dictionary is empty, a copy of e is made.
        """
        e1 = Etok(name=e.name,etoks=e.etoks,raw=e.raw,rule=e.rule,misc=e.misc,altrepr=e.altrepr)
        for key,value in d.items():
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
    
    >>> get_lookup_parse('hello').process(mk_stream('hello and')).acc
    Etok(hello,default,'hello')
    """
    def f(acc):
        return Etok.etok(acc).update({'name':nonterminal,'rule':'default'})
    ps = lookup_parse.get(nonterminal,[])
    ps.append(Etok.parse(Parse.next_token().if_value(nonterminal)).treat(f))
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
    
    >>> next_word_net(build_word_net(['aa bb cc','bb cc','aa bb cc dd'])).process(mk_stream('aa bb cc dd ee')).acc
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
    
    >>> phrase_list_transition().process(mk_stream('therefore')).pos
    1
    
    >>> phrase_list_transition().process(mk_stream('we first show that a')).pos
    4
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
    'qed': first_word('end qed obvious literal'),
    'def': first_word('def definition'),
    'axiom': first_word('axiom conjecture hypothesis equation formula'),
    'with-property': next_phrase('with property'),
    'param': next_phrase('with parameter'),
    'theorem': first_word('proposition theorem lemma corollary'),
    # type proposition property classsifier atomic 
    }

def lit(s):
    """parser generator for 's'-like words or phrases
    
    canned phrases that have small variants
    lit[w] gives parser for w-like words or phrases
    
    Output Etok(name='lit', rule=s, value=None)
    
    >>> lit('qed').process(mk_stream('obvious')).acc
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
    
    >>> lit_read('assoc').process(mk_stream('right')).acc
    Etok(ASSOC,right,'right')
    """
    def f(acc):
        return Etok(s.upper(),[],[acc],acc.value)
    local_lit_dict = {
            'assoc': first_word('left right no'),
            'field-key': first_word('coercion notationless notation parameter type call'),
            'document': first_word('document article section subsection subsubsection subdivision division'),
            'end-document': first_word('endsection endsubsection endsubsubsection enddivision endsubdivision')
        }
    if s == 'doc':
        return (local_lit_dict['document'] | local_lit_dict['end-document']).treat(f)
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
    
    >>> option_paren(Parse.next_token()).process(mk_stream('(hello)')).acc
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
    
    
    >>> cs_brace(Etok.parse(next_any_word())).process(mk_stream('cs {term} {term} c')).acc
    Etok(cs_brace,word,'cs term term')
    """
    def f(acc):
        return Etok(name='cs_brace',etoks=acc,raw=Etok.get_raw([acc]),rule=cs_parse.nonterminal)      
    return (cs_parse + c.brace(expr()).treat(c.strip_delim).many()).treat(f)

# case_sensitive_word -> use next_value(s)



def atomic():
    """parser for atomic identifiers, 
    converting words and integers as needed
    
    Atomic identifiers cannot be a single letter (a short var)
    wordlike atomic identifiers are modulo case-sing-syn.
    but hierarchical identifiers are always case sensitive.
    
    Integers are included for section numbers in labels.
    
    output Etok
    
    >>> atomic().process(mk_stream('HELLO')).acc
    Etok(ATOMIC,HELLO,'HELLO')
    """
#    def f(item):
#        item1 = Parse.next_token().process(item)
#        result = item1.tok
#        if result.type == 'INTEGER' or result.type == 'WORD':
#            tok = copy.copy(result)
#            if tok.type == 'WORD':
#                tok.value = c.synonymize(tok.value)
#            tok.type = 'ATOMIC_IDENTIFIER'
#            return (tok,item1)
#        if result.type == 'ATOMIC_IDENTIFIER':
#            return result
#        raise ParseError(item,'atomic','main-atomic')
        
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
        return (Parse.next_token().if_raw_value('True') | Parse.next_token().if_raw_value('False'))
    add_lookup_parse('prim_relation', bool().name('prim_relation','True-False'))
    pass

_add_prim1()

def section_preamble():
    """Section label.
    
    Output Etok.etoks = [section,label?]
    
    >>> section_preamble().process(mk_stream('Section 3.')).acc
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
        
        >>> Instruction._syn().process(mk_stream('aa/bb,cc/-dd,ee/ff')).acc
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
        
        >>> Instruction.instruction().process(mk_stream('[exit 1]')).acc
        Etok(instruction,exit,'[ exit 1 ]')
 
        >>> Instruction.instruction().process(mk_stream('[read filename]')).acc
        Etok(instruction,read,'[ read filename ]')
       
        >>> Instruction.instruction().process(mk_stream('[synonym another/extras, yet/-s]')).acc
        Etok(instruction,synonym,'[ synonym another / extra yet /- s ]')
        """
        
        return (c.bracket(next_word('synonym') + Instruction._syn()).treat(Instruction._treat_syn) |
             c.bracket(Instruction._keyword_instruct).treat(Instruction._treat_instruct))


def expr():
    """parse for expression (term, type, or prop).
    
    Output Etok(expr,...)
    
    >>> expr().process(mk_stream('term')).acc
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
    # earlier implementation.
    #def p(tok):
        # commas can appear in quantified variables
    #    return not(tok.value in [';','.'])
    #return reparse('expr').process(c.balanced_condition(p))
    
def sort_expr():
    return get_lookup_parse('sort_expr')

def colon_sort():
    def f(acc):
        (_,e) = acc
        return Etok.update(e,{'raw':acc})    
    return (colon + sort_expr()).treat(f)

def opt_colon_sort():
    return colon_sort().possibly()

def colon_type():
    """Parse a colon then a post_colon_type.
    
    output Etok
    
    >>> colon_type().process(mk_stream(':post_colon_type')).acc
    Etok(post_colon_type,default,': post_colon_type')
    """
    def f(acc):
        (_,e) = acc
        return Etok.update(e,{'raw':Etok.get_raw(acc)})
    return (colon + get_lookup_parse('post_colon_type')).treat(f)

def opt_colon_type():
    return colon_type().possibly()

def var():
    """parser for a single variable.
    Accepts a single token that is a variable.
    
    >>> var().process(mk_stream('x')).acc
    Etok(VAR,x,'x')
    """
    #def f(acc):
    #    return Etok.etok(acc)
    return c.next_type('VAR').name('VAR').treat(Etok.etok)

def annotated_var():
    """
    Parser for annotated variable in parentheses.  
    Annotation is colon_type or None
    
    Parser output Etok('annotated_var'...)
    etoks:(VAR,colon_type)  
    
    Sample input to parser:
        (x : A)
        
    >>> annotated_var().process(mk_stream('(x:post_colon_type)')).acc
    Etok(annotated_var,'( x : post_colon_type )')
    
    """
    def f(acc):
        (_,(v,ann),_)  = acc
        if not ann:
            return Etok.update(v,{'raw':Etok.get_raw(acc)})
        return Etok('annotated_var',etoks=(v,ann),raw=acc)
 
#        d = {'type':'var',
#             'value':v.value,
#             'raw':lib.flatten(acc)}
##        if ann:
#            d['annotation']=ann[1]
#        return d
    return c.paren(var() + opt_colon_type()).treat(f)

def annotated_vars():
    """
    Parser for annotated list of variables
    Output Etok.etoks:(vars,post_colon_type or None)
    
    Sample input:
        (x y z : A)
        (u v)

    >>> annotated_vars().process(mk_stream('(x y:post_colon_type)')).acc
    Etok(annotated_vars,'( x y : post_colon_type )')
    """
    def f(acc):
        (_,(vs,ann),_) = acc
        return Etok('annotated_vars',etoks=(vs,ann),raw=acc)
    return c.paren(var().plus() + opt_colon_type()).treat(f)

def tvar():
    """
    >>> tvar().process(mk_stream('x')).acc
    Etok(VAR,x,'x')
    
    >>> tvar().process(mk_stream('(x : post_colon_type)')).acc
    Etok(annotated_var,'( x : post_colon_type )')
    """
    #def f(acc):
    #    return Etok.update(acc,{'name':'annotated_var','etoks':(acc,None)})
    return var() | annotated_var()

def assign_expr():
    """parser for := followed by an expression
    The output is the expression at Etok
    
    >>> assign_expr().process(mk_stream(':= general_type')).acc
    Etok(expr,general_type,':= general_type')
    """
    def f(acc):
        (_,e) = acc
        return Etok.update(e,{'raw':Etok.get_raw(acc)})
    return (next_value(':=') + expr()).name('assign_expr').treat(f)

def var_or_atomic():
    """parser for a var or atomic identifier.
    Output of parser is a single token of one of those types."""
    return (var() | atomic()).name('var_or_atomic')

def var_or_atomics():
    """parser for a sequence of one or more var or atomics"""
    return var_or_atomic().plus()

def var_or_atomic_or_blank():
    """parser for var or atomic or _.
    The parser output is a single token that is one of those types."""
    return var_or_atomic() | next_value('_').treat(Etok.etok)

def brace_assign():
    """
    Parser for comma-separated list of assignments within braces.
    The return type is a Etok(brace_assign), one for each item in the list.
    
    Each assignment output is a triple Etok (id,type annotation,assigned expr)
    the last two can be None.
    
    >>> brace_assign().process(mk_stream('{ x := term ; y : post_colon_type := term }')).acc
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
    return c.brace_semi().treat(f_brace).reparse_list(brace_assign_item()).treat(f_final)

def brace_noassign():
    def brace_noassign_item():
        return (var_or_atomics() + opt_colon_type())
    return c.brace_semi().reparse_list(brace_noassign_item())

def hierarchical_identifier():
    """
    Parser for hierarchical identifiers.
    Parser output is a Etok.
    """
    return c.next_type('HIERARCHICAL_IDENTIFIER').treat(Etok.etok)

def identifier():
    """parser for hierarchical or atomic identifier.
    Parser output is a single Etok"""
    return (atomic() | hierarchical_identifier()).expect('identifier')



#renamed map -> call



 
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
    return ((first_word( 'fix let') + c.comma_nonempty_list(annotated_sort_vars)) |
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


            

  
            
            