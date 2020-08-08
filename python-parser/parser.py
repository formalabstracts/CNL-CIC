# parser combinators 

import lexer
import lib 
import copy

# input stream should have next:stream -> (result,stream)

class ParseError(Exception):
    """Standard parse error"""
    pass

class ParseNoCatch(Exception):
    """Exception not caught by other parsers"""
    
    def __init__(self,msg=''):
        self.msg = msg
        
def can_eval(f,x):
    try:
        f(x)
        return True
    except ParseError:
        return False
    
def raise_false(b):
    if not(b):
        raise ParseError
    return b

class ParseCell:
    """base class for parsed data"""
    
    def __init__(self,toks,cells,name,celltyp):
        self.toks = toks
        self.cells = cells
        self.name = name 
        self.celltyp = celltyp
        
    def start_index(self):
        lexer.token_len(self.toks[0])
        
    def end_index(self):
        t = self.toks[-1]
        lexer.token_len(t)+t.lexpos 
        
class Parse:
    """base class for parsers"""
    def __init__(self,repr,r):
        self.read = r
        self.repr = repr 
        
    def __repr__(self):
        """Description of production rule"""
        return 'Parse(%s)' % self.repr 
    
    def set_repr(self,rep):
        self.repr = rep
        
    def next_token(): # constructor for next token
        return Parse('next_token',next)
        
    def __call__(self,input):
        self.read(input)
    
    def __add__(self,other):
        """combine two parsers in succession"""
        def f(input):
            (result,input1) = self(input)
            (result2,input2) = other(input1)
            return ((result,result2),input2)
        return Parse(self.repr()+'+'+other.repr(),f)

    def __or__(self,other):
        """try first parser then next"""
        def f(input):
            try:
                return self(input)
            except ParseError:
                return other(input)
            
        return Parse(self.repr()+'|'+other.repr(),f)
    
    def __rshift__(self,treatment):
        """apply treatment to parser output"""
        def f(input):
            (result,input1) = self(input)
            return (treatment(result),input1)
        
    def many(self):
        """parse zero or more times"""
        def f(input):
            try:
                result,input1 = self(input)
                results,input2 = many(self)(input1)
                return ([result]+results,input2)
            except ParseError:
                return ([],input)
        return Parse('many(%s)' % self.repr(),f)
    
    def fix(self,msg):
        """No catch error if failure"""
        def f(input):
            try:
                return self(input)
            except ParseError:
                raise ParseNoCatch(msg)
        return Parse(self.repr(),f)
    
    def separated_nonempty_list(self,sep):
        """Sequence of at least one parse with separation sep"""
        def treat(l,ls):
            [l]+ls
        return (self + many(sep + self >> snd)) >> treat
            
    def nothing():
        """Does no parsing, empty list as output"""
        def f(input):
            return ([],input)
        return Parse("nothing",f)
    
    def separated_list(self,sep):
        """sequence of parses with separation sep"""
        return (separated_nonempty_list(self,sep) | nothing()).set_repr("sep list")
                
    def possibly(self):
        """zero or one parses returned in a list"""
        def f(input):
            try:    
                result,input1 = self(input)
                return [x],input1
            except ParseError:
                return [],input
        return Parse('possibly(%s)' % self.repr(),f)
    
    def if_test(self,p): #was some
        """Next token passes boolean test or fail"""
        def f(input):
            (result,input1) = self(input)
            if p(result):
                return (result,input1)
            else:
                raise ParseError
        return Parse('if_test',f)
    
    def if_test_treat(self,p): #was someX
        """Next passes test and evaluates or fail"""
        def f(input):
            (result,input1) = self(input)
            b,treat = p(result)
            if b:
                return treat,input1
            else:
                raise ParseError
        return Parse('if_test_delay',f)
    
    def atleast(self,n):
        """parse at least n times"""
        def f(input):
            if n < 1:
                self.many()
            else:
                self + atleast(self,n-1) >> treat
        return Parse('at least {n}'.format(n=n),f)
    
    def plus(self):
        """parse at least once"""
        return self.atleast(1)
    
    def compose(self,other): #was dependent plus
        """compose parsers"""
        def f(input):
            (result,input1) = self(input)
            return other(result,input1)
        return Parse('compose',f)
    
    def if_next_value(v): #was a
        """parse if next token has value v or fail"""
        def p(tok):
            return tok.value == v
        return next_token().if_test(p)
    
    def if_next_type(ts): 
        """parse if next type is in ts or fail"""
        def p(tok):
            return tok.type in ts
        return next_token().if_test(p)
    
    def parse_all(prs):
        """sequentially parse a list of parsers and return list of results"""
        def f(input):
            if len(prs) == 0:
                return ([],input)
            else:
                (result,input1) = prs[0](input)
                (result2,input2) = parse_all(prs[1:],input1)
                return [result]+result2,input2
        return Parse('parse_all',f)
    
    def parse_first(prs): #was parse_some 
        """parse first in a list that does not fail"""
        def f(input):
            if len(prs) == 0:
                raise ParseError
            else:
                try: 
                    return prs[0](input)
                except:
                    return parse_first(prs[1:])(input)
        return Parse('parse_first',f)
    
def wordify(tok):
    # need to (shallow) clone because of backtracking.
    clone = copy.copy(tok)
    clone.type = 'WORD'
    clone.value = (tok.value,tok.value.lower())
    return clone
    
def word_parse(s):
    """parser constructor that matches word string s"""
    def p(tok):
        if tok.type == 'WORD':
            return (True,tok)
        # convert single character variables to words
        elif tok.type == 'VAR' and len(tok.value)==1 and tok.value.isalpha():
            return (True,wordify(tok))
        else:
            return (False,None)
    return if_test(p).set_repr('word(%s)' % s)

# synonym handling uses a global dictionary.

synonym = {}

def synonym_add(ts):
    keys = synonym.keys
    for s in ts:
        if singularize(s) in keys:
            return msg_error('synonym already declared: %s' % s)
            return
        if len(s) < 2:
            return msg_errror('synonyms must have at least 2 chars: %s',s)
    ls = sort([singularize(s) for s in ts])
    js = ls.join(' ')
    for s in ls:
        synonym[s] = js

def synonymize(s):
    lower = s.lower()
    synonym.get(lower,default=[lower])


        
            
                                  
    
    
                
    
    
            
            