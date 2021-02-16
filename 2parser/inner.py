import tokenlib, lib, inner_lexer
from parser_combinator import Parse
from parser_combinator import (next_type,next_value,
                               LazyParse,getvalue)


def mk_inner_stream(s):
    """This function is primarily for debugging.
    It creates an initialized item-stream from a string,
    which is tokenized with the inner lexer.
    """
    inner_lexer.tokenizer.input(s)
    it = tokenlib.init_item(list(inner_lexer.tokenizer))
    return it


class Inner():
    """parsing of inner tokens.
    Inner parsing is used for parser generation, based on string specs.
    
    >>> st = "a/b th:\the$"
    >>> inner_lexer.tokenizer.input(st)
    
    """
    
    def escape():
        def f(acc):
            return ('ESCAPED',getvalue(acc))
        return next_type('ESCAPED').treat(f).name('escaped')
    
    def label():
        return next_type('LABEL')
    
    def dot():
        def f(acc):
            return ('DOT','.')
        return next_type('.').treat(f).name('dot')
        
    def id():
        def f(acc):
            return (acc.type,acc.value)
        return Parse.next_token().if_types(['TY','ID']).treat(f)
    
    def bracket():
        def f(acc):
            ((_,a),_) = acc
            return a
        return (next_type('[') + Inner.annotated().plus() + next_type(']')).treat(f).name('bracket')
    
    def opt():
        def f(acc):
            ((_,a),_) = acc
            return ('OPT',a)
        return (next_type('(') + Inner.annotated().plus(next_value('|')) + next_type(')')).treat(f).name('opt')
    
    def ending():
        def f(acc):
            l = {}
            (a,b) = acc
            if a:
                l['bracket']=a
            if b:
                l['rep']=b.value
            return l
        return (Inner.bracket().possibly() + ((next_type('*') | next_type('?')).possibly())).treat(f).name('ending')
    
    def parser():
        return ((Inner.id() | Inner.dot() | Inner.escape() | Inner.opt())).name('parser')

    def annotated():
        """This parser must be lazy to delay infinite recursion"""
        def f(acc):
            ((a,b),c) = acc
            d= {'parser':b}
            if a:
                d['label'] = getvalue(a)
            d.update(c)
            return d
        return LazyParse(lambda p: (Inner.label().possibly() + p()  + Inner.ending()).treat(f),Inner.parser,'annotated')
    
    def nontermprod():
        return (Inner.id() + next_type('/')).treat(lib.fst) + Inner.id()
         
    def top_level():
        def f(acc):
            (((a,b),c),p) = acc
            return {'finished':bool(p),'nonterminal':a,'production':b,'annotated':c}
        return (Inner.nontermprod() + Inner.annotated().plus() + next_value('$').possibly()).treat(f)
    
def debug_lazyparse2():
    r"""This procedure is for debugging and testing only. 
    
    >>> Inner.nontermprod().process(mk_inner_stream("a/b")).acc
    (('ID', 'a'), ('ID', 'b'))
    
    >>> Inner.escape().process(mk_inner_stream(r'\abc. d')).acc
    ('ESCAPED', 'abc')
    
    >>> Inner.label().process(mk_inner_stream(r"abc:\ d")).acc
    Illegal inner lexer character '\'
    LexToken(LABEL,'abc',1,0)
    
    >>> Inner.dot().process(mk_inner_stream(r"...")).acc
    ('DOT', '.')
    
    >>> Inner.id().process(mk_inner_stream(r"abc_d efg")).acc
    ('ID', 'abc_d')
    
    >>> Inner.id().process(mk_inner_stream(r"ABC_ efg")).acc
    ('TY', 'ABC_')
    
    >>> (Inner.id() + Inner.label()).process(mk_inner_stream(r"AB cd:")).acc
    (('TY', 'AB'), LexToken(LABEL,'cd',1,3))
    
    >>> next_type('*').process(mk_inner_stream(r"*")).acc
    LexToken(*,'*',1,0)
    
    #>>> Inner.ending().process(mk_inner_stream(r"?")).acc
    #{'rep': '?'}
    
    >>> Inner.annotated().process(mk_inner_stream(r"rgb:ochre")).acc
    {'parser': ('ID', 'ochre'), 'label': 'rgb'}
    
    >>> Inner.annotated().plus().process(mk_inner_stream(r"color:red* part:leg? separate \one . ")).acc
    [{'parser': ('ID', 'red'), 'label': 'color', 'rep': '*'}, ... 
     
    >>> Inner.bracket().process(mk_inner_stream(r"[a \b]")).acc
    [{'parser': ('ID', 'a')}, {'parser': ('ESCAPED', 'b')}]
    
    >>> Inner.top_level().process(mk_inner_stream(r"term/prod a:b?  the . \red (c | d [ts:t] | ee:e)* ")).acc
    {'finished': False, ... {'parser': ('OPT', ... 'rep': '*'}]}
    """

class OuterParser(Parse):
    """parsing of outer tokens"""
    pass
