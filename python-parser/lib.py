#
from collections.abc import Iterable   

def iterable(obj):
    return isinstance(obj, Iterable)

def flatten(ls):
    """flatten a list one level"""
    return [v for sub in ls for v in sub]

#sum([[1,2],[3,4],[5,6]])??
    
#print(flatten([[1,2],[3,4],[5,6]]))

def fflatten(ls):
    """flatten recursive nested lists"""
    def rec_f(vs):
        if not(iterable(vs)):
            return [vs]
        return fflatten(vs)
    return flatten([rec_f(v) for v in ls ])
    
#print(fflatten([3,4,5]))
#print(fflatten([3,[4,5,[6,7]],8,[9,10],(11,(12,13))]))

def compress(ls,ind):
    """create sublist from given indices"""
    return [ls[i] for i in ind]

def swap(f):
    """reverse the argument order if two arguments"""
    def g(x,y):
        return f(y,x)
    return g

def curry(f):
    """curry a function of two variables"""
    def Cf(x):
        def f2(y):
            return f(x,y)
        return f2
    return Cf

def part(ind):
    return curry(swap(compress)) (ind)

def fst(ls):
    return ls[0]

def snd(ls):
    return ls[1]

def prepend(x):
    return [fst(x)]+snd(x)
    



