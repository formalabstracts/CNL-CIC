#
from collections.abc import Iterable   

def iterable(obj):
    return isinstance(obj, Iterable)

def flatten(ls):
    """flatten a list one level
    
    >>> flatten([[1,2],[3,4],[5,6]])
    [1, 2, 3, 4, 5, 6]
    """
    return [v for sub in ls for v in sub]

def fflatten(ls):
    """Recursively flatten nested iterables.
    Treat strings as atomic noniterable objects.
    
    >>> fflatten([3,4,5])
    [3, 4, 5]
    
    >>> fflatten([3,['45',6]])
    [3, '45', 6]
    
    >>> fflatten([3,[4,5,[6,7]],8,[9,10],(11,(12,13))])
    [3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]
    """
    def rec_f(vs):
        if not(iterable(vs)) or isinstance(vs,str):
            return [vs]
        return fflatten(vs)
    return flatten([rec_f(v) for v in ls ])


def compress(ls,ind):
    """create sublist from given indices
    
    >>> compress([7,8,9,10],[2,3])
    [9, 10]
    """
    return [ls[i] for i in ind]

def swap(f):
    """reverse the argument order of two arguments.
    
    >>> swap(compress)([2,3],[7,8,9,10])
    [9, 10]
    """
    def g(x,y):
        return f(y,x)
    return g

def curry(f):
    """curry a function of two variables.
    
    >>> curry(compress)([7,8,9,10])([2,3])
    [9, 10]
    """
    def Cf(x):
        def f2(y):
            return f(x,y)
        return f2
    return Cf

def part(ind):
    """
    part(ind) is a function that compresses a list to the set of indices ind.
    
    >>> part([2,3])([7,8,9,10])
    [9, 10]
    """
    return curry(swap(compress)) (ind)

def fst(ls):
    """returns the 0th element of a list"""
    return ls[0]

def snd(ls):
    """returns the 1st element of a list"""
    return ls[1]

def prepend(x):
    """Takes a pair and prepends the first element to the second list"""
    return [fst(x)]+snd(x)

if __name__ == "__main__":
    import doctest
    doctest.testmod()
    



