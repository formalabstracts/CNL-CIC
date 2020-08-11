#

def flatten(ls):
    """flatten a list one level"""
    return [v for sub in ls for v in sub]
    
#print(flatten([[1,2],[3,4],[5,6]]))

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

#print(part([3,4,5])([0,1,2,7,8,9,6,7]))

#print(swap(compress)([0,1],[7,8,2,3,4,5]))

#print(fst((3,4)))
#print(snd((3,4)))
