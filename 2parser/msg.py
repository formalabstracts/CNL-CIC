def error(s):
    print('Error:'+s)
    
def setdefault(s,default):
    """ 
    >>> setdefault('message','default')
    'message'
    
    >>> setdefault('','default')
    'default'
    
    >>> setdefault(None,'default')
    'default'
    """
    if s:
        return s
    else:
        return default
