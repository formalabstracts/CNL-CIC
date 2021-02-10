#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 17 10:37:32 2021

@author: thales
"""

#exception

from collections import namedtuple

# item on which nonterminal/production_rule fails
ErrorItem = namedtuple('ErrorItem','item nonterminal')

#add new exceptions to the end of the list

class ParseError(BaseException):
    """Standard parse error. Give data ErrorItem.
    Chain errors using the 'from' Python keyword 
    """
    def __init__(self,error_stack:ErrorItem):
        self.error_stack = error_stack

class ParseNoCatch(BaseException):
    """Exception not caught by other parsers"""
    
    def __init__(self,error_stack:ErrorItem):
        self.error_stack = error_stack
        
class DataProcess(BaseException):
    """Exception raised during post-parsing processing"""
    
    def __init(self,data):
        self.data = data