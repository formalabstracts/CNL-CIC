#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 17 10:37:32 2021

@author: thales
"""

#exception

from collections import namedtuple

# item on which nonterminal/production_rule fails
ErrorItem = namedtuple('ErrorItem','nonterminal production item')

#add new exceptions to the end of the list

class ParseError(BaseException):
    """Standard parse error. Give data as list of ErrorItem"""
    def __init__(self,error_stack):
        self.error_stack = error_stack

class ParseNoCatch(BaseException):
    """Exception not caught by other parsers"""
    
    def __init__(self,error_stack):
        self.error_stack = error_stack