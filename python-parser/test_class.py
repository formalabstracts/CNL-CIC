#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 25 08:10:26 2020

@author: radix
"""

# test of class scope

class Test:
    def f():
        print('hello')
        pass
    
    def g():
        Test.f()
        pass
        
Test.g()