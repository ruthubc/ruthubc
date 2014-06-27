'''
Created on Jun 27, 2014

@author: Ruth
'''
def decorator1(func):
    return lambda: func() * func()

def decorator2(func):
    def print_func():
        print "hello"
        print func()
    return print_func

@decorator2
@decorator1
def function():
    return 41

function()
# prints '42'