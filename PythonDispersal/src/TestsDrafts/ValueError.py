'''
Created on Mar 11, 2015

@author: Ruth
'''

def add(number):
    #print number
    if number < 0:
        raise ValueError("number negative")
    else:
        print number + 5
    print "not exception"
    print number - 10
    
add(-10)