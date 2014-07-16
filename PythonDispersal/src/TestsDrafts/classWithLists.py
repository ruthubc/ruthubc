'''
Created on Jul 9, 2014

@author: Ruth
'''

class ListClass(object):
    '''
    classdocs
    '''


    def __init__(self, params):
        '''
        Constructor
        '''
    x=0
fruits = ['aaple', 'orange', 'pear', 'banana']

fruits2=['peach', 'strawberry']

for fruit in fruits:
    x += 1
    fruits2.extend([fruit])  
    
    print fruit
    print x
    print fruits2    