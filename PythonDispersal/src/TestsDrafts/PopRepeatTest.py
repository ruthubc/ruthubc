'''
Created on Jul 21, 2014

@author: user
'''
class test(object):
    
    def __init__(self, x):
        self.x = x
        
        
    def add(self):
        self.x = self.x + 2
        



z = test(5)


for i in range(0, 5):
    print i, 
    print z.x
    z.add()
    print "updated", 
    print z.x