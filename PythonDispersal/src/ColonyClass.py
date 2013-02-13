'''
Created on 2013-02-12

@author: ruth
'''


from SpiderClass import Spider

class Colony(Spider): 

    def __init__(self, list1):
        self.list1= list1;
        
    def pword (self):
        for i in range(len(self.list1)):
            print self.list1[i]
            
    def allinstar(self):
        for i in range(len(self.list1)):
            self.list1[i]
        
      
   
 

j=Colony ([Spider(3, 6, 5), Spider(1, 0, 5)])

print j.list1[1]

j.pword()

spi=Spider(3, 6, 5)

print spi

spi.addInstar()

print spi

