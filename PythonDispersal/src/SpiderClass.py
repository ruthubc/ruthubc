'''
Created on 2012-12-23

@author: ruth

creating a spider class for each individual with the same method as in test2.
'''
#pylint: disable=C0111


class Spider(object):

    def __init__(self, instar, size, rel_size):
        '''defining the spider object'''
        self.instar = instar
        self.size = size
        self.rel_size = rel_size

    def __str__(self):
        return "instar: %s, size: %s, rel size: %s" % (self.instar, self.size, self.rel_size)
    
        
    def instar_inc(self, instar_levels_list):
        '''updates the instars of individuals after feeding
        instar_levels_list defines the size limits to progress instars'''

        for k in range(len(instar_levels_list)):

            if (self.size >= instar_levels_list[k] and
             self.size < instar_levels_list[k + 1]):
                self.instar = k
                
    def addInstar(self):
        self.instar += 1

