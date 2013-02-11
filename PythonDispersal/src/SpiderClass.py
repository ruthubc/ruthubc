'''
Created on 2012-12-23

@author: ruth

creating a spider class for each individual with the same method as in test2.
'''
#pylint: disable=C0111


class Spider(object):

    def __init__(self, instar, size, rel_size):
        self.instar = instar
        self.size = size
        self.rel_size = rel_size

