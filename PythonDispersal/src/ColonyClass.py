'''
Created on 2013-02-12

@author: ruth
'''


class Colony(object):
    '''making colony class'''

    def __init__(self, colony_list, total_size=0.0, total_instar=0.0, ):
        self.colony_list = colony_list
        self.total_size = total_size
        self.total_instar = total_instar

    def print_spiders(self):
        '''prints all instances of spider'''
        for i in range(len(self.colony_list)):
            print "i = %s: %s" % (i, self.colony_list[i])

    def all_instar(self, instar_levels):
        "updating all instars in spider"
        for i in range(len(self.colony_list)):
            self.colony_list[i].instar_inc(instar_levels)

    def col_size(self):
        '''calculates the total size of all members in a colony'''
        self.total_size = 0.0
        for i in range(len(self.colony_list)):
            self.total_size += self.colony_list[i].size
        return self.total_size

    def rel_size(self):
        '''calculates the relative size of ind compared to colony
        size varies between 0 and 1 to determine prop of food to ind'''
        self.col_size()  # getting total colony size
        for i in range(len(self.colony_list)):
            self.colony_list[i].rel_size = self.colony_list[i].size / self.total_size

    def instar_sum(self):
        '''calculates the total instar in the colony'''
        for i in range(len(self.colony_list)):
            self.total_instar += self.colony_list[i].instar
        return self.total_instar
