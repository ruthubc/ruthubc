'''
Created on 2012-12-23

@author: ruth

creating a spider class for each individual with the same method as in test2.
'''


class Spider(object):

    def __init__(self, rank=1, die = 0, food = 0):
        '''defining the spider object'''
        self.rank = rank
        self.die = die  # 0 means not to die, 1 means die!
        self.food = food

    def __str__(self):
        return "rank: %s,  die: %s, food: %s" % (self.rank, self.die, self.food)

    def SpiderList(self):
        return [self.rank, self.die]

    def update_rank(self, rank):  # to update relative rank to x
        self.rank = rank
