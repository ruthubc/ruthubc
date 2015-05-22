'''
Created on May 21, 2015

@author: Ruth
'''

#TODO: think about the range of adult sizes? Does it really matter?

from AdultClass import Adult
from JuvClass import Juv
from SpiderClass import Spider
from ColonyClass import Colony

K = 100

# making colonies
# adult [rank, die, food], disp, num off

#start_col1 = Colony(1, [Adult([1, 0, 0.8], 0, 0)], slope = comp_slp)

# make list of adults for colony

#runs = [[0, 0.8, 2.5, 10], [0.05], [100], [0]] # slope, risk of dispersal, MeanK , Var k
#combinations = list(itertools.product(*runs))

sze_min = 1
sze_max = K * 2
intvl = sze_max / 10


sze_rng_lst =  range(intvl, (sze_max+intvl) ,intvl)

print [1] + sze_rng_lst

print len(sze_rng_lst)


colSize = 10

def make_col(colSize, ad_food, col_number, comp_slp):
    
    ad_list = []

    for i in range(0, colSize):
        ad_list.append(Adult([1, 0, ad_food], 0, 0))
    
    col = Colony(col_number, ad_list, slope = comp_slp)
    return col

def make_col_list(colSize, ad_food, col_number, comp_slp):

print make_col(10, 0.5, 1, 0.5)


