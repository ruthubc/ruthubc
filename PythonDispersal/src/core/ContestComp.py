'''
Created on Aug 15, 2014

@author: Ruth
'''

import numpy as np
import matplotlib.pyplot as plt
from core.SpiderClass import Spider
from core.ColonyClass import Colony

def find_m(colony): # finding the gradient 

    if colony.colony_food < 0.5:
        m = 1/(2*colony.colony_food*colony.num_spi()-1)
    else :
        m = -(2*(colony.colony_food-1))/ (colony.num_spi() -1) # everyone gets fed if food_tot >=  0.5
    return m
#TODO: put in exception if m is below or zero!

def contest_many_ind(spider, m ): #find the rank for each spider

    food = 1- m * spider.rank

    if food < 0:
        Fd_ind = 0
    else:
        Fd_ind =  round(food, 4)
    
    spider.ind_food = Fd_ind
    
def replacing_min_fd(colony): # replacing the minimum amount to make sure the numbers add up
    minFdInx = colony.food_list().index(min(x for x in colony.food_list() if x != 0)) # gets the index of the last spider to be fed
    minSpiFd = colony.food_list()[minFdInx]
    tot_ind_fd = sum(colony.food_list())
    corrMinAmt = colony.tot_col_fd() - (tot_ind_fd - minSpiFd)
    corrMinAmt = round(corrMinAmt, 4)
    colony.colony_list[minFdInx].ind_food = corrMinAmt


def contest_few_inds(colony):
    minRankInx = colony.rank_list().index(min(colony.rank_list()))
    print "contest few inds:",
    print colony.tot_col_fd()
    colony.colony_list[minRankInx].ind_food = colony.tot_col_fd()
    
        
def contest(colony):
    if colony.tot_col_fd() < 1.0:
        print "few inds"
        contest_few_inds(colony)
    else:
        m = find_m(colony)
        print "lots inds m:",
        print m
        [contest_many_ind(i, m) for i in col.colony_list]
        replacing_min_fd(colony)
        
        

# making colony  
    
jon = Spider(rank = 2)
peter = Spider(rank = 1)
sue = Spider(rank = 0)
brad = Spider(rank = 3)
ir= Spider(rank = 4)
u = Spider(rank = 5)


col = Colony([jon, peter, sue, ir, u, brad], 4, colony_food = 0.6)
print "tot col food:",
print col.tot_col_fd()
contest(col)

print col.food_list()


tot_ind_fd = sum(col.food_list())
tot_col_fd = col.colony_food * col.num_spi()

print 'sum of ind food:',
print tot_ind_fd
print "colony food:",
print tot_col_fd

col.print_spiders()
'''

# find minimum value of food

minFdInx = col.food_list().index(min(x for x in col.food_list() if x != 0)) # gets the index of the last spider to be fed

minSpiFd = col.food_list()[minFdInx]

print minFdInx
print minSpiFd

print col.colony_list[minFdInx]

corrMinAmt = tot_col_fd - (tot_ind_fd - minSpiFd)

print corrMinAmt

col.colony_list[minFdInx].ind_food = corrMinAmt

col.print_spiders()

tot_ind_fd = sum(col.food_list())
tot_col_fd = col.colony_food * col.num_spi()

print 'sum of ind food:',
print tot_ind_fd
print "colony food:",
print tot_col_fd





#TODO sum all the indfood, find the smallest food and replace it with the calculated value
#  lists keep their order so find the index of the list with the min value and replace in the colony list using 
# to index number! 



    

contest_few_inds(col)


    

def contest_test(col_food, num_spiders):  # question? how to organize this with instars -> prob needs to be run seperatly for each instar
    tot_food = col_food * num_spiders
    if tot_food <= 1:
        contest_few_inds()# highest ranked ind gets one
    else:
        #contest_lots_inds()
        pass
'''
    