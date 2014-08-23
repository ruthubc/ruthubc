'''
Created on Aug 15, 2014

@author: Ruth
'''

import numpy as np
import matplotlib.pyplot as plt
from core.SpiderClass import Spider
from core.ColonyClass import Colony

def find_m(food_tot, num_spi): # finding the gradient 

    if food_tot < 0.5:
        m = 1/(2*food_tot*num_spi-1)
    else :
        m = -(2*(food_tot-1))/ (num_spi -1) # everyone gets fed eqn if food_tot >=  0.5
    return m



def contest(spider, m ): #find the rank for each spider

    food = 1- m * spider.rank

    if food < 0:
        Fd_ind = 0
    else:
        Fd_ind =  round(food, 4)
        
    print Fd_ind
    spider.ind_food = Fd_ind
        
    
    
jon = Spider(rank = 2)
print jon

m = find_m(0.6, 7)

contest(jon, m )

print jon

col = Colony([jon, jon], 4)

print col.food_list()

#TODO sum all the indfood, find the smallest food and replace it with the calculated value
#  lists keep their order so find the index of the list with the min value and replace in the colony list using 
# to index number! 


def contest_few_inds():
    pass
    


'''

    

def contest_test(col_food, num_spiders):  # question? how to organize this with instars -> prob needs to be run seperatly for each instar
    tot_food = col_food * num_spiders
    if tot_food <= 1:
        contest_few_inds()# highest ranked ind gets one
    else:
        #contest_lots_inds()
        pass
        
  '''      
    
    