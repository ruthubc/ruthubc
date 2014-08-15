'''
Created on Aug 15, 2014

@author: Ruth
'''

import numpy as np
import matplotlib.pyplot as plt


def contest_lots_inds(food_tot, num_spi, spi_rank ):

    if food_tot < 0.5:
        m = 1/(2*food_tot*num_spi-1)
    else :
        m = -(2*(food_tot-1))/ (num_spi -1) # everyone gets fed eqn if food_tot >=  0.5
    Fd_ind = 1- m * spi_rank
    
    if Fd_ind < 0:
        return 0
    else:
        return Fd_ind
    
    

print contest_lots_inds(0.4, 100, 96)  



def contest_few_inds():