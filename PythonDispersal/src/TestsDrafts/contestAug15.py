'''
Created on Aug 15, 2014

@author: Ruth
'''

import numpy as np
import matplotlib.pyplot as plt

food_tot = 0.3
num_spi = 10 # when having comp only between instars this is the total number of instars

if food_tot < 0.5:
    m = 1/(2*food_tot*num_spi-1)
else :
    m = -(2*(food_tot-1))/ (num_spi -1) # everyone gets fed eqn if food_tot >=  0.5


print "m max:",
print m




Ranks = np.arange(0, (num_spi), 1) # remember that it goes up to number -1

Fd_ind = 1- m * Ranks

print "food list:",
print Fd_ind


#Fd_ind = [i if i >= 0 else 0 for i in Fd_ind]


print "sum of ind food, should equal:",
print num_spi * food_tot
print "actual sum:",
print sum(Fd_ind)

plt.ylim(-1.1,1.1)

plt.plot(Ranks, Fd_ind, marker = 'o')

plt.show()