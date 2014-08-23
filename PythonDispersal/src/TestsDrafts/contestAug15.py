'''
Created on Aug 15, 2014

@author: Ruth
'''

import numpy as np
import matplotlib.pyplot as plt

Ranks = np.array([6, 1, 0, 4, 3, 5, 2]) # has to be an array

food_tot = 0.41
num_spi = len(Ranks) # when having comp only between instars this is the total number of instars


if food_tot < 0.5:
    m = 1/(2*food_tot*num_spi-1)
else :
    m = -(2*(food_tot-1))/ (num_spi -1) # everyone gets fed eqn if food_tot >=  0.5


print "m max:",
print m





#np.arange(0, (num_spi), 1) # remember that it goes up to number -1
print Ranks

Fd_ind = 1- m * Ranks

Fd_ind = [i if i >= 0 else 0 for i in Fd_ind] # removing minus numbers

Fd_ind = [round(i, 4) for i in Fd_ind] # rounds the decimial places

minSpiInx = Fd_ind.index(min(x for x in Fd_ind if x != 0)) # gets the index of the last spider to be fed

minSpiFd = Fd_ind[minSpiInx]

totColFd = num_spi * food_tot


minFd = round(totColFd - (sum(Fd_ind) - minSpiFd), 4)


Fd_ind[minSpiInx] = minFd

print 'index of min food:',
print minSpiFd

print "food list:",
print Fd_ind





print "sum of ind food, should equal:",
print totColFd
print "actual sum:",
print  sum(Fd_ind)

plt.ylim(-0.1,1.1)

plt.plot(Ranks, Fd_ind, marker = 'o')
plt.ylabel('food amount for each individual')
plt.xlabel('Individual Rank')

plt.show()