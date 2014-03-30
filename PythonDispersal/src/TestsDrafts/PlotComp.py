'''
Created on Mar 28, 2014

@author: Ruth
'''

import matplotlib.pyplot as plt
import numpy as np
import random
from numpy import vectorize

Fmx = 1 # food rank number 1 gets
Fc = 0.5 # colony food
c = 0.01 # slope i.e. the degree of contest competition (c in document)
N = 50


RMax = ((Fmx -c/2) - np.sqrt(np.square(c)/4 -2 *c*N*Fc + Fmx * (1-3*c)))/ c # the max rank to get food.

print(RMax)

R = np.arange(0, 50, 1) # have to have the ranks starting at zero so the equation works

F = np.where(R <= RMax, -c*R + Fmx, 0 )

print R
print F

plt.ylim(0,1.1)

plt.xlabel('individual rank')
plt.ylabel('food to individual')


plt.plot(R,F)

plt.show()