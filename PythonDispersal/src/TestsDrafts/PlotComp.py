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
comp = 0.01 # slope i.e. the degree of contest competition

RMax = (2 * (Fc +Fmx))/comp # the max rank to get food

print(RMax)

R = np.arange(0, 50, 1) # have to have the ranks starting at zero so the equation works

F = np.where(R <= RMax, -comp*R + Fmx, 0 )

print R
print F

plt.ylim(0,1)

plt.plot(R,F)

plt.show()