'''
Created on Mar 28, 2014

@author: Ruth
'''

import matplotlib.pyplot as plt
import numpy as np

from core.CompetitionIntercept import CompInt

numJuvs = 200.0
food = 10.0

compObject = CompInt(food, numJuvs, 1.25) # food, numJuvs, comp input

print "ave food", food/numJuvs


output = compObject.CompIntFun()

slope = output[0]
intercept = output[1]

ranks = np.linspace(0, numJuvs)
food = -slope * ranks + intercept

plt.ylim(0,1.1)

plt.xlabel('individual rank')
plt.ylabel('food to individual')


plt.plot(ranks, food)

plt.show()