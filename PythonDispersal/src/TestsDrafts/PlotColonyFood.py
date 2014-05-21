'''
Created on 2013-03-19

@author: Ruth
'''
import matplotlib.pyplot as plt
import numpy as np


plt.ylabel('Total amount of food in the colony')
plt.xlabel('Group size')

c=0.01 # a number that has something to do with optimum group size
d=2 #level of skewdness

print "d/c which is the optimum gp size =", (d/c)


mxFd = np.exp(-d) * np.power((d/c), d) # used to scale the equation to make max food = 1

print "max food = ", mxFd

x = np.arange(0, 10*(1/c), 1)
#y = np.exp(d-c*x) *np.power((c*x/d), d)
y = (1/mxFd) * np.power(x, d) * np.exp(-c*x)

plt.plot(x, y)

plt.show()
