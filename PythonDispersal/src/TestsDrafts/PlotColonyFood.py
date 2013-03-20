'''
Created on 2013-03-19

@author: Ruth
'''
import matplotlib.pyplot as plt
import numpy as np



plt.ylabel('some numbers')

c=0.2
d=10


x = np.arange(0, 30*(1/c), 1)
y = np.exp(d-c*x) *np.power((c*x/d), d)

plt.plot(x, y)

plt.show()
