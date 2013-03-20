'''
Created on 2013-03-19

@author: Ruth
'''

import matplotlib.pyplot as plt
import numpy as np
import random

for i in range(40):
    ran = random.random()
    
    if ran < 0.9:
        x=0
    elif ran >= 0.9:
        x=1
    
    print x



plt.title("mortality")

a = 0.9
mu = 2
ymin = 0.2

m = -np.log(ymin/a)

size = np.arange(0, 1+0.1, 0.1)
prob = a * np.exp(-mu * m * size)

plt.plot(size, prob)
plt.ylim([0,1])

plt.show()