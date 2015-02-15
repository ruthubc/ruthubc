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

a = 0.1 #risk of death at size zero
mu = 1
ymin = 0.005 # risk of death at size 1

m = -np.log(ymin/a)

size = np.arange(0, 1+0.01, 0.1)
prob = a * np.exp(-mu * m * size)

plt.plot(size, prob)
plt.ylim([0,0.12])
plt.ylabel('Prob of death per time round')
plt.xlabel('Individual size')

plt.show()