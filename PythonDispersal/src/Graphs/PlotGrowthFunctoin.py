'''
Created on 2013-03-19

@author: Ruth
'''

import matplotlib.pyplot as plt
import numpy as np

k = 0.2 #rate of growth i.e. the amount of food you have to have to grwo
d_t = 1
B = 0.9# 1-b is size at birth

t = np.arange(0, 35, 1)
s = (1- (B * np.exp(-k * t)))


plt.plot(t, s)
"""plt.ylabel('Individual Size')
plt.xlabel('Total amount of food')
plt.show()



s_old = np.arange(0.1, 1+0.1, 0.1)

#s_new = 1-  B * (np.exp(-k * d_t) * np.power(((1 - s_old)/B),-k))

s_new = 1- ( (1-s_old)*np.exp(-k * d_t) )


plt.plot(s_old, s_new)

"""
#plt.show()

sz= 1-B
time= [0]
size = [sz]


for i in np.range(1,50):
    sz = 1- ( (1-sz)*np.exp(-k * d_t) )
    time.append(i)
    size.append(sz)
    

print time
print size


plt.plot(time, size)
plt.title("last graph")
plt.show()


