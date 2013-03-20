'''
Created on 2013-03-19

@author: Ruth
'''

import matplotlib.pyplot as plt
import numpy as np

k = 0.1
d_t = 1
B = 0.9

t = np.arange(0, 50, 1)
s = (1- (B * np.exp(-k * t)))


plt.plot(t, s)
#plt.show()



s_old = np.arange(0.1, 1+0.1, 0.1)

#s_new = 1-  B * (np.exp(-k * d_t) * np.power(((1 - s_old)/B),-k))

s_new = 1- ( (1-s_old)*np.exp(-k * d_t) )


plt.plot(s_old, s_new)

#plt.show()

sz=1-B
time= [0]
size = [sz]


for i in range(1,50):
    sz = 1- ( (1-sz)*np.exp(-k * d_t) )
    time.append(i)
    size.append(sz)
    

print time
print size


plt.plot(time, size)
plt.title("last graph")
plt.show()

 

