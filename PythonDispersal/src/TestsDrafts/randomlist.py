'''
Created on Apr 4, 2015

@author: user
'''
'''
import random

foo = ['a', 'b', 'c', 'd', 'e']

print(random.choice(foo))

s=range(1,6)
print 's=', s

while len(s)>3:
  s.remove(random.choice(list(s)))
  print(s)
'''  
  
from core.Functions import random_gus
import matplotlib.pyplot as plt
import numpy as np

mylist = []

for i in range (0, 50000):
    mylist = mylist + [random_gus(0.5, 0.3)]

#n, bins, patches = plt.hist(mylist, 1, normed=1, facecolor='g', alpha=0.75)

plt.hist(mylist, 100)
plt.xlim([0, 1])

plt.show()