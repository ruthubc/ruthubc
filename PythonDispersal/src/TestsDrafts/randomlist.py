'''
Created on Apr 4, 2015

@author: user
'''
import random

foo = ['a', 'b', 'c', 'd', 'e']

print(random.choice(foo))

s=range(1,6)
print 's=', s

while len(s)>3:
  s.remove(random.choice(list(s)))
  print(s)