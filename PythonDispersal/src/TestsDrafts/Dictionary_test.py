'''
Created on Jul 9, 2014

@author: Ruth
'''

instar = {
      0: 0.5,
      1: 1,
      2: 1.5
      }

#print instar[0]
#print instar.keys()
#print instar
#print instar.items()

print [k for k, v in instar.items() if v < 6]

print [k for k in instar if instar[k] < 6]
