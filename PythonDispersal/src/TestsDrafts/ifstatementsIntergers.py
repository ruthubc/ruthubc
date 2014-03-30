'''
Created on Mar 28, 2014

@author: Ruth
'''
import numpy as np

Fmx = 1 # food rank number 1 gets
Fc = 0.5 # colony food
c = 0.01 # slope i.e. the degree of contest competition (c in document)
N = 50

#y = (Fmx - c/2) + (np.sqrt(np.square(c/2-Fmx)-2*c*(N*Fc-Fmx))) / c

y = (np.sqrt(np.square(c) + (4*c*Fmx) - (8*c*Fc*N) + (4*np.square(Fmx))))

z = -c + 2*Fmx


print y
print z
print y+z

