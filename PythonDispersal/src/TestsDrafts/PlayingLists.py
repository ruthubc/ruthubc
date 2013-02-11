'''
Created on 2013-02-10

@author: Ruth
http://www.jesshamrick.com/2012/04/29/the-demise-of-for-loops/
'''

import numpy as np

list = [1,2,4,7,8]

list2 = [i*2 for i in list]

#list2 = np.multiply(1, list)

#list3 = np.multiply(10, list2)

total1= np.sum(list)

print total1

print list
print list2

total = np.sum(list2)

print total
