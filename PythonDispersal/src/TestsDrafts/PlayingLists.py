'''
Created on 2013-02-10

@author: Ruth
http://www.jesshamrick.com/2012/04/29/the-demise-of-for-loops/
'''

import numpy as np
'''
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

print list[1]
print list[1:5]

'removing item from list remember not to change the list while itterating over it'
'http://docs.python.org/2/tutorial/datastructures.html'

'''
'''
mylist = [1, 2, 4, 5, 3,  6, 7, 8, 9, 10]

mylist.remove(3) # removes the item that is 3, but only the first one!

a=mylist.pop(2)

print mylist

print a

'''

hlist=[1, 2, 3, 4, 5, 6]

print (len(hlist))
print ('word')

for i in range(len(hlist)):
    print "i = %s, len = %s" % (i, hlist[i])

mylist = [1]*5

print hlist

hlist = []
if hlist:
    print "list full"

list = [[1,2,3,4], [5,6,7,8], [9,10,11,12]]

list2 = []

list2.append(list[0])
list2.append(list[1])
list2.append(list[2])

print "list zero"
print list[0]

print len(list2)

for i in range(len(list2)):
    print list2[i]