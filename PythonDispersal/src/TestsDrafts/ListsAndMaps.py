'''
Created on Jul 14, 2014

@author: Ruth
'''

mylist = []

newlist = [x*2 for x in mylist]

#map(lambda x: x*2, mylist)

mylist.append([200, 259])
mylist.append([2000, 2519])

print 'updatedList', mylist;

alist = range(0,10)

print alist