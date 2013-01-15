'''
Created on 2012-12-17

@author: ruth
'''
print 'hello world'

friends = ['john', 'pat', 'gary', 'michael']
for i, name in enumerate(friends):
    print "iteration {iteration} is {name}".format(iteration=i, name=name)
    
def greet(name):
    print 'Hello', name
greet('Jack')
greet('Jill')
greet('Bob')    

import math

print(0.6/0.2)

print (round(3.14159265,2))

print math.exp(5)

collection = ['hey', 5, 'd']
for peter in collection:
    print peter


testInt=4

testInt=testInt+4

print(testInt)

#playing with lists to see how they work

playList = [1, 2, 5, 7]

print (playList[0]) #displays the first element