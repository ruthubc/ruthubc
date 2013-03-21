'''
Created on 2013-03-20

@author: Ruth
'''

from SpiderClass import Spider
import numpy as np
'''
temperature = float(input('What is the temperature? '))
if temperature > 70:
    print('Wear shorts.')
else:
    print('Wear long pants.')
print('Get some exercise outside.')


x = []
y = [Spider(1), Spider(0), Spider(1), Spider(1), Spider(1), Spider(0)]
     
     
for i in range(len(y)):
    
    if y[i].instar == 0:
        #x.insert(0, i)
        x.append(i)
        
print x

y = [i for j, i in enumerate(y) if j not in x]

for i, item in enumerate(y):
    print y[i]



print y

'''

IN_INSTAR = np.arange(0.1, 1, 0.1)

print IN_INSTAR