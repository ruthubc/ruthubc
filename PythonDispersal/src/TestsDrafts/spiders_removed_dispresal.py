'''
Created on May 20, 2015

@author: Ruth
'''
import random

disList = [1,2,3,4,5,6,7,8,9,10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21]

num_list = len(disList) * 0.1
print num_list
while len(disList) > num_list:
    disList.remove(random.choice(list(disList)))
                   
print disList