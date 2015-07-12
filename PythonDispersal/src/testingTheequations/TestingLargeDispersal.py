'''
Created on Jul 11, 2015

@author: user
'''
from core.AdultClass import Adult
import numpy as np
import csv



foodRange = np.arange(0.3, 1, 0.00006)

print len(foodRange)

print foodRange[456]

adList = []

for i in foodRange:
    ad = Adult([1,0,i], 0, 0)
    adList.extend([ad])
    
print adList[356]

[i.disperseChoice(0.8) for i in adList]



ofile  = open('dipersalChoiceTest.csv', "wb")
writer = csv.writer(ofile, delimiter=',', quotechar='"', quoting=csv.QUOTE_ALL)

for ad in adList:
    writer.writerow([ad.food, ad.disperse])


ofile.close()




    