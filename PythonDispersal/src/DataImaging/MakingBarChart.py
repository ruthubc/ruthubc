'''
Created on Jan 22, 2016

@author: user
'''


import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


data=pd.read_csv("AnimationData.csv", sep = ',')

print data

print data.head(5)

print data.describe()

newdata = data[data.pop_age == 1]

print "new data"

#newdata = newdata[['colony_ID', 'num_adsB4_dispersal']]


#newdata = newdata.transpose()
#print newdata.ix[5,2] #http://stackoverflow.com/questions/20838395/what-is-the-point-of-ix-indexing-for-pandas-series

print len(newdata.colony_ID.values)

#newdata.plot(kind='bar')#; plt.axhline(0, color='k')

#fig = plt.figure()
#ax = plt.subplot(111)

#ax.bar(newdata[['colony_ID']], newdata[['num_adsB4_dispersal']] )
'''
plt.figure(figsize=(20,10)) # changes the size of the figure

plt.bar(newdata.colony_ID.values, newdata.num_adsB4_dispersal.values)

plt.xlim(1,200)
plt.subplots_adjust(left=0.03, right=0.99, top=0.99, bottom=0.03)

plt.show()

'''
