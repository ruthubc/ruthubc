'''
Created on Jan 25, 2016

@author: Ruth
'''

import pandas as pd
import UpdateColSpace
import numpy as np

data = pd.read_csv("AnimationDataInput.csv", sep = ',') # import data

print data.head(5)

data = data[['pop_age', 'colony_ID', 'num_adsB4_dispersal', 'dispersers', 'colAlive']]

data['dispersers'] = np.where(data['dispersers'] > 0 , 'D', 'N')

data['state'] = data['dispersers'] + data['colAlive']

'''
                          np.where((data['dispersers'] > 0) & (data['colAlive'] == 'alive'), 'DS',
                                    np.where((data['dispersers'] == 0) & (data['colAlive'] == 'dead'), 'DE',
                                             'A')))
'''
print 'updating colony space'
UpdateColSpace.main(data)  ## updates the colony space

print data.head(5)

data.to_csv('AnimationDataOutput.csv', sep = ',', index = False)

print 'File exported'
