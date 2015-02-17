'''
Created on Feb 16, 2015

@author: Ruth
'''
'''
from astropy.table import Table, Column
import numpy as np

t = Table()
t['a'] = [1, 4]
t['b'] = Column([2.0, 5.0], unit='cm', description='Velocity')
t['c'] = ['x', 'y']

t = Table(names=('a', 'b', 'c'), dtype=('f4', 'i4', 'S2'))
t.add_row((1, 2.0, 'x'))
t.add_row((4, 5.0, 'y'))

print t



import numpy as np

data = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

np.savetxt('examp_output.txt', data, delimiter = ',')

'''

import pandas as pd

data = pd.DataFrame([ [1, 2, 3], [4, 5, 6], [7, 8, 9]], columns = ['t1', 't2', 't3'])
print data
data.to_csv('examp_output.txt')
