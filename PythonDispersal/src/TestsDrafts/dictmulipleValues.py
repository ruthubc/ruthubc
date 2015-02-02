'''
Created on Jul 16, 2014

@author: Ruth
'''
'''multiple values in one dictionary'''

from collections import defaultdict
import json

d= defaultdict(list)
d['a'].append(1)
d['b'].append(2)
d['a'].append(3)
d['b'].append(4)


print d

d['a'].append(5)
d['b'].append(6)

print d

print d.values()

#json.dump(d, open('defltdict.csv','w'))