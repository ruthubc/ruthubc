'''
Created on Mar 12, 2015

@author: user
'''
from core.PopulationClass import Poplt
from core.ColonyClass import Colony
from core.AdultClass import Adult
import numpy as np
import matplotlib.pyplot as plt

lala = np.arange(0.2, 1.1, 0.01) # food

print lala
ad_list = []


for fd in lala:
    print fd
    ad = Adult([0.1, 0, fd], 0, 0)
    ad_list.extend([ad])


col = Colony(1, ad_list)
pplt = Poplt([col])

pplt.update_offVar()
print pplt.Off_C
print pplt.Off_M
#col.print_adults() 
fdlst1=  [ad.food for ad in ad_list]

fdlst = [np.log(m) for m in fdlst1]
print "fdlst",  fdlst

num_off1 = col.col_num_off(pplt.Off_M, pplt.Off_C)
print "numoff1", num_off1
num_off = [np.log(n) for n in num_off1]

plt.plot(fdlst1, num_off1)
plt.show()
    