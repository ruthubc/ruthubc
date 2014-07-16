'''
Created on Jul 14, 2014

@author: Ruth
'''

import csv
'''

RESULTS = ['apple', 'cherry', 'orange', 'mango']
RESULTS2 = ['apple2', 'cherry2', 'orange2', 'mango2']

resultFile = open("mypythonfile.csv",'wb')

wr = csv.writer(resultFile, dialect='excel')

wr.writerow(RESULTS)

wr.writerow(RESULTS2)

RESULTS3 = ['apple3', 'cherry3', 'orange3', 'mango3']

wr.writerow(RESULTS3)

y=2+4
z=4+6

RESULTS4 = [y, z, 5, 7]

wr.writerow(RESULTS4)


def testWrite(i):
    a = i+1
    b = i+2
    c = i+4
    d = i+5
    return [a, b, c, d]




resultFile = open("mypythonfile.csv",'wb')
wr = csv.writer(resultFile, dialect='excel')

for i in range(0, 5):
    wr.writerow(  [7,8,9] + testWrite(i) )
    print testWrite(i) + [7,8,9]
    
print 'done'
'''

x = {'key1': 1, 'key2': 2, 'key3': 3,'key4': 4}

y={'dict2_1': 10, 'dict2_2': 20}

z= dict(x.items() + y.items())

#mydict.update(10)

#mydict.update(mydict2)
'''
writer = csv.writer(open('dict.csv', 'wb'))

for key, value in mydict.items():
    writer.writerow([key, value])
'''
print z.values() #returns a list of the items in the dictionary

print z.keys()

resultFile = open("mypythonfile.csv",'wb')


wr = csv.writer(resultFile, dialect='excel')

class exportRow(object):
    def __init__(self, write, result):
        self.write = write
        self.result = result
        
    def Write_to_file(self):
        
        
        
    
    
    
    

ROWNAMES = dict.keys(z)
wr.writerow(ROWNAMES)
ROW1= dict.values(z)
wr.writerow(ROW1)
    