'''
Created on Apr 4, 2015

@author: user
'''


import csv
'''
i=[]


with open('RunNumbers.csv', 'rb') as f:
    reader = csv.reader(f)
    for row in reader:
        print row
        i = i + [int(row[0])]
    print i

print i

maxNum =  max(i)

with open('RunNumbers.csv', 'ab') as f:
    writer = csv.writer(f, dialect='excel')
    writer.writerow([maxNum +1])
'''

from core.Functions import run_numbers

print run_numbers("RunNumbers.csv")