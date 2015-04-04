'''
Created on Apr 4, 2015

@author: user
'''
'''
f = open('RunNumbers.txt', 'aw')


#print f.read()

#print f.readline()

f.write('11\n')
#for line in f:
#        print line,
        


f.close()

f= open('RunNumbers.txt','a+')
f.write("12")

f = open('RunNumbers.txt', 'r')
for line in f:
    print line,


print f.readlines()
print len(f.readlines())
'''


import csv



    
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
    