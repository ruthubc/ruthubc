'''
Created on Jul 15, 2014

@author: user
'''

import csv


#(1) open csv file and make writing object (or whatever it is)

resultFile = open("mypythonfile.csv",'wb')

wr = csv.writer(resultFile, dialect='excel')


