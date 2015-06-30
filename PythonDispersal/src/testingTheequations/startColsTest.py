'''
Created on Jun 29, 2015

@author: user
'''
from core.StartColsClass import StartCols

cols = StartCols(126, 1, ad_fd = 0.6)
cols.make_col_list()
col_list = cols.col_list
    

print "col_list", col_list