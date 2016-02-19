'''
Created on May 22, 2015

@author: Ruth
'''

from AdultClass import Adult
from JuvClass import Juv
from SpiderClass import Spider
from ColonyClass import Colony


class StartCols(object):
    def __init__(self, indFile, compType, K, comp_slp, ad_fd, ):
        self.indFile = indFile
        self.K = K
        self.compType = compType
        self.comp_slp = comp_slp
        self.ad_fd = ad_fd
        self.col_no = 0
        self.sze_min = 1
        self.sze_max = self.K
        self.intvl = self.sze_max / 30
        self.col_list = []

    def make_col(self, colSize):
        self.col_no += 1
        ad_list = []
        for _ in range(0, colSize):
            ad_list.append(Adult([1, 0, self.ad_fd], 0, 0))
        col = Colony(self.compType, self.indFile, self.col_no, ad_list, slope = self.comp_slp)
        return col

    def make_col_list(self):
        sizeRangeList = [1, 1, 1, 1, 1] + range(self.intvl, (self.sze_max + self.intvl), self.intvl)
        sizeRangeList = sizeRangeList * 2
        for  j in sizeRangeList:
            col = self.make_col(j)
            self.col_list.append(col)
