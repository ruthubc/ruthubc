'''
Created on Dec 18, 2014

@author: Ruth
'''
#pylint: disable= too-many-statements, line-too-long

from SpiderClass import Spider
import numpy as np


class Juv(Spider):

    def __init__(self, SpiderList = [0, 0, 0], juv_fd =0):  # spider list = rank , die, food
        Spider.__init__(self, SpiderList[0], SpiderList[1], SpiderList[2])
        self.juv_fd = juv_fd

    def assign_ind_fd(self, colony_fd, num_juvs, cal_med_rnk, slp):  # TODO: check this works
        xbr = float(colony_fd / num_juvs)
        tm1 = slp * cal_med_rnk
        tm2 = xbr - ((xbr * self.rank) / cal_med_rnk)
        tm12 = (tm1 * tm2) / np.power(xbr, 2)
        CompEqn = xbr * (1 + tm12)
        if CompEqn > 1:
            IndFd = 1
        elif CompEqn < 0:
            IndFd = 0
        else:
            IndFd = CompEqn
        self.juv_fd = IndFd
