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

