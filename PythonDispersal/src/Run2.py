'''
Created on Jun 27, 2014

@author: Ruth

'''
import numpy as np
from core.ColonyClass import Colony
from core.SpiderClass import Spider

def run_function(colony,
        adult_size,
        number_offspring):

    """ updates the age of all spiders within the colony"""
    colony.age_increment()

    """ (1) Reproduction"""
    colony.reproduction()

print run_function.__doc__

print Colony.__doc__
