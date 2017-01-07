'''
Created on Jan 6, 2017

@author: Ruth
'''

import pip

def install(package):
    pip.main(['install', package])
    
install("lmfit")