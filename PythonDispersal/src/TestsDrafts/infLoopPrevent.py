'''
Created on Feb 13, 2015

@author: Ruth
http://stackoverflow.com/questions/24840746/the-proper-python-exception-to-break-out-of-an-infinite-loop-that-shouldnt-be-i
'''
 
for i in xrange(10):
    if not i:
        print "hello"
        print i
    else:
        print "goodbye"
        print i
        