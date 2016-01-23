'''
Created on Jul 9, 2014

@author: Ruth
'''
'''
instar = {
      0: 0.5,
      1: 1,
      2: 1.5
      }

#print instar[0]
#print instar.keys()
#print instar
#print instar.items()

print [k for k, v in instar.items() if v < 6]

print [k for k in instar if instar[k] < 6]
'''

states = {
    'Oregon': 'OR',
    'Florida': 'FL',
    'California': 'CA',
    'New York': 'NY',
    'Michigan': 'MI'
}

# create a basic set of states and some cities in them
cities = {
    'CA': 'San Francisco',
    'MI': 'Detroit',
    'FL': 'Jacksonville'
}
cities['NY'] = 'New York'
cities['OR'] = 'Portland'

print 'peterbrough' * 10
print "NY State has: ", cities['NY']
print "OR State has: ", cities['OR']

# do it by using the state then cities dict
print '-' * 10
print "Michigan has: ", cities[states['Michigan']]
print "Florida has: ", cities[states['Florida']]

# print every state abbreviation
print '-' * 10
for state, abbrev in states.items():
    print "%s is abbreviated %s" % (state, abbrev)
    

print states.items()

print states.values()

print "The keys in my dictionary are: " + ','.join([k for k in states/.keys()])
