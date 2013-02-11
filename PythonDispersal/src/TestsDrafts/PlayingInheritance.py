'''
Created on 2013-02-10

@author: Ruth
'''

class Pet(object):
    "making class pet"
    def __init__(self, name, species):
        self.name = name
        self.species = species

        def getName(self):
            return self.name

        def getSpecies(self):
            return self.species

    def __str__(self):
        return "%s is a %s" % (self.name, self.species)



jon = Pet("jon", "cat")

print jon

#subclasses


class Dog(Pet):

    def __init__(self, name, chases_cats):
        Pet.__init__(self, name, "Dog")
        self.chases_cats = chases_cats

    def chasesCats(self):
        return self.chases_cats


pete= Dog("pete", True)

print pete.chases_cats

