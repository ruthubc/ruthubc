'''
Created on Dec 18, 2014

@author: Ruth
'''
class Pet(object):
    def __init__(self, name, size, ID):
        self.name = name
        self.size = size
        self.ID = ID

    def GetName(self):
        return self.name

    def GetList(self):
        return [self.name, self.size, self.ID]


class Puppy(Pet):
    def __init__(self, name, size, ID, pupID):
        Pet.__init__(self, name, size, ID)
        self.pupID = pupID


class Dog(Pet):
    def __init__(self, petList, dogID):
        Pet.__init__(self, petList[0], petList[1], petList[2])
        self.dogID = dogID

ambr = Pet('Amber', 3, "D1")

print ambr.GetList()

prl = Puppy('Pearl', 1, "P1", "puppy")

print prl.GetList()

ruby = Dog(prl.GetList(), "dog1")

print ruby.name

