'''
Created on 2013-02-10

@author: Ruth
'''


class Pet(object):
    "making class pet"
    def __init__(self, name, species, id = "1d"):
        self.name = name
        self.id = id
        self.species = species

    def getName(self):
        return self.name

    def getSpecies(self):
        return self.species

    def addingSomething(self, number):
        self.species += number

    def __str__(self):
        return "%s is a %s" % (self.name, self.species)


'''
jon = Pet("jon", "cat")

num = Pet("pe", 5)

print num

num.addingSomething(7)

print num


'''


#subclasses


class Dog(Pet):

    def __init__(self, name, chases_cats):
        Pet.__init__(self, name, "Dog")
        self.chases_cats = chases_cats

    def chasesCats(self):
        return self.chases_cats

pete= Dog("pete", True)

print pete.chases_cats

print pete.getName()

print pete.id

class Cat(Dog):
    def __init__(self, name, running_cat):
        Dog.__init__(self, name, False)
        self.running_cat = running_cat
 

list2 = ["pete", True]
print list2[1]

peter = Dog(list2[0], list2[1])

class Test(object):
        def __init__(self, inputlst):
            self.name = inputlst[0]
            self.id = inputlst[1]
            self.sec = inputlst[2]
            
        def spiderList(self):
            return [self.name, self.id, self.sec]

list3 = ['apily', 45, 'sed43']

apluy = Test(list3)

print apluy.name

print apluy.spiderList()

'''
   
class extends(Pet):
    
    def __init__(self):
        self.ext = (Pet, thing)
        
    def doThing(self):
        print self.thing


jon = Pet("jon", "cat")

li = extends(jon, 'donut')

print li.name

'''