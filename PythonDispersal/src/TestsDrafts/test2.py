'''
Created on 2012-12-22

@author: ruth

'''
print('test2')


class Employee:
    'Common base class for all employees'
    empCount = 0

    def __init__(self, name, salary):
        self.name = name
        self.salary = salary
        Employee.empCount += 1


    def displayCount(self):
        print "Total Employee %d" % Employee.empCount
       
    def displayEmployee(self):
        print "Name : ", self.name,  ", Salary: ", self.salary
        
    def displaySalary(self):
        print "Employee salary: %d" % self.salary
        

"This would create first object of Employee class"
emp1 = Employee("Zara", 2000)
"This would create second object of Employee class"
emp2 = Employee("Manni", 5000)

emp3 = Employee("Anna", 50990)

emp1.displayEmployee()
emp2.displayEmployee()
emp3.displayEmployee()
print "Total Employee %d" % Employee.empCount

emp1.displayCount()
emp2.displaySalary()

emp1.salary = emp1.salary + 38

print emp1.salary
