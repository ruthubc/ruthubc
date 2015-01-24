'''
Created on Jan 23, 2015

@author: Ruth
'''

#Original competition equations
    def scramble(self):  # pure scramble competition, everyone gets the same
        [i.update_indFood(self.colony_food) for i in self.colony_list]

    def full_contest(self): # the highest ranks get 1 food, everyone else gets zero
        for k in self.colony_instars():
            insr_len = [j for j in self.colony_list if j.instar == k]
            fraction = len(insr_len) - (self.colony_food * len(insr_len))
            [i.update_indFood(1) for i in self.colony_list if i.rank > fraction and i.instar == k] # need to check numbers?

    def ind_food(self, comp): # comp: 0 = scramble, 1 = mid_contest, 2 = full contest
        [i.update_indFood(0) for i in self.colony_list] # updating all indfood to 0
        if comp == 0:
            print "scramble"
            self.scramble()
        elif comp == 1:
            print "mid_contest"
            self.mid_contest()
        elif comp == 2:
            print "full contest"
            self.full_contest()

    #TODO: check if this actually works
    def col_alive(self): # testing whether colony is dead
        if not self.ad_list and not self.juv_list:
            self.alive = 'dead'

    def colony_list_to_append(self): # returns dictionary values
        return self.colony_dict().values()

    def find_m(self): # finding the gradient for the competition equation
        if self.colony_food < 0.5:
            m = 1/(2*self.colony_food*self.num_spi()-1)
        else :
            m = -(2*(self.colony_food-1))/ (self.num_spi() -1) # everyone gets fed if food_tot >=  0.5
        return m
#TODO: put in exception if m is below or zero!



    def replacing_min_fd(self): # replacing the minimum amount to make sure the numbers add up
        minFdInx = self.food_list().index(min(x for x in self.food_list() if x != 0)) # gets the index of the last spider to be fed
        minSpiFd = self.food_list()[minFdInx]
        tot_ind_fd = sum(self.food_list())
        corrMinAmt = self.tot_col_fd() - (tot_ind_fd - minSpiFd)
        corrMinAmt = round(corrMinAmt, 4)
        self.colony_list[minFdInx].ind_food = corrMinAmt

    def contest_few_inds(self):
        minRankInx = self.rank_list().index(min(self.rank_list()))
        self.colony_list[minRankInx].ind_food = self.tot_col_fd()

    def mid_contest(self):
        if self.tot_col_fd() < 1.0:
            print "few inds"
            self.contest_few_inds()
        else:
            m = self.find_m()
            print "lots inds m:",
            print m
            [spi.contest_many_ind(m) for spi in self.colony_list]
            self.replacing_min_fd()


    def colony_time_step(self, dispersal_list, output_list, min_juv_fd):

            # (1) colony age increase
        self.col_age_increase()  # updates colony age by one

            # (2) adults decide whether to disperse
        #TODO: make dispersal functions according to Leticia's specificiations, change functoin below
        self.colDispersal_choice(self.ad_min_fd, self.ad_max_fd) #dispersal decision TODO: add variables to pop class
        self.dispersal_list = self.spis_to_dis_lst() + self.dispersal_list #adds spiders to dispersal list

        #rest of the steps
        self.Core_colony_timestep(dispersal_list, output_list, min_juv_fd)\


