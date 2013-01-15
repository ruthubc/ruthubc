'''
Created on 2012-12-26

@author: ruth
TODO: include scramble comp into amt_ind_food
'''


def instar_inc(spider_list, instar_levels_list):
    "updates the instars of individuals after feeding"
    "instar_levels_list defines the size limits to progress to the nest instar"

    for i in range(len(spider_list)):

        for k in range(len(instar_levels_list)):

            if (spider_list[i].size > instar_levels_list[k] and
             spider_list[i].size <= instar_levels_list[k + 1]):
                spider_list[i].instar = k + 1


def rel_size(spider_list):
    '''calculates the relative size of ind compared to colony
    size varies between 0 and 1'''
    total_size = 0  # getting total size of spiders in colony
    for i in range(len(spider_list)):
        total_size += spider_list[i].size

        for j in range(len(spider_list)):
            spider_list[j].rel_size = spider_list[j].size / total_size


def colony_instar(spider_list):
    "totals the instar number"
    total_instar = 0
    for i in range(len(spider_list)):
        total_instar += spider_list[i].instar
        return total_instar


def colony_food(spider_list, ):


def amt_ind_food(rel_size, col_food):
    '''calculating the amount of food for each spider
    amount of food varies between 0 and 1'''
    ind_food = rel_size * col_food
    return ind_food


def growth(spider_list, col_food):
    