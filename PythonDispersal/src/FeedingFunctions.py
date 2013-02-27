'''
Created on 2012-12-26

@author: ruth
TODO: include scramble comp into amt_ind_food
'''




    


def amt_ind_food(rel_size, col_food):
    '''calculating the amount of food for each spider
    amount of food varies between 0 and 1'''
    ind_food = rel_size * col_food
    return ind_food


def growth(spider_list, col_food):
    