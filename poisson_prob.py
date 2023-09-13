# -*- coding: utf-8 -*-
"""
Created on Wed Sep 13 08:47:14 2023

@author: Underwood_Derek
"""

from numpy import random

# known rate of some discrete event, expected goals, shots, saves, etc
known_rate = 2
x = random.poisson(lam=known_rate, size=100000)

# initializing k
k = 3
 
# using list comprehension
# to get numbers > k
count = len([i for i in x if i > k])
print("P(k > ", k, "): ", count/100000, sep = '')