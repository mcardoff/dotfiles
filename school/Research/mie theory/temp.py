#import PyMieScatt as ps
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
from aux_funcs import *
import csv
import sys
from scipy.special import spherical_jn


wlr1 = np.linspace(10,1200,200)
wlr2 = np.linspace(10,1200,500)
np.savetxt("C:/Users/mcard/Documents/mie theory/wlr1.csv", wlr1, delimiter=",")
np.savetxt("C:/Users/mcard/Documents/mie theory/wlr2.csv", wlr2, delimiter=",")
