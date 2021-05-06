#!/usr/bin/python3

import math
import numpy
import sys
import statistics
from operator import itemgetter
from collections import namedtuple
import operator

from PIL import Image

FILENAME='small.png' #image can be in gif jpeg or png format
im=Image.open(FILENAME).convert('RGB')
pix=im.load()
w=im.size[0]
h=im.size[1]
for i in range(w):
    for j in range(h):
        print("(",i,j,")",pix[i, j])

#im = Image.open("../../../Downloads/zhenya_blut1.png", 'r')
#im = Image.open("crab.png", 'r')
#pix_val = list(im.getdata())
#px = im.load()
#print(pix_val)
