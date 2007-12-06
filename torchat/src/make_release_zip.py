#!/usr/bin/python

import version
import os

dir = os.getcwd()
os.chdir("..")
try:
    os.mkdir("release")
except:
    pass
os.chdir("release")
