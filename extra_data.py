# -*- coding: utf-8 -*-
"""
Created on Sat Jul  6 09:38:27 2024

@author: HP
"""

# Run this python file on your local python writer
# Make sure pybaseball is installed on your terminal
# Change local directory last code line to match where you'd saved the file





# pip install pybaseball
# dictionary: https://baseballsavant.mlb.com/csv-docs
import pandas as pd
from pybaseball import statcast

# Prolly more date range feasible if you we want to expand scope
savant = statcast(start_dt='2021-04-01', end_dt='2023-11-01')
savant.to_csv('C:/Users/HP/OneDrive/Documents/R/SURE 2024/CMSAC-Dodgers-Project/savant.csv', index=False)

