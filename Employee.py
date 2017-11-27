# -*- coding: utf-8 -*-
"""
Created on Mon Nov 27 17:28:08 2017

@author: AJ
"""
# Data Preprocessing Template

# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Importing the dataset
dataset = pd.read_excel('Employeeattrition.xlsx')

header = list(dataset.columns.values)
header.remove('Attrition')

X = dataset[header].values
y = dataset['Attrition'].values

# Label Encoding
from sklearn.preprocessing import LabelEncoder
labelencoder=LabelEncoder()
y = labelencoder.fit_transform(f)

