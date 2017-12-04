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
dataset['Education'] = dataset['Education'].replace([1, 2, 3, 4, 5], 
                     ['Below College', 'College', 'Bachelor', 'Master', 'Doctor']) 
dataset['EnvironmentSatisfaction'] = dataset['EnvironmentSatisfaction'].replace([1, 2, 3, 4], 
                     ['Low', 'Medium', 'High', 'Very High']) 
dataset['JobInvolvement'] = dataset['JobInvolvement'].replace([1, 2, 3, 4], 
                     ['Low', 'Medium', 'High', 'Very High']) 
dataset['JobSatisfaction'] = dataset['JobSatisfaction'].replace([1, 2, 3, 4], 
                     ['Low', 'Medium', 'High', 'Very High']) 
dataset['PerformanceRating'] = dataset['PerformanceRating'].replace([1, 2, 3, 4], 
                     ['Low', 'Good', 'Excellent', 'Outstanding']) 
dataset['RelationshipSatisfaction'] = dataset['RelationshipSatisfaction'].replace([1, 2, 3, 4], 
                     ['Low', 'Medium', 'High', 'Very High']) 
dataset['WorkLifeBalance'] = dataset['WorkLifeBalance'].replace([1, 2, 3, 4], 
                     ['Bad', 'Good', 'Better', 'Best']) 

dataset.describe()
header = list(dataset.columns.values)
header.remove('Attrition')

#X = dataset[header].values.astype('str') 
X = dataset[header].values
y = dataset['Attrition'].values

# Label Encoding
from sklearn.preprocessing import LabelEncoder
labelencoder=LabelEncoder()
y = labelencoder.fit_transform(y)



