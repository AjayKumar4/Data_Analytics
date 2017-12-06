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
from ggplot import *

# Importing the dataset
dataset = pd.read_excel('Employeeattrition.xlsx')

employee_dataset = dataset
employee_dataset['Education'] = employee_dataset['Education'].replace([1, 2, 3, 4, 5], 
                     ['Below College', 'College', 'Bachelor', 'Master', 'Doctor']) 
employee_dataset['EnvironmentSatisfaction'] = employee_dataset['EnvironmentSatisfaction'].replace([1, 2, 3, 4], 
                     ['Low', 'Medium', 'High', 'Very High']) 
employee_dataset['JobInvolvement'] = employee_dataset['JobInvolvement'].replace([1, 2, 3, 4], 
                     ['Low', 'Medium', 'High', 'Very High']) 
employee_dataset['JobSatisfaction'] = employee_dataset['JobSatisfaction'].replace([1, 2, 3, 4], 
                     ['Low', 'Medium', 'High', 'Very High']) 
employee_dataset['PerformanceRating'] = employee_dataset['PerformanceRating'].replace([1, 2, 3, 4], 
                     ['Low', 'Good', 'Excellent', 'Outstanding']) 
employee_dataset['RelationshipSatisfaction'] = employee_dataset['RelationshipSatisfaction'].replace([1, 2, 3, 4], 
                     ['Low', 'Medium', 'High', 'Very High']) 
employee_dataset['WorkLifeBalance'] = employee_dataset['WorkLifeBalance'].replace([1, 2, 3, 4], 
                     ['Bad', 'Good', 'Better', 'Best']) 

dataset_describe = employee_dataset.describe()

ggplot(dataset,aes(x = 'Attrition', color = 'Attrition')) + geom_bar()

#ggplot(aes(x='Attrition',color = 'Attrition'), data=dataset) + \
#     geom_bar()

header = list(dataset.columns.values)
header.remove('Attrition')

#X = dataset[header].values.astype('str') 
X = dataset[header].values
y = dataset['Attrition'].values

# Label Encoding
from sklearn.preprocessing import LabelEncoder
labelencoder=LabelEncoder()
y = labelencoder.fit_transform(y)



