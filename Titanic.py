#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 30 22:43:10 2025

@author: andreaswesth
"""

# Import dataset
import kagglehub

# Download latest version
path = kagglehub.dataset_download("brendan45774/test-file")

print("Path to dataset files:", path)



# Task
import pandas as pd

# 1 Load data
df = pd.read_csv("data/Titanic-Dataset.csv")



# 2 + 3 Extract title and save in  'title'
df['title'] = df['Name'].str.extract(r'\s(\w+)\.')

# Check NaN values
pd.isnull(df).title.sum()

# 4 Group rare title
df['title'].unique()

common_titles = ['Mr', 'Mrs', 'Miss', 'Master', 'Ms']

# ~is like ! in R
# grouping in their own df
rare_df = df[~df['title'].isin(common_titles)]

# making a boolean
df['is_rare'] = ~df['title'].isin(common_titles)

# or just making a new column, with NaN for non rare
df['rare_title'] = df['title'].where(~df['title'].isin(common_titles))


#  5 Count frequency of each title
print(df['title'].value_counts())


# 6 Plot freq
import matplotlib.pyplot as plt
df['title'].value_counts().plot(kind='bar')
plt.xlabel('Title')
plt.ylabel('Count')
plt.title('Freq of titles')
plt.show()



# Extra
import seaborn as sns
plt.figure(figsize=(10, 6))
sns.barplot(data=df, x='title', y='Survived', estimator = 'mean')#, hue='sex')
plt.title('Survival Rate by Passenger title')
plt.show()


sns.barplot(data=df[df['title'].isin(['Mr', 'Miss', 'Mrs', 'Master','Dr', 'Rev', 'Capt'])], 
            x='title', y='Survived', estimator='mean')
plt.title('Survival Rate by Passenger title filteret')
plt.show()


plt.figure(figsize=(10, 6))
sns.countplot(data=df[df['title'].isin(['Mr', 'Miss', 'Mrs', 'Master','Dr', 'Rev', 'Capt'])], x='title', hue='Survived')
plt.title('idk')
plt.show()