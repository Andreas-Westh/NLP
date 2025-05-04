import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

# import data
df = pd.read_csv("data/home.csv", encoding="iso-8859-1")

df.info()
df.describe()




# Feature Engineering length of reviews
df['length'] = df['content'].apply(len)


# polt distribution
sns.distplot(df['length'])
plt.show()

# subsetting
dfsub = df[(df['length'] > 100) & (df['length'] < 1000)]
dfsub = df.query('length > 100 & length < 1000')


# Feature Engineer: First aName
df['Navn'] = df['name'].str.split()
# only get the first element (aka first name) 
# .title makes it start with a capital letter
df['Navn'] = df['Navn'].apply(lambda x: x[0])
df['Navn'] = df['Navn'].str.title()

# Feature Enginnering: Gender from male and female names
# Load data
male_names = pd.read_excel('data/drenge.xls',header=None)
male_names.rename(columns={0: 'Navn'}, inplace=True)
# FE: boolean
male_names['male_name'] = 'yes'
female_names = pd.read_excel('data/piger.xls',header=None)
female_names.rename(columns={0: 'Navn'}, inplace=True)
# FE: Boolean
female_names['female_name'] = 'yes'

# this needs to be male/female, depending on which of the gender name lists, the name appears in
df_merged_boy = pd.merge(df, male_names,on ="Navn",how="left")
df_merged_all = pd.merge(df_merged_boy, female_names,on ="Navn",how="left")


# Compute gender
def computegender(row):
    gender=""
    if (row["male_name"]=="yes") and (row["female_name"]=="yes"):
        gender="Uni"
    elif row["male_name"]=="yes":
        gender="M"
    elif row["female_name"]=="yes":
        gender="F"
    else:
        gender="U"
    return gender
# use function
df_merged_all['gender']=df_merged_all.apply(computegender,axis=1)

# plot
sns.countplot(data=df_merged_all,x='gender')
df_merged_all['gender'].value_counts
plt.show()
