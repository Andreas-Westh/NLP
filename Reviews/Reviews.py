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
sns.countplot(data=df_merged_all,x='gender',hue='gender')
df_merged_all['gender'].value_counts
plt.show()


# Sentida sentiment score
from sentida import Sentida
df_merged_all["sscore"]=df_merged_all["content"].apply(lambda x: Sentida().sentida(x,output="mean",normal=False))


#plot distribution 
sns.distplot(df_merged_all['sscore'])
plt.show()


# score for genders
# FE: Category for sscore
df_merged_all['sscore'].describe()
bins=['low','medium','high','very high']
intervals = [-2,0,1,1.5,6]
df_merged_all["score"]=pd.cut(df_merged_all['sscore'], bins=intervals, labels=bins, include_lowest=True)
sns.countplot(df_merged_all, x='score',hue='score')
plt.show()

sns.barplot(data = df_merged_all, x="gender",y="length",hue="score")
plt.show()


#### NLP
import nltk
# whole data depending on word count, most positive and negative
# made from aarup.csv

# combine all reviews
dftotal = df_merged_all['content'].str.cat()
dftotalwords = nltk.word_tokenize(dftotal,language="danish")
# frequency
dftfd=nltk.FreqDist(dftotalwords)
dftfd.plot(10)
plt.show() # , . and other words can be seen

dftotalwords_clean = [w for w in dftotalwords if len(w) > 4]
dftfd=nltk.FreqDist(dftotalwords_clean)
dftfd.plot(10)
plt.show() 

# remove danish stopwords
stopwords = nltk.corpus.stopwords.words('danish')
stopwords.append('vores')
dtt = dftotalwords_clean
dttsw=[w for w in dtt if w not in stopwords]
swFreq = nltk.FreqDist(dttsw)
swFreq.plot(10)
plt.show()

# save as as a data structure
dffreq = pd.DataFrame(list(swFreq.items()),columns=['Word','Frequency'])
swFreq.items()

# join with aarups-sentiment 
aarup = pd.read_csv('data/aarup.csv', encoding = "iso-8859-1")
aarup.rename(columns={'stem': 'Word'}, inplace=True)
dftotal_stem = pd.merge(dffreq,aarup,on='Word', how='inner')

# stem words - combining words with the same root form
from nltk.stem.snowball import SnowballStemmer
stemmer = SnowballStemmer("danish")
dftotal_stem['sw'] = dftotal_stem['Word'].apply(lambda x: stemmer.stem(x))

# plot 10 most negative and 10 most positive
dfPos = dftotal_stem.sort_values('score', ascending=False).iloc[0:10]
dfNeg = dftotal_stem.sort_values('score', ascending=True).iloc[0:10]
dfNeg = dfNeg.sort_values('score',ascending=False) # 'Reverse' the order, so most negativ is at the bottom
dfTenners = pd.concat([dfPos,dfNeg],axis=0)
PlotTenners = sns.barplot(dfTenners,y='Word',x='score',hue="score", palette="Spectral")
plt.show()


# Spacy
import spacy

nlp = spacy.load("da_core_news_sm")
testtext = df_merged_all.loc[2698,'content']
doc=nlp(testtext)
len(doc)

# word is definied within the doc[x]
doc[0].pos_ # find out waht the word is
doc[0].text # The word

# function for making it a df
def spacydf(document):
    data = []
    for token in doc:
        data.append({
            'text': token.text,
            'Lemma': token.lemma_,
            'pos': token.pos_,
            'is_stop': token.is_stop
            })
    return pd.DataFrame(data)

testdf=spacydf(doc)

# find total NOUNs
totaldoc=nlp(dftotal)
totalnouns=[w.text for w in totaldoc if w.pos_ == "NOUN"]
# Frequency 
Freq_NOUNS = nltk.FreqDist(totalnouns)
Freq_NOUNS.plot(20)
# make it a df
df_NounFreq = pd.DataFrame(list(Freq_NOUNS.items()),columns=['Word','Frequency'])



# bigrams 
review_tokens = nltk.word_tokenize(dftotal)
bigrams = list(nltk.bigrams(review_tokens))
bigrams[2]

# now to find important words, and figuring out the context
# make the list of KPI words
KPI = df_NounFreq.sort_values('Frequency', ascending=False).iloc[:10]
KPI_List = list(KPI['Word'])
#KPI_Top = KPI.iloc[0:10]
# Find the bigrams where KPIs appear
KPI_bigrams = [b for b in bigrams if b[1] in KPI_List and b[0] in list(aarup['Word'])]
