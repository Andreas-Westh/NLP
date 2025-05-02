# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

# import
import numpy as np
import statsmodels.api as sm
import pandas as pd
from nltk import FreqDist, ConditionalFreqDist, bigrams

# Option 1 of loading
with open("data/out.txt", "r") as file:
    lines = file.readlines()

df = pd.DataFrame(lines, columns=["text"])


# Other Wulf way - saves as 1 single string
fh = open("data/out.txt", mode = "r")
content = fh.read()

# Splitting them
cententlist = content.split("SECRET")

# Make into a dataframe
contenDf = pd.DataFrame(data = cententlist, columns = ["text"])

# Feature Engineerijg: size of each chunck
# lambda laver nogle one-liner functioner
contenDf["size"] = contenDf["text"].apply(lambda x: len(x))






### for show
tokens = [w.lower() for w in content.split() if w.isalpha()]
pairs = bigrams(tokens)
cfd_bigrams = ConditionalFreqDist(pairs)

start_word = "vietnam"
print(f"\nGenerated sentence starting from '{start_word}':")
for i in range(15):
    print(start_word, end=' ')
    start_word = cfd_bigrams[start_word].max()
