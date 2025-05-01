#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri May  2 00:01:32 2025

@author: andreaswesth
"""

import nltk
from nltk.corpus import genesis
from nltk import FreqDist, ConditionalFreqDist, bigrams

# download text
#nltk.download('genesis')

## Corpus loading
# load english
words = genesis.words('english-kjv.txt')
# total words
print(f"total words: {len(words)}") #f is for f-string
# first 20 words
print(words[:20])


## Lexical Diversity
# ratio of unique words to total words in a text
def lexical_diversity(text):
    return len(set(text)) / len(text)

print("Lexical diversity:", round(lexical_diversity(words), 4))


## Word frequency
# how often a word (or item ig) appears in the text
fdist = FreqDist(words)
print("most common words (fdist):", fdist.most_common(10))


## Conditional counts
# 3. Bigrams and CFD over word pairs
pairs = bigrams(w.lower() for w in words if w.isalpha())
cfd_bigrams = ConditionalFreqDist(pairs)

## Word pair model
# Pick a word and generate a chain of 15 words using most likely continuation
start_word = "god"
print(f"\nGenerated sentence starting from '{start_word}':")
for i in range(15):
    print(start_word, end=' ')
    start_word = cfd_bigrams[start_word].max()