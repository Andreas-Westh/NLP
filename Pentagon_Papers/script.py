import spacy
import pandas as pd
import spacy
from textblob import TextBlob
import nltk
import os

nlp = spacy.load("en_core_web_sm")

# Folder with OCR text files
folder = "txt_files"

# Output list
data = []

df = pd.DataFrame()

# loop through all .txt files inside subfolders of txt_files
for root, dirs, files in os.walk(folder):
    for file in files:
        if file.endswith(".txt") and file != "out.txt":
            filepath = os.path.join(root, file)

            with open(filepath, "r", encoding="utf-8") as f:
                text = f.read()

            doc = nlp(text)

            # grab all nouns
            nouns = [
                token.text.lower()
                for token in doc
                if token.pos_ == "NOUN"
                and token.is_alpha             # only real words, no numbers/symbols
                and len(token.text) > 2        # skip short junk like "r" or "ee"
                ]
            noun_freq = pd.Series(nouns).value_counts()
            top_noun = noun_freq.index[0] if not noun_freq.empty else ""
            # grab all adjectives
            adjs = [
                token.text.lower()
                for token in doc
                if token.pos_ == "ADJ"
                and token.is_alpha
                and len(token.text) > 2
                ]

            adj_freq = pd.Series(adjs).value_counts()
            top_adj = adj_freq.index[0] if not adj_freq.empty else ""


            # sentiment analysis
            sentiment = TextBlob(text).sentiment.polarity
            
            # get all entity labels (e.g. PERSON, ORG, GPE, etc.)
            entity_labels = [ent.label_ for ent in doc.ents]
            ent_freq = pd.Series(entity_labels).value_counts()
            top_entity = ent_freq.index[0] if not ent_freq.empty else ""

            # extract folder name (e.g. "Pentagon-Papers-Part-V-B-3a")
            folder_name = os.path.basename(root)

            # combine folder and filename
            filename = f"{folder_name}-{file}"

            # extract ppname and page number
            parts = file.split("-")
            ppname = filename.split(".")[0]
            try:
                page = int(parts[-1].split(".")[0])
            except ValueError:
                page = -1

            # append result
            data.append({
                "filename": filename,
                "text": text.strip()[:1500],
                "size": len(text),
                "sentiment": sentiment,
                "entity": top_entity,
                "noun": top_noun,
                "adj": top_adj,
                "ppname": ppname,
                "page": page
            })



# Create DataFrame
df = pd.DataFrame(data)

df.to_pickle("pentagonpapers.pkl")


# Optional: sort by page
df = df.sort_values(by="page").reset_index(drop=True)

# Preview
print(df.head())