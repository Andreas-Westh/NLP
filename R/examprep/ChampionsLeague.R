# exam goal
# find different articles about the champions league final
# count the most freq words (remember in %)
# get sentiment, are they different between different popishers?



library(tidytext)
library(tidyverse)
library(stopwords)
tmp <- data.frame(fra="DR",
              artikel="Korrespondent: Dødsfald under Champions League-fejring har lagt en dæmper på Paris", 
              text="I går kl. 11:11

https://www.dr.dk/nyheder/udland/korrespondent-doedsfald-under-fodboldfejring-har-lagt-en-daemper-paa-paris
To personer er døde efter voldsomme uroligheder sent i går i Paris og andre franske byer, hvor fodboldfans fejrede Paris Saint-Germains første triumf i Champions League.

Det oplyser Frankrigs indenrigsministerium ifølge nyhedsbureauet AFP.

Dødsfaldene bliver også omtalt hos den franske tv-station TF1. og hos Le Monde.

Her står der, at en 17-årig mand blev stukket med kniv og døde i Dax syd for Bordeaux, men de nærmere omstændigheder omkring dødsfaldet er uklare.

De franske medier skriver også, at en 20-årig på scooter døde efter at være blevet ramt af et køretøj med fodboldtilhængere.

Det er dog fortsat uklart, om der er en direkte sammenhæng mellem fodboldfejringen og dødsfaldene.

Glæde og vild jubel på Champs-Elysees i Paris i går aftes. Men der opstod også uroligheder og kampe mellem nogle folk på gaden og politiet.


Anklagemyndigheden i Dax oplyser ifølge Le Monde, at knivstikkeriet skete 'i forbindelse med fejringerne', men om der er en 'reel sammenhæng, og om det drejer sig om tilhængere, kan ikke bekræftes på nuværende tidspunkt.', lydet det.

Le Monde skriver desuden, at mindst tre personer blev kvæstet i Grenoble i det sydøstlige Frankrig, da føreren af en bil mistede herredømmet og kørte ind i en gruppe mennesker, som fejrede sejren.

Macron tager imod - men stemningen bliver en anden
I dag er der planlagt en stor fejring af PSG's triumf, hvor de i aften modtages af den franske præsident, Emmanuel Macron, i Elyséepalæet.

Det er kæmpestort i Frankrig, at klubben for første gang har vundet fodboldturneringen, og det er desuden første gang i 30 år, at en fransk fodboldklub står med CL-pokalen.

Men nattens uroligheder lægger en gevaldig dæmper på fejringen, der nu bliver afholdt i et helt andet lys på grund af de to ofre, de mange sårede og de flere hundrede anholdelser.

Det siger DR's internationale korrespondent, Stéphanie Surrugue, der befinder sig i Paris.

- Det, der skulle have været en kæmpe fest, får nu en helt anden tone, og der er lagt en dæmper på franskmændenes glædesrus, siger hun.

PSG vandt over Inter med 5-0, og det fik de tusindvis af fans til at gå på gaden i den franske hovedstad og flere andre byer.

Ifølge den franske tv-station TF1 var mellem 2.000 og 3.000 mennesker i aftes samlet på Champs-Élysées i Paris efter kampen.


Flere hundrede blev anholdt i forbindelse med urolighederne. Der er også meldinger om smadrede butiksvinduer og enkelte plyndringer i storbyen.

Her til formiddag er tallet 559 anholdelser, heraf 491 anholdelser i Paris.

Det franske indenrigsministerium har indtil videre registreret 692 brande over hele landet, herunder 264 udbrændte køretøjer, skriver Le Monde.

Inden kampen meldte myndighederne ud, at over 5.000 betjente ville blive sendt på gaden.

Ifølge det franske medie blev 22 betjente såret, heraf 18 i Paris.

Surrugue: Plejer at være vildt
Stéphanie Surrugue fortæller, at det plejer at gå vildt for sig, når franskmændene går på gaden for at fejre sejre i blandt andet fodboldturneringer, og det er ikke uvant, at trafikken stopper helt, og at folk eksempelvis hopper på bilerne i ren eufori.

Men når det kan gå så galt som i aftes og i nat i Paris, giver det anlednings til at se på, om myndighederne har været godt nok forberedte på, at så mange ville gå gaden for at fejre triumfen.

- Det er jo enormt svært at mandsopdække et helt Frankrig ovenpå en Champions League-finale, men man kan godt forestille sig, at der bliver sat gang i en undersøgelse af, om det her var hændeligt, eller om myndighederne skulle have passet bedre på gaderne, siger Stéphanie Surrugue.")
#artikler_df <- data.frame(fra=NULL,artikel=NULL, text=NULL)
artikler_df <- rbind(artikler_df, tmp)

# tokennize og fjern stopord
#library(reticulate)
#Sys.setenv(RETICULATE_PYTHON = "/opt/anaconda3/envs/spacy/bin/python")
library(spacyr)
spacy_initialize(
  model            = "da_core_news_md",
  refresh_settings = TRUE,
  verbose          = TRUE
)

artikler_spacy <- spacy_parse(artikler_df$text)

doc_ids <- unique(artikler_spacy$doc_id)
better_ids <- unique(artikler_df$fra)
artikler_spacy <- artikler_spacy %>% 
  mutate(doc_id = better_ids[match(doc_id, doc_ids)])

# lemma
artikler_spacy <- artikler_spacy %>% 
  mutate(lemma = str_extract(lemma,"^[A-Za-zÆØÅæøå]+$")) %>% 
  drop_na(lemma) %>% 
  filter(nchar(lemma)>1)
dkstop <- stopwords("da")
artikler_spacy <- artikler_spacy %>% filter(!lemma %in% dkstop)

# count with procent
lemma_counts <- artikler_spacy %>% 
  group_by(doc_id, lemma) %>% 
  summarise(count = n(), .groups = "drop")

total_counts <- lemma_counts %>% 
  group_by(doc_id) %>% 
  summarise(total = sum(count))

lemma_pct <- lemma_counts %>% 
  left_join(total_counts, by = "doc_id") %>% 
  mutate(percent = round(100 * count / total, 2)) %>% 
  arrange(desc(percent))

lemma_pct %>% 
  filter(!doc_id == "NA") %>% 
  group_by(doc_id) %>% 
  slice_max(percent, n = 5, with_ties = F) %>% # to fix top_n() with facet_wrap 
  mutate(lemma = reorder_within(lemma, percent, doc_id)) %>% # needed for the reordering of sort
  ggplot(aes(lemma, percent, fill = doc_id)) +
  geom_col(show.legend = F) +
  facet_wrap(~doc_id, scales = "free") + 
  coord_flip() +
  scale_x_reordered()
layout(t(1:2))
old_plot

lemma_pct_avg <- lemma_pct %>%
  group_by(lemma) %>%
  summarise(mean_percent = mean(percent), .groups = "drop") %>%
  arrange(desc(mean_percent))

library(wordcloud2)
wordcloud2(
  data = lemma_pct %>% select(word = lemma, freq = percent, color),
  color = lemma_pct$doc_id,
  backgroundColor = "white",
  size = 1
)




#backup_df <- artikler_df 
#backup_pct <- lemma_pct

library(Sentida)
scored_tokens <- artikler_spacy %>%
  rowwise() %>%
  mutate(score = sentida(token, output = "mean")) %>%
  ungroup()

scored_tokens %>% 
  filter(doc_id != "NA") %>% 
  group_by(doc_id) %>% 
  slice_max(score, n = 10, with_ties = F) %>% # to fix top_n() with facet_wrap 
  mutate(lemma = reorder_within(lemma, score, doc_id)) %>% # needed for the reordering of sort
  ggplot(aes(lemma, score, fill = doc_id)) +
  geom_col(show.legend = F) +
  facet_wrap(~doc_id, scales = "free") + 
  coord_flip() +
  scale_x_reordered()

scored_tokens %>% 
  filter(doc_id != "NA") %>% 
  group_by(doc_id) %>% 
  slice_min(score, n = 10, with_ties = F) %>% # to fix top_n() with facet_wrap 
  mutate(lemma = reorder_within(lemma, score, doc_id)) %>% # needed for the reordering of sort
  ggplot(aes(lemma, score, fill = doc_id)) +
  geom_col(show.legend = F) +
  facet_wrap(~doc_id, scales = "free") + 
  coord_flip() +
  scale_x_reordered()

mean_scores <- scored_tokens %>%
  group_by(doc_id) %>%
  summarise(mean_score = round(mean(score, na.rm = TRUE), 2))

mean_scores %>% 
  ggplot(aes(doc_id,mean_score,fill=))


