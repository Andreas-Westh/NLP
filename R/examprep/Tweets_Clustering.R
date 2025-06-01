library(tm)
library(tidyverse)

# read tweets
tweets <- readLines("data/Tweets.txt")


# building corpus
corpus <- Corpus(VectorSource(tweets))


# create term document matrix
tdm <- TermDocumentMatrix(corpus,
                          control = list(minWordLength=c(1,Inf)))

# high sparcity means words dont appear in most documents
t <- removeSparseTerms(tdm, sparse = 0.98)

m <- as.matrix(t)


# Freq terms
freq <- rowSums(m)
barplot(freq, las=2,col=rainbow(25))




# Hieriachical clustering with dendrogram
distance <- dist(scale(m))
print(distance)
hc <- hclust(distance, method = "ward.D")
plot(hc, hang = -1)
rect.hclust(hc, k = 12)


# nonhierarcical k-means clustering
m1 <- t(m)
set.seed(222)
k <- 12
kc <- kmeans(m1, k)
kc
# the higher between_ss / total_SS the better - the distance between the clusters
#   and the lower Within cluster sum of squares the better