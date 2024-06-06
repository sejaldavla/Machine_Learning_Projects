# required packages

library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(Hmisc)
library(caret)
library(GGally)

df<- read.csv(here("winequality.csv")) |>
  clean_names()

# observe data

str(df)
describe(df) 

# find summary of NA values

colSums(is.na(df)) # data has no missing values

# get a sense of categorical, non-numerical columns and identify categories

unique(df$quality)
unique(df$good)
unique(df$color)

df$quality <- as.factor(df$quality)
df$good <- as.factor(df$good)
df$color <- as.factor(df$color)


# For numerical data, lets pot some basic graphs and observe data distribution

df |>
  ggplot(aes(quality,fill=color)) +
  geom_histogram(binwidth = 0.5) +  
  facet_wrap(~color) +
  labs(title = "Wine Quality in Red and White Wine") 

df |>
  ggplot(aes(x = quality, y = alcohol, color = color)) +
  geom_point(position = position_jitter())+
  facet_wrap(~color) +
  labs(title = "Alcohol content based on wine quality in Red and White wine")

#############################################################################################

# Clustering - Data preparation

# step 1 - all columns must be numeric
str(df)
df |>
  mutate(color = ifelse(color == "red", 0, 1))

df$quality <- as.numeric(df$quality)
df$good <- as.numeric(df$good)
df$color <- as.numeric(df$color)

# step 2 - Find near zero variance

nearZeroVar(df) # this wil give a column no that could be problematic with high variance

# step 3 - Normalize and Scale data

norm_df <- sapply(df, function(x) (x - min(x))/(max(x) - min(x)))  # rescales data between 0 and 1
# observe what happens to color and good columns

scale_df <- scale(df) # another way of normalizing data

# step 4 - Check correlations between columns

ggcorr(scale_df,
       label = TRUE) # this helps check for colinearity

scale_df_trim <- scale_df[ , colnames(scale_df) != "good"]

# Data is ready for clustering

# step 1 - determine the number of clusters

kclust <- kmeans(scale_df_trim, centers = 3) # centers is no of clusters
kclust 

class(kclust)
augment(kclust, scale_df_trim) # info about each point and cluster
tidy(kclust) #info about cluster ie withinss
glance(kclust) # summary about clustering

# How many clusters do you need?

kclusts <- 
  tibble(k = 1:10) |> 
  mutate(
    kclust = map(k, ~kmeans(scale_df_trim, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, scale_df_trim)
  )

kclusts

clusters <- kclusts |>
  unnest(cols = c(tidied))

assignments <- kclusts |>
  unnest(cols = c(augmented))

clusterings <- kclusts %>%
  unnest(cols = c(glanced))

# Visualize the clusters

assignments |>
  ggplot(aes(x = citric_acid, y = residual_sugar)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)

# What is the right cluster number? Use the elbow method

clusterings |>
  ggplot(aes(k, tot.withinss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(0,10),
                     breaks = seq(0, 10, 1))

# This graph shows the variance within the clusters. It decreases as k increases. The elbow or bend around k = 3 indicates that additional clusters beyond the third have little value. 

# plot data with 3 clusters again

assignments |>
  filter(k == 3) |>
  ggplot(aes(x = citric_acid, y = residual_sugar)) +
  geom_point(aes(color = .cluster), alpha = 0.8) 

# Exercise: Is k=3 better or k=5? Visualize both data and decide which is better


############################################################################################

# Heirarchical cluster 

df_trim <- df |>
  slice_head(n = 50)

dist_mat <- dist(df_trim, method = "euclidean")
dist_mat

h_cluster <- hclust(dist_mat, method = "average")
h_cluster

# plot a dendrogram

plot(h_cluster)

# There are two ways to trim the tree - height and no of clusters

k_trim <- cutree(h_cluster, k = 3)
k_trim

table(k_trim)
rect.hclust(h_cluster, k = 3, border = "blue")

abline(h = 30, col = "blue")
  




















