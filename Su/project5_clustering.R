
# Clustering 

# Load the data set
library(tidyverse)
nfl_passing <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nfl_passing.csv")
spec(nfl_passing)
library(ggplot2)
library(flexclust)


## clustering 1: 

M1 <- nfl_passing |> 
  group_by(posteam) |> 
  summarize(total_pass = n(),
            num_complete = sum(complete_pass, na.rm = TRUE),
            complete_rate = num_complete / total_pass,
            num_interception = sum(interception, na.rm = TRUE),
            interception_rate = num_interception / total_pass,
            touchdown_rate = mean(touchdown, na.rm = TRUE),
            avg_yards_gained = mean(yards_gained, na.rm = TRUE),
            avg_epa = mean(epa, na.rm = TRUE),
            qb_hit_rate = mean(qb_hit, na.rm = TRUE),
            )
  

M1 <- M1[, -c(2,3,5)] 
rownames(M1) <- M1$posteam
M1_revised <- M1[, -1]

# standardizing M1
M1_std <-scale(M1_revised, scale = TRUE)[,]; head(M1_std)
rownames(M1_std) <- M1$posteam
D <- as.matrix(dist(M1_std)); head(D)

clusters_complete <- cutree(hclust(as.dist(D), method = "complete"), 4); clusters_complete
q <- cclust(M1_std, k = 4, save.data = TRUE)

plot(q, project = prcomp(M1_std), asp=1, simlines=FALSE, points = FALSE)
text(M1_std, labels = rownames(M1_std), cex = 0.7)

barplot(q)

