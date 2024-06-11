library(ape)
library(MASS)
library(rgl)
library(igraph)
library(flexclust)
library(pald)
library(cluster.datasets)

# Build the data set
library(tidyverse)
nfl_passing <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nfl_passing.csv")


# Data cleaning 
passing_clean <- na.omit(nfl_passing)



# Visualizations
library(ggplot2)

## Complete-passing rate for each team -- Teams with high complete passing rate? 
nfl_passing |> 
  group_by(posteam) |> 
  summarize(total_pass = n(), num_complete = sum(complete_pass, na.rm = TRUE), complete_rate = num_complete / total_pass) |> 
  ggplot(aes(x = reorder(posteam, -complete_rate), y = complete_rate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Complete-passing Rates",
       x = "Team",
       y = "Complete-passing Rate")

## Interception rate for each team -- Teams with high interception rate?
nfl_passing |>
  group_by(posteam) |>
  summarize(total_pass = n(), num_interception = sum(interception, na.rm = TRUE),
            interception_rate = num_interception / total_pass) |>
  ggplot(aes(x = reorder(posteam, -interception_rate), y = interception_rate)) +
  geom_bar(stat = "identity", fill = "darkgreen") + 
  labs(title = "Interception Rates",
       x = "Team",
       y = "Interception Rate") +
  theme_minimal()

## Effect of pass length and pass location on complete-passing rate
nfl_passing |>
  drop_na(pass_location, pass_length) |>
  group_by(pass_location, pass_length) |>
  summarize(complete_rate = mean(complete_pass, na.rm = TRUE)) |>
  ggplot(aes(x = pass_length, y = complete_rate, fill = pass_location)) +
  geom_col() +
  labs(title = "Complete-passing Rate by Pass Length and Pass Location",
       x = "Pass Length",
       y = "Completion Rate")

## EPA by team and pass completion status
nfl_passing |>
  group_by(posteam, complete_pass) |>
  summarize(avg_epa = mean(epa, na.rm = TRUE)) |> 
  ggplot(aes(x = reorder(posteam, -avg_epa), y = avg_epa, fill = factor(complete_pass))) +
  geom_col() +
  coord_flip() +
  labs(title = "Average EPA by Team and Pass Completion Status",
       x = "Team",
       y = "Average EPA",
       fill = "Complete Pass")
  

  
# Clustering 

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
            sack_rate = mean(sack, na.rm = TRUE)
            )
  
M1 <- M1[, -c(2,3,5)] 
rownames(M1) <- M1$posteam
M1_revised <- M1[, -1]

# standardizing M1
M1_std <-scale(M1_revised, scale = TRUE)[,]; head(M1_std)
rownames(M1_std) <- M1$posteam
D <- as.matrix(dist(M1_std)); head(D)

## clustering analysis
# plot(stepFlexclust(M1_std, nrep = 20, k = 2:10), type = "l")
#a<- 47; while(a > 43){q <- cclust(M1_std, k = 5, save.data = TRUE); a <- info(q, "distsum")}; sort(clusters(q)); a

clusters_complete <- cutree(hclust(as.dist(D), method = "complete"), 4); clusters_complete

q <- cclust(M1_std, k = 5, save.data = TRUE)

plot(q, project = prcomp(M1_std), asp=1, simlines=FALSE, points = FALSE)
text(M1_std, labels = rownames(M1_std), cex = 0.7)

barplot(q)


# clustering with dendrograms








