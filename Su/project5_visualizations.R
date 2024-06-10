library(ape)
library(MASS)
library(rgl)
library(igraph)
library(flexclust)
library(pald)
library(cluster.datasets)

# Build the data set
library(tidyverse)
library(nflfastR) # install.packages("nflverse")
nfl_pbp <- load_pbp(2023)
nfl_passing <- nfl_pbp |> 
  filter(play_type == "pass", season_type == "REG", 
         !is.na(epa), !is.na(posteam), posteam != "") |> 
  select(# player info attempting the pass
    passer_player_name, passer_player_id, posteam,
    # info about the pass:
    complete_pass, interception, yards_gained, touchdown,
    pass_location, pass_length, air_yards, yards_after_catch, epa, wpa,
    shotgun, no_huddle, qb_dropback, qb_hit, sack,
    # context about the receiver:
    receiver_player_name, receiver_player_id,
    # team context
    posteam, defteam, posteam_type,
    # play and game context
    play_id, yardline_100, side_of_field, down, qtr, play_clock,
    half_seconds_remaining, game_half, game_id,
    home_team, away_team, home_score, away_score,
    # description of play
    desc)


# Data cleaning 
passing_clean <- na.omit(nfl_passing)



# Visualizations
library(ggplot2)

## Complete-passing rate for each team -- Teams with high complete passing rate? 
nfl_passing |> 
  select(passer_player_name, passer_player_id, posteam, posteam_type, complete_pass, interception) |> 
  group_by(posteam) |> 
  summarize(total_pass = n(), num_complete = sum(complete_pass, na.rm = TRUE), complete_rate = num_complete / total_pass) |> 
  ggplot(aes(x = reorder(posteam, -complete_rate), y = complete_rate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Complete-passing Rates",
       x = "Team",
       y = "Complete-passing Rate")

## Interception rate for each team -- Teams with high interception rate?
nfl_passing |>
  select(passer_player_name, passer_player_id, posteam, posteam_type, complete_pass, interception) |> 
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
plot(stepFlexclust(M1_std, nrep = 20, k = 2:10), type = "l")
a<- 47; while(a > 43){q <- cclust(M1_std, k = 5, save.data = TRUE); a <- info(q, "distsum")}; sort(clusters(q)); a

plot(q, project = prcomp(M1_std), asp=1, simlines=FALSE, points = FALSE)
text(M1_std, labels = rownames(M1_std), cex = 0.7)

barplot(q)









