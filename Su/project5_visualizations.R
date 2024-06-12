# Load the data set
library(tidyverse)
nfl_passing <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nfl_passing.csv")
spec(nfl_passing)

# exploring the dataset
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


# Data Visualization

## visualization 1 (categorical)
## Effect of pass length and pass location on complete-passing rate
nfl_passing |>
  drop_na(pass_location, pass_length) |>
  group_by(pass_location, pass_length) |>
  summarize(complete_rate = mean(complete_pass, na.rm = TRUE)) |>
  ggplot(aes(x = pass_length, y = complete_rate, fill = pass_location)) +
  geom_col(position = "fill") +
  labs(title = "Complete-passing Rate by Pass Length and Pass Location",
       x = "Pass Length",
       y = "Completion Rate")

## or
nfl_passing |>
  drop_na(pass_location, pass_length) |>
  group_by(pass_location, pass_length) |>
  summarize(freq = sum(complete_pass, na.rm = TRUE),
            complete_rate = mean(complete_pass, na.rm = TRUE)) |>
  ggplot(aes(x = pass_length, y = pass_location)) +
  geom_tile(aes(fill = freq), color = "white") +
  geom_text(aes(label = scales::percent(complete_rate))) +
  scale_fill_gradient2() +
  labs(title = "Complete-passing Rate by Pass Length and Pass Location",
       x = "Pass Length",
       y = "Pass Location")



## Visualization 2 (categorical)
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


## Visualization 3 (categorical)
nfl_passing |>
  select(touchdown, complete_pass) |> 
  table() |> 
  mosaicplot(main = "Relationship between Pass Completion and Touchdown")


## Visualization 4 (continuous)
nfl_passing |> 
  group_by(posteam) |> 
  summarize(avg_yards_gained = mean(yards_gained), 
            avg_epa = mean(epa)) |> 
  ggplot(aes(x = avg_yards_gained, y = avg_epa)) +
  geom_point(size = 2.5, alpha = 0.5) + 
  geom_density2d() +
  theme(legend.position = "bottom")


nfl_passing |> 
  group_by(posteam) |> 
  summarize(avg_yards_gained = mean(yards_gained), 
            avg_epa = mean(epa)) |> 
  ggplot(aes(x = avg_yards_gained, y = avg_epa)) + 
  stat_density2d(aes(fill = after_stat(level)),
                 h = 0.2, bins = 10, geom = "polygon") +
  scale_fill_gradient(low = "lightblue", 
                      high = "purple") +
  theme(legend.position = "bottom")



## Visualization 5 (continuous)
nfl_passing |> 
  group_by(passer_player_name) |> 
  summarize(total_passes = n(),
            avg_yards = mean(yards_gained))|> 
  ggplot(aes(x = total_passes, y = avg_yards)) +
  geom_point(size = 2.5, alpha = 0.5) + 
  geom_density2d() +
  theme(legend.position = "bottom")

nfl_passing |> 
  group_by(passer_player_name) |> 
  summarize(com_passes = sum(complete_pass == 1),
            avg_yards = mean(yards_gained))|> 
  ggplot(aes(x = com_passes, y = avg_yards)) +
  geom_point(size = 2.5, alpha = 0.5) + 
  geom_density2d() +
  theme(legend.position = "bottom")







