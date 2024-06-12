# Hello World !!!
nfl_passing <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nfl_passing.csv")

# Set theme
theme_set(theme_light())


# Viz 1: Completion Rate by Pass Location & Length

# Calculate completion rate by pass_location and length
completion_rate <- nfl_passing |> 
  filter(!is.na(pass_location), !is.na(pass_length)) |> 
  group_by(pass_location, pass_length) |> 
  summarize(completions = sum(complete_pass, na.rm = T), 
            attempts = n(),
            completion_rate = completions/attempts) |> 
  ungroup()


# Plotting
completion_rate |> 
  ggplot(aes(pass_length, completion_rate, fill=pass_location))+
  geom_bar(stat='identity', position = 'dodge')+
  labs(title = 'Completion Rate by Pass Location & Length',
       x = 'Pass Length',
       y='Completion Rate',
       fill='Pass Location')+
  theme(plot.title = element_text(hjust = .5, face='bold'),
        legend.position = 'bottom')



# Viz 2: Expected Points Added (EPA) by Down & Shotgun Formation

# Calculate average EPA by down and shotgun
epa_by_down_shotgun <- nfl_passing |> 
  filter(!is.na(down), !is.na(shotgun)) |> 
  group_by(down, shotgun) |> 
  summarize(avg_epa = mean(epa, na.rm = T)) |> 
  ungroup()

# Convert shotgun to factor to enhance customization on a discrete scale
epa_by_down_shotgun <- epa_by_down_shotgun |> 
  mutate(shotgun = as.factor(shotgun))

# Plotting
epa_by_down_shotgun |> 
  ggplot(aes(x=factor(down), y=avg_epa, fill=shotgun))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values = c('0' = 'gold', '1' = 'midnightblue'), labels = c('No Shotgun', 'Shotgun'))+
  labs(title = 'Average EPA by Down & Shotgun Formation',
       x='Down',
       y='Average EPA',
       fill='Shotgun Formation')+
  coord_flip()+
  facet_grid(down~., 
             scales = 'free_y', 
             space = 'free',
             labeller = labeller(down = c('1' = "1st Down", '2' = '2nd Down', '3' = '3rd Down', '4' = '4th Down'))) +
  theme(plot.title = element_text(hjust = .5, face='bold'),
        panel.spacing = unit(.5, 'lines'),
        legend.position = 'bottom'
  )



# Viz 3: Change in Score Differential by Time Remaining

library(dplyr)
library(ggplot2)


# Prepare the data
score_diff_data <- nfl_passing |>
  filter(!is.na(half_seconds_remaining), 
         !is.na(home_score), 
         !is.na(away_score), 
         !is.na(posteam_type), 
         !is.na(game_half)) |>
  mutate(
    score_differential = ifelse(posteam_type == "home", home_score - away_score, away_score - home_score),
    score_differential_change = score_differential - lag(score_differential, default = first(score_differential)), # Tally & Calculates the change in score differential from the preceeding play
    reversed_half_seconds_remaining = max(half_seconds_remaining) - half_seconds_remaining # Reverse time variable to count down instead of up instead of up
  ) |>
  filter(!is.na(score_differential_change))


# Plotting
score_diff_data |> 
  ggplot(aes(x = reversed_half_seconds_remaining,
             y = score_differential_change,
             color = game_half))+
  geom_smooth(method = "loess", se = F) +
  geom_hline(yintercept = 0,
             linetype = 'dashed',
             color = 'black')+
  scale_color_manual(values = c("Half1" = "blue", "Half2" = "green", "Overtime" = "red"))+
  labs(title = "Average Change in Score Differential by Time Remaining",
       x = "Time in Half (seconds remaining)",
       y = "Change in Score Differential",
       color = "Game Half",
       fill = 'Score Differential') +
  scale_x_reverse() +
  scale_y_continuous(breaks = seq(-1,4, by = 1))+
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(hjust = 0, size = 13, face = "bold")
  )
  