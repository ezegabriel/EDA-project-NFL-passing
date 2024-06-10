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
  theme(plot.title = element_text(hjust = .5, face='bold'))



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
        panel.spacing = unit(.5, 'lines')
  )








