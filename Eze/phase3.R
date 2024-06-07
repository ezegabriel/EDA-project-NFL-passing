# Hello World !!!
nfl_passing <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nfl_passing.csv")


nfl_passing |> 
  filter(!is.na(pass_length), !is.na(complete_pass)) |> 
  ggplot(aes(y=complete_pass, x=pass_length))+
  geom_col()

names(nfl_passing)

nfl_passing |> 
  group_by(passer_player_name) |> 
  table(interception)


nfl_passing |> 
  ggplot(aes(y=wpa, x=epa))+
  geom_point(alpha=.5, color='darkblue')+
  geom_smooth()+
  coord_flip() 
