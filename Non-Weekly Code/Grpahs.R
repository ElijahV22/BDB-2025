library(tidyverse)
library(gtExtras)
data <- read.csv("comp_distances.csv")

### SETTING UP DATA -----------
 # The best offenses at creating yards on these type of plays 
best_off <- data %>% 
  dplyr::select(teamAbbr_receiver, exp_vaue_ou) %>% 
  group_by(teamAbbr_receiver) %>% 
  summarize(
    by_play = round(sum(exp_vaue_ou) / n(), 2),    # average for each group
    overall = round(sum(exp_vaue_ou), 2),           # total for each group
    n = n()
  ) %>% 
  arrange(-by_play)

# worst offenses
worst_off <- best_off %>% 
  arrange(by_play)

 # The best defenses at preventing yards on these type of plays 
worst_def <- data %>% 
  dplyr::select(teamAbbr_defender, exp_vaue_ou) %>% 
  group_by(teamAbbr_defender) %>% 
  summarize(
    by_play = round(sum(exp_vaue_ou) / n(), 2),    # average for each group
    overall = round(sum(exp_vaue_ou), 2),           # total for each group
    n = n()
  ) %>% 
  arrange(-by_play)

# worst defenses
best_def <- worst_def %>% 
  arrange(by_play)
 

### TABLES-----------

# ALL TABLES WERE MADE WITH THE HELP OF BRAD CONGELIO'S BOOK, INTRODUCTION TO NFL ANALYTICS WITH R 

 # table for the best offenses by play 
best_off_table <- best_off %>%
  slice(1:10) %>%  
  gt() %>%  
  tab_header(
    title = md("Top 10 Teams in Yards Created Over Expected"),
    subtitle = md("2022 Season Weeks 1-9")
  ) %>%
  cols_label(
    teamAbbr_receiver = "Team",
    by_play = "Per Play",
    overall = "Total ",
    n = "Plays"
  ) %>%
  data_color(
    columns = by_play,
    colors = scales::col_numeric(
      palette = c("lightblue", "blue"),
      domain = NULL  # Automatically scales to the column's range
    ))



 # table for the worst offenses by play 
worst_off_table <- worst_off %>%
  slice(1:10) %>%  
  gt() %>%  
  tab_header(
    title = md("Bottom 10 Teams in Yards Created Over Expected"),
    subtitle = md("2022 Season Weeks 1-9")
  ) %>%
  cols_label(
    teamAbbr_receiver = "Team",
    by_play = "Per Play",
    overall = "Total ",
    n = "Plays"
  ) %>%
  data_color(
    columns = by_play,
    colors = scales::col_numeric(
      palette = c("red", "lightcoral"),
      domain = NULL  # Automatically scales to the column's range
    ))



 # table for the best defenses by play 
best_def_table <- best_def %>%
  slice(1:10) %>%  
  gt() %>%  
  tab_header(
    title = md("Top 10 Defenses Allowing Yards Created Over Expected"),
    subtitle = md("2022 Season Weeks 1-9")
  ) %>%
  cols_label(
    teamAbbr_defender = "Team",
    by_play = "Per Play",
    overall = "Total ",
    n = "Plays"
  ) %>%
  data_color(
    columns = by_play,
    colors = scales::col_numeric(
      palette = c("blue", "lightblue"),
      domain = NULL  # Automatically scales to the column's range
    ))



 # table for worst defenses by play 
worst_def_table <- worst_def %>%
  slice(1:10) %>%  
  gt() %>%  
  tab_header(
    title = md("Bottom 10 Defenses Allowing Yards Created Over Expected"),
    subtitle = md("2022 Season Weeks 1-9")
  ) %>%
  cols_label(
    teamAbbr_defender = "Team",
    by_play = "Per Play",
    overall = "Total ",
    n = "Plays"
  ) %>%
  data_color(
    columns = by_play,
    colors = scales::col_numeric(
      palette = c("lightcoral", "red"),
      domain = NULL  # Automatically scales to the column's range
    ))
gtsave(best_off_table, "best_off_table.png")
gtsave(worst_off_table, "worst_off_table.png")
gtsave(best_def_table, "best_def_table.png")
gtsave(worst_def_table, "worst_def_table.png")
