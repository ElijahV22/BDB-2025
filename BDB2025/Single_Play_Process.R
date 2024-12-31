library(tidyverse)
library(pracma)
# lets look at a play from week two, https://www.youtube.com/watch?v=zzT0P1L3uAA
# 6:45 time stamp 

 # reading in the data
week_two <- read.csv("tracking_week_2.csv")
player_play <- read.csv("player_play.csv")

### FINDING THE PLAY ----


 # joining the data 
combined_data <- week_two %>%
  left_join(player_play, by = c("gameId", "playId", "nflId"))


 # filtering to find the motivating play 
 # Looking to find plays where Brandon Powell went in motion and Cooper Kupp was the targeted receiver
finding_play <- combined_data %>% 
  group_by(playId) %>%
  filter(
    any(displayName == "Brandon Powell" & inMotionAtBallSnap == TRUE),  # Brandon Powell is in motion
    any(displayName == "Cooper Kupp" & wasTargettedReceiver == 1)        # Cooper Kupp is the target
  )


 # play id 2039
summary(finding_play$playId) 

# this is the only play where both event occurred, as seen by above summary  


### THE PLAY ----

 # Grabbing the play 


play <- combined_data %>% 
  filter(gameId == 2022091807, playId == 2039, 
         inMotionAtBallSnap == TRUE | # the motion man 
         wasTargettedReceiver == TRUE | # the targeted receiver 
         pff_primaryDefensiveCoverageMatchupNflId == 44881) # the primary defender of targeted recevier 

### LINERAR ALGEBRA----

 # all math will be done at the point of release by the quarterback 
released <- play %>% 
  filter(event == "pass_forward") %>% 
  select(playId, displayName, nflId, x, y, 
         inMotionAtBallSnap, wasTargettedReceiver,
         pff_primaryDefensiveCoverageMatchupNflId)

 # motion man coordinates
motion_player <- released %>%
  filter(inMotionAtBallSnap == TRUE) %>%
  select(playId, motion_x = x, motion_y = y)

 # targeted receiver coordinates
targeted_receiver <- released %>%
  filter(wasTargettedReceiver == 1) %>%
  select(playId, target_x = x, target_y = y)

 # primary defender coordinates
primary_defender <- released %>%
  filter(pff_primaryDefensiveCoverageMatchupNflId == 44881) %>% 
  select(playId, defender_x = x, defender_y = y)

 # Join the data together by playId
combined_play_coords <- motion_player %>%
  left_join(targeted_receiver, by = "playId") %>%
  left_join(primary_defender, by = "playId")

 # calculate the vectors
combined_vectors <- combined_play_coords %>%
  mutate(
    vector_motion_to_target_x = target_x - motion_x,
    vector_motion_to_target_y = target_y - motion_y,
    vector_motion_to_defender_x = defender_x - motion_x,
    vector_motion_to_defender_y = defender_y - motion_y
  ) %>% 
  select(vector_motion_to_target_x,
         vector_motion_to_target_y,
         vector_motion_to_defender_x, 
         vector_motion_to_defender_y)

 # our two vectors 
v1 <- c(combined_vectors$vector_motion_to_defender_x, combined_vectors$vector_motion_to_defender_y)
v2 <- c(combined_vectors$vector_motion_to_target_x, combined_vectors$vector_motion_to_target_y)

 # orthogonal projection of v1 onto v2 
dot_both <- dot(v1, v2)
dot_v2 <- dot(v2, v2)
v3 <- (dot_both / dot_v2)*v2


 # distance from defender to target, in relation to motion man 
combined_vectors$distance <- Norm(v2) - Norm(v3)
combined_vectors$distance
 # motion created roughly 3 yards of separation at the time of the throw 



### DATA VIZ OF THE PLAY ---------------

play_viz <- combined_data %>% 
  filter(gameId == 2022091807, playId == 2039)

field_plot <- ggplot() +
  ggfootball::geom_field() +
  theme_minimal() +
  theme(panel.grid = element_blank())