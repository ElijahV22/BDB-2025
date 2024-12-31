week_one <- read.csv("tracking_week_4.csv")
player_play <- read.csv("player_play.csv")

# FOR WEEK FOUR 

# variables we don't need 
week_one  <- week_one  %>%
  select(-playDirection, -s, -a, -dis, -o, -dir) 

# adding in the player play data 
week_one  <- week_one  %>% 
  left_join(player_play, by = c("gameId", "playId", "nflId"))

# filtering to what we need 
week_one_filtered <- week_one %>% 
  select(gameId, playId, nflId, displayName, frameId, club, teamAbbr, 
         x, y, event, wasTargettedReceiver, inMotionAtBallSnap, 
         pff_primaryDefensiveCoverageMatchupNflId) 

# plays where at least one player was in motion at the snap
w1_motion_plays <- week_one_filtered %>%
  filter(inMotionAtBallSnap == TRUE) %>%
  select(gameId, playId) %>%
  distinct()

# plays where any receiver was targeted
w1_targeted_plays <- week_one_filtered %>%
  filter(wasTargettedReceiver == 1) %>%
  select(gameId, playId) %>%
  distinct()

# plays with motion and a targeted receiver 
w1_valid_plays <- w1_motion_plays %>%
  inner_join(w1_targeted_plays, by = c("gameId", "playId"))

# all valid plays
final_w1_plays <- week_one_filtered %>%
  semi_join(w1_valid_plays, by = c("gameId", "playId"))

# targeted receivers
w1_targeted_receivers <- final_w1_plays %>% 
  filter(wasTargettedReceiver == 1) %>% 
  select(gameId, playId, nflId) %>% 
  distinct(gameId, playId, .keep_all = TRUE) %>% 
  rename("targetId" = "nflId")

# motion men
w1_motion_men <- final_w1_plays %>%
  filter(inMotionAtBallSnap == TRUE) %>%
  select(gameId, playId, nflId) %>%
  distinct(gameId, playId, .keep_all = TRUE) %>% 
  rename("motionId" = "nflId")

# targeted defenders 
w1_targeted_defenders <- final_w1_plays %>% 
  select(gameId, playId, nflId, 
         pff_primaryDefensiveCoverageMatchupNflId) %>% 
  filter(complete.cases(.)) %>% 
  distinct(gameId, playId, nflId, .keep_all = TRUE) %>% 
  rename("deftargetId" = "pff_primaryDefensiveCoverageMatchupNflId", 
         "defenderId" = "nflId") %>% 
  select(gameId, playId, defenderId, deftargetId)

# combining all players of interest 
final_w1_players <- merge(w1_targeted_receivers, w1_motion_men,
                          by = c("gameId", "playId"))

# combining all of it 
final_w1_players <- final_w1_players %>% 
  left_join(w1_targeted_defenders, by = c("gameId", "playId",
                                          "targetId" = "deftargetId"))

# filtering to only plays with a primary defender 
final_w1_players <- final_w1_players %>% 
  select(gameId, playId, targetId, motionId, defenderId) %>% 
  filter(complete.cases(.))

# selecting from the original filtered data set
test_final_w1_plays <- final_w1_plays %>% 
  left_join(final_w1_players, by = c("gameId", "playId")) %>% 
  mutate(inMotionAtBallSnap = ifelse(inMotionAtBallSnap == TRUE, nflId, inMotionAtBallSnap)) %>% 
  mutate(wasTargettedReceiver = ifelse(wasTargettedReceiver == 1, nflId, wasTargettedReceiver))

# filtering to only the players we want 
test_final_w1_plays <- test_final_w1_plays %>% 
  filter(nflId == wasTargettedReceiver | nflId == defenderId | motionId == inMotionAtBallSnap)

# filtering to when the ball gets thrown 
test_final_w1_plays <- test_final_w1_plays %>% 
  filter(event == "pass_forward", !is.na(defenderId), # at time pass is forward, getting rid of duplicates
         inMotionAtBallSnap != wasTargettedReceiver | pff_primaryDefensiveCoverageMatchupNflId != "NA") # no plays where motion man caught the ball 

# FIRST FILTER
filtering <- test_final_w1_plays %>% 
  filter(wasTargettedReceiver != 0 |
           pff_primaryDefensiveCoverageMatchupNflId != "NA")


# define a distance function

distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# finding the distances of every player from every other player 
distances_df <- filtering %>% 
  select(gameId, playId, nflId, x, y) %>% 
  left_join(filtering, by = c("gameId", "playId"), 
            suffix = c("_player1", "_player2")) %>% 
  mutate(distance = mapply(distance, x1 = x_player1, y1 = y_player1, 
                           x2 = x_player2, y2 = y_player2)) %>% 
  filter(nflId_player1 != nflId_player2) %>%
  select(playId, nflId_player1, nflId_player2, distance)

# joining so that we should have the distances from the primary defenders to the target only 
filtering <- filtering %>% 
  left_join(distances_df, by = c("nflId" = "nflId_player1"
                                 ,"pff_primaryDefensiveCoverageMatchupNflId" = "nflId_player2")) %>% 
  distinct()

# unique id for each play 
filtering <- filtering %>%
  group_by(gameId, playId.x) %>% 
  mutate(uniqueId = cur_group_id()) %>% 
  ungroup()

# closest defender to the receiver when the qb is throwing
# we're calling this man the primary defender 
filtering <- filtering %>%
  group_by(uniqueId) %>%  
  filter(distance != max(distance, na.rm = TRUE)) %>% 
  ungroup()



# every play now has 3 players corresponding to what we want 
week1_final <- test_final_w1_plays %>% 
  semi_join(filtering, by = c("defenderId" = "nflId")) 

# taking the groups that joined incorrectly
# will just manually fix 
week1_removal <- week1_final %>%
  group_by(gameId, playId) %>% 
  filter(n() == 6) %>% 
  ungroup()

# removing groups that joined incorrectly, will add back later 
week1_final <- week1_final %>% 
  group_by(gameId, playId) %>% 
  filter(n() != 6) %>% 
  ungroup()

# getting rid of accidental duplicates created 
week1_final <- week1_final %>% 
  group_by(gameId, playId) %>% 
  filter(n() != 1) %>% 
  ungroup()

week1_removal <- week1_removal[-c(3, 5, 6, 8, 9, 11, 
                                  13, 16, 17, 21, 22, 24,
                                  26, 27, 28, 31, 34, 36), ]

# adding back wanted observations 
week1_final <- week1_final %>% 
  rbind(week1_removal)

# creating roles for every player
week1_final <- week1_final %>% 
  mutate(role = case_when(
    wasTargettedReceiver != 0 ~"receiver",
    pff_primaryDefensiveCoverageMatchupNflId != "NA" ~ "defender",
    inMotionAtBallSnap != 0 ~ "motion",
    inMotionAtBallSnap != "NA" ~ "motion",
  ))

# selecting only the needed columns
week1_final <- week1_final %>% 
  select(gameId, playId, nflId, teamAbbr, role, x, y) 

# one observation per play 
week1_final <- week1_final %>% 
  pivot_wider(
    names_from = role,
    values_from = c(x, y, nflId, teamAbbr))

write.csv(week1_final, file = "week4.csv", row.names = TRUE)
