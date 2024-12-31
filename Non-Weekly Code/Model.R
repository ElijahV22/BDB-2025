library(tidyverse)
library(pracma)
library(purrr)
library(MASS)
library(cowplot)
library(fitdistrplus)
library(ggplot2)
### LINEAR ALGEBRA ---------------------
 # list of file names
file_names <- paste0("week", 1:9, ".csv")
data_list <- lapply(file_names, read.csv)

 # all motion plays where a player was in motion at the snap of the ball, weeks 1-9 for 2022 season 
 # PASSING PLAYS ONLY 
combined_data <- do.call(rbind, data_list)

 # Finding the distance from the distance from defender to receiver when the ball is thrown
 # Relate the distance from defender to the motion man using an orthogonal projection onto the motion man and receiver vector 

distances <- combined_data %>% 
  filter(complete.cases(.))

 # vector from motion man to receiver
distances$vector_motion_to_receiver <- mapply(
  function(x, y, mx, my) c(x - mx, y - my),
  distances$x_receiver, distances$y_receiver, distances$x_motion, distances$y_motion,
  SIMPLIFY = FALSE
)

 # vector from motion man to defender 
distances$vector_motion_to_defender <- mapply(
  function(x, y, mx, my) c(x - mx, y - my),
  distances$x_defender, distances$y_defender, distances$x_motion, distances$y_motion,
  SIMPLIFY = FALSE
)

# orthogonal projection of motion to defender onto motion to receiver 
distances <- distances %>%
  rename(v1 = vector_motion_to_defender, v2 = vector_motion_to_receiver) %>%
  mutate(
    projection = pmap(
      list(v1, v2),
      function(v1, v2) {
        # calculate scalar projection of v1 onto v2
        scalar_proj <- dot(v1, v2) / dot(v2, v2)
        # multiply by v2 to get the projection vector
        scalar_proj * v2
      }
    )
  )

 # the distance from the defender to the receiver relative to the motion man 
distances$distance <- mapply(
  function(v2, proj) {
    # magnitudes of the two vectors 
    e_v2 = Norm(v2)
    e_proj = Norm(proj)
    # distance from projection of defender to receiver 
    return(e_v2 - e_proj)
  },
  distances$v2, distances$projection
)


distances <- distances %>% 
  dplyr::select(gameId, playId, teamAbbr_receiver, teamAbbr_defender,
         distance, nflId_motion, nflId_receiver, nflId_defender ) %>% 
  mutate(distance = abs(distance))


### DEFINING THE MODEL -----------------------

 # histogram of distances 
hist <- ggplot(distances, aes(x = distance)) +
  geom_histogram(binwidth = 1, bins = 35)
hist

 # density plot of distances
density <- ggplot(distances, aes(x = distance)) +
  geom_density()
density


 # model this data with a gamma distribution 
 # the shape and scale parameters will just be modeled using very basic MASS package 
set.seed(22)  
x <- distances$distance # distances data 

fitdistr(x, "gamma", start=list(shape=1, rate=1))$estimate

shape2 <- 1.12 # alpha, scale
rate = .429 # rate 

# modeling our distribution 
model <- dgamma(x, shape = shape2, rate = rate)

 # a plot of our gamma distribution 
gamma <- ggplot(data.frame(x, model), aes(x, model)) +
  geom_line()

 # plotting the distributions side by side 
distributions <- plot_grid(density, gamma, ncol = 2) 

 # not perfect, but fairly good for what we need 
 
 # CDF of the model 
mod_cdf <- pgamma(x, shape = shape2, rate = rate)

 # plot of CDF 
gamma_cdf <- ggplot(data.frame(x, mod_cdf), aes(x, mod_cdf)) +
  geom_line()
### GETTING READY FOR COMPARISONS WITH THE MODEL ---------

x <- distances$distance # distances data 
model <- dgamma(x, shape = shape2, rate = rate)

 # expected value for our gamma distribution 
exp_value <- shape2*(1/rate)

 # expected value across the board
distances$expected <- exp_value

 # amount off expected value 
distances$exp_vaue_ou <- distances$distance - distances$expected

### MAKING THE DISTRIBUTIONS GRAPH---------------
 
 # nice graph for density
density_graph <- ggplot(distances, aes(x = distance)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(
    title = "Density Plot",
    x = "Values",
    y = "Density"
  ) +
  scale_y_continuous(
    limits = c(0, 0.3),  
    breaks = seq(0, 0.3, by = 0.1) 
  ) +
  theme_minimal()

 # nice graph for gamma distribution 
gamma_graph <- ggplot(data.frame(x, model), aes(x, model)) +
  geom_line(color = "red", size = 1) +  
  labs(
    title = "Gamma Distribution",
    subtitle = " Shape = 1.12, Rate = .429",
    x = "Values",
    y = "Density"
  ) +
  theme_minimal()


distributions2 <- plot_grid(density_graph, gamma_graph, ncol = 2)

distributions2


write.csv(distances, file = "comp_distances.csv", row.names = TRUE)
ggsave("distributions.png", plot = distributions2, width = 10, height = 6)
