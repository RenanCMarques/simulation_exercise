################################################################################
# Simulation Exercise
################################################################################

#Loading data wrangling and visualization packages for efficiency and aesthetics purposes
library(dplyr)
library(ggplot2)
library(ggrepel) 

################################################################################
# Task (1) 
################################################################################

#Using set.seed to make the results reproducible
set.seed(1)

#Creating "points": a function which generates randomly placed points within a unit square and names each observation (id)
points <- function(sample_size){
  table <- data.frame(
    id = c(1:sample_size),
    x = runif(sample_size, min = 0, max = 1),
    y = runif(sample_size, min = 0, max = 1))
  return(table)
}

#Generating random sample of n=10 points and naming it sample10
sample10 <- points(10)

#Plotting sample10 with ggplot2 and ggrepel
ggplot(sample10, aes(x=x, y=y)) +
  geom_point() +
  geom_text_repel(aes(label = id), size = 4) +
  ggtitle("Randomly placed n=10 points within a unit square")

################################################################################
# Task (2) 
################################################################################

#Creating two functions: (i) max_dist and (ii) min_dist which return the max and min distance between points of a given random sample 
#Two step rationale behind the functions (i) and (ii): 
#1st: creating a key (constant n=1) to merge sample on itself and get all possible combinations of the points
#2nd: merging sample, creating a distance (dist) variable, removing the duplicates and finding the obs with max and min dist 
max_dist <- function(sample){
   samplef <- sample %>% mutate(n = 1)
   max <- samplef %>%
     full_join(samplef, by = "n") %>% 
     filter(id.x != id.y) %>%
     mutate(dist = sqrt((x.x - x.y)^2 + (y.x - y.y)^2)) %>%
     select(id.x, id.y, dist, x.x, y.x, x.y, y.y) %>%
     filter(duplicated(dist) == FALSE) %>%
     slice_max(dist)
   return(max)
}

min_dist <- function(sample){
  samplef <- sample %>% mutate(n = 1)
  min <- samplef %>%
    full_join(samplef, by = "n") %>% 
    filter(id.x != id.y) %>%
    mutate(dist = sqrt((x.x - x.y)^2 + (y.x - y.y)^2)) %>%
    select(id.x, id.y, dist, x.x, y.x, x.y, y.y) %>%
    filter(duplicated(dist) == FALSE) %>%
    slice_min(dist)
  return(min)
}

#Finding the farthest points and the distance (dist) between them
max_dist(sample10)

#Finding the closest points and the distance (dist) between them
min_dist(sample10)

################################################################################
# Task (3) 
################################################################################

#Using set.seed to make the script and results reproducible
set.seed(1)

#Generating 50 random samples of n=10 points within a unit square
sample10x50 <- replicate(50, points(10), simplify = FALSE)

#Generating the farthest and closest pairs of the 50 random samples
sample10x50_max <- lapply(sample10x50, max_dist) 
sample10x50_min <- lapply(sample10x50, min_dist)

#Transforming the two lists (sample10x50_max and sample10x50_min) into data.frames
sample10x50_max <- do.call(rbind.data.frame, sample10x50_max)
sample10x50_min <- do.call(rbind.data.frame, sample10x50_min)

#Creating a variable to identify if farthest (1) or closest (0) pairs and binding both data.frames to create a unique one
sample10x50_max <- sample10x50_max %>%
  mutate(identifier = 1)

sample10x50_min <- sample10x50_min %>%
  mutate(identifier = 0)

sample10x50_maxmin <- bind_rows(sample10x50_max, sample10x50_min) %>%
  select(dist, identifier, x.x, y.x, x.y, y.y)

#Calculating average distance in farthest-pairs and closest-pairs
sample10x50_maxmin %>%
  group_by(identifier) %>%
  summarise(avg = mean(dist))

#Plotting results
ggplot(sample10x50_maxmin, aes(x.x, y.x, color = ifelse(identifier == 1, "Farthest Pairs", "Closest Pairs"))) +
  geom_point() +
  geom_point(aes(x.y, y.y, color = ifelse(identifier == 1, "Farthest Pairs", "Closest Pairs"))) +
  scale_color_manual(breaks = c("Farthest Pairs", "Closest Pairs"),
                     values = c("red", "blue")) +
  labs(title = "Farthest and Closest pairs of 50 random samples",
       subtitle = "Each sample consists of 10 random points within a unit square",
       x = "",
       y = "",
       color = "",
       caption = "Average farthest-pairs distance = 0.991
                  Average closest-pairs distance = 0.088")
  
################################################################################
# Task (4) 
################################################################################

#Using set.seed to make the results reproducible
set.seed(1)

#Generating random sample of n=20 points and naming it sample20
sample20 <- points(20)

#Plotting sample20 with ggplot2 and ggrepel
ggplot(sample20, aes(x=x, y=y)) +
  geom_point() +
  geom_text_repel(aes(label = id), size = 4) +
  ggtitle("Randomly placed n=20 points within a unit square")

#Finding the farthest points and the distance between them using the function created in task (2)
max_dist(sample20)

#Finding the closest points and the distance between them using the function created in task (2)
min_dist(sample20)

#Generating 50 random samples of n=20 points within a unit square
sample20x50 <- replicate(50, points(20), simplify = FALSE)

#Generating the farthest and closest pairs of the 50 random samples
sample20x50_max <- lapply(sample20x50, max_dist) 
sample20x50_min <- lapply(sample20x50, min_dist)

#Transforming the two lists (sample20x50_max and sample20x50_min) into data.frames
sample20x50_max <- do.call(rbind.data.frame, sample20x50_max)
sample20x50_min <- do.call(rbind.data.frame, sample20x50_min)

#Creating a variable to identify if farthest (1) or closest (0) pairs and binding both data.frames to create a unique one
sample20x50_max <- sample20x50_max %>%
  mutate(identifier = 1)

sample20x50_min <- sample20x50_min %>%
  mutate(identifier = 0)

sample20x50_maxmin <- bind_rows(sample20x50_max, sample20x50_min) %>%
  select(dist, identifier, x.x, y.x, x.y, y.y)

#Calculating average distance in farthest-pairs and closest-pairs
sample20x50_maxmin %>%
  group_by(identifier) %>%
  summarise(avg = mean(dist))

#Plotting results
ggplot(sample20x50_maxmin, aes(x.x, y.x, color = ifelse(identifier == 1, "Farthest Pairs", "Closest Pairs"))) +
  geom_point() +
  geom_point(aes(x.y, y.y, color = ifelse(identifier == 1, "Farthest Pairs", "Closest Pairs"))) +
  scale_color_manual(breaks = c("Farthest Pairs", "Closest Pairs"),
                     values = c("red", "blue")) +
  labs(title = "Farthest and Closest pairs of 50 random samples",
       subtitle = "Each sample consists of 20 random points within a unit square",
       x = "",
       y = "",
       color = "",
       caption = "Average farthest-pairs distance = 1.130
                  Average closest-pairs distance = 0.039")
