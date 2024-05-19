library("readr")
library("dplyr")
library("sf")
library("ggplot2")
wildschwein <- read_delim("wildschwein_BE_2056.csv", ",")


sabi <- wildschwein |>
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE) |>
  filter(TierName == "Sabi", DatetimeUTC >= "2015-07-01", DatetimeUTC < "2015-07-03")
ggplot(sabi, aes(E,N, color =DatetimeUTC))+
  geom_point()+
  geom_path()+
  coord_fixed()+
  scale_color_datetime(low = "blue", high ="red")+
  guides(color = guide_colorbar(title.position = "top")
  )

distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

sabi <- sabi |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  # distance to pos -30 minutes
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  # distance to pos -15 minutes
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)), # distance to pos +15 mintues
    nPlus2  = distance_by_element(geometry, lead(geometry, 2))  # distance to pos +30 minutes
  )
sabi
sabi <- sabi |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2))
  ) |>
  ungroup()

sabi

sabi <- sabi |>
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))
sabi
sabi_filter <- sabi |>
  filter(!static)

sabi_filter |>
  ggplot(aes(E, N)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")


#####Import data
Daten_Bewegung <- read_csv("Daten_Bewegung.csv")
View(Daten_Bewegung)
Daten_mod  <- Daten_Bewegung
Daten_mod$user_id <- NULL
Daten_mod$weekday <- NULL
Daten_mod$place_name <- NULL
Daten_mod$transport_mode <- NULL
View(Daten_mod)

movment <- Daten_mod |>
  st_as_sf(coords = c("lon_x", "lat_y"), crs = 2056, remove = FALSE) |>
  filter(datetime >= "2024-04-05", datetime < "2024-04-06")
View(movment)


movment$datetime <- as.POSIXct(movment$datetime, format = "%Y-%m-%dT%H:%M:%SZ")


ggplot(movment, aes(lon_x,lat_y, color =datetime))+
  geom_point()+
  geom_path()+
  coord_fixed()+
  scale_color_datetime(low = "blue", high ="red")+
  guides(color = guide_colorbar(title.position = "top")
  )

distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

movment <- movment |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  # distance to pos -30 minutes
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  # distance to pos -15 minutes
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)), # distance to pos +15 mintues
    nPlus2  = distance_by_element(geometry, lead(geometry, 2))  # distance to pos +30 minutes
  )
movment

movment <- movment |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2))
  ) |>
  ungroup()

movment
summary(movment)
hist(movment$stepMean)


movment <- movment |>
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

movment_filter <- movment |>
  filter(!static)
View(movment_filter)

movment_filter|>
  ggplot(aes(lon_x, lat_y)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

######Visualize segmented trajectories


ggplot(movment, aes(lon_x,lat_y, color =static))+
  geom_point()+
  geom_path()+
  coord_fixed()


######Segment-based analysis

rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
  
}

movment <- movment |>
  mutate(segment_id = rle_id(static))

ggplot(movment, aes(lon_x,lat_y, color =segment_id))+
  geom_point()+
  geom_path()+
  coord_fixed()

########## Task 5 Similarity measures

pedestrian <- read_csv("pedestrian.csv")
View(pedestrian)


ggplot(pedestrian, aes(x = E, y = N, color = factor(TrajID))) +
  geom_point() +
  geom_path() +
  coord_fixed() +
  scale_color_viridis_d(option = "viridis") + 
  labs(title = "Visual comparison of the trajectories",
       subtitle = "Each subplot highlights a trajectory",
       x = "Easting",
       y = "Northing",
       color = "Trajectory ID") +
  theme_minimal() +
  theme(legend.position = "none") +  
  facet_wrap(~ TrajID, ncol = 3) 

###########TAsk 6Calculate similarity

install.packages("SimilarityMeasures")
library("SimilarityMeasures")
library(reshape2)

help(package = "SimilarityMeasures")




trajectory_list <- split(pedestrian, pedestrian$TrajID) %>%
  lapply(function(df) {
    as.matrix(df[, c("E", "N")])
  })


trajectory1 <- trajectory_list[[1]]


results_df <- data.frame(Trajectory = integer(), Measure = character(), Value = numeric(), stringsAsFactors = FALSE)


for (i in 2:length(trajectory_list)) {
  traj <- trajectory_list[[i]]
  

  dtw_result <- DTW(trajectory1, traj)
  editDist_result <- EditDist(trajectory1, traj)
  frechet_result <- Frechet(trajectory1, traj)
  lcss_result <- LCSS(trajectory1, traj, pointSpacing = 1, pointDistance = 0.1, errorMarg = 0.1)
  

  results_df <- rbind(results_df, data.frame(Trajectory = paste("Trajectory", i),
                                             Measure = "DTW",
                                             Value = dtw_result))
  results_df <- rbind(results_df, data.frame(Trajectory = paste("Trajectory", i),
                                             Measure = "EditDist",
                                             Value = editDist_result))
  results_df <- rbind(results_df, data.frame(Trajectory = paste("Trajectory", i),
                                             Measure = "Frechet",
                                             Value = frechet_result))
  results_df <- rbind(results_df, data.frame(Trajectory = paste("Trajectory", i),
                                             Measure = "LCSS",
                                             Value = lcss_result))
}





if (!("Measure" %in% names(results_df))) {
  results_df <- melt(results_df, id.vars = "Trajectory", variable.name = "Measure", value.name = "Value")
}


measure_colors <- c("DTW" = "red", "EditDist" = "green", "Frechet" = "blue", "LCSS" = "purple")

# Plot
plot <- ggplot(results_df, aes(x = as.factor(Trajectory), y = Value, fill = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = measure_colors) +
  facet_wrap(~ Measure, scales = "free_y") +  
  labs(title = "Computed Similarities Using Different Measures",
       x = "Comparison Trajectory",
       y = "Value") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 13, face = "bold"),
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Print the plot
print(plot)

