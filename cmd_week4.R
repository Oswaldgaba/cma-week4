library("readr")
library("dplyr")
library("sf")
library("ggplot2")
wildschwein <- read_delim("wildschwein_BE_2056.csv", ",")

# Careful! What Timezone is assumed?
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

movment_filter |>
  ggplot(aes(lon_x, lat_y)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")