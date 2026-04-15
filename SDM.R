library(tidyverse)
library(sf)
library(terra)
library(usdm)
library(predicts)
library(glmnet)

X <- 10.44
Y <- 50.07

sail <- vect(
  data.frame(x = X, y = Y),
  geom = c("x", "y"),
  crs = "EPSG:4326"
)
sail_3035 <- project(sail, "EPSG:3035")

inv_species <- c("Fagus sylvatica")
eu <- ext(-20, 45, 34, 75)

model_species <- function(sp) {
  mauri |>
    filter(.data$species == sp) |>
    select(X, Y, occurs) |>
    st_as_sf(coords = c("X", "Y"), crs = "EPSG:3035") |>
    vect() -> species_pts

mauri <- read_csv("biogeomodel/data/EUForestspecies.csv", na = "-9999") |>
  rename(species = `SPECIES NAME`) |>
  mutate(
    occurs_1 = ifelse(is.na(`DBH-1`), FALSE, TRUE),
    occurs_2 = ifelse(is.na(`DBH-2`), FALSE, TRUE),
    occurs = occurs_1 + occurs_2,
    occurs = ifelse(occurs > 0, TRUE, FALSE)
  ) |>
  select(X, Y, species, occurs)
filter(X > 1500000, X < 7500000, Y > 900000, Y < 5500000)

 #Checking beech is present and how many records
mauri |>
  filter(.data$species == 'Fagus sylvatica')
  
