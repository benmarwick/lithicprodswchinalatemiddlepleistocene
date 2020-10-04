

#install required packages
library(dplyr)
library(knitr)
library(ggplot2)
library(reshape2)
library(readxl)
library(fitdistrplus)
library(plyr)
library(tidyr)
library(classInt)
library(knitr)
library(cowplot)

# read in the data


file_name <- here::here("analysis/data/raw_data/artefacts-after Sam_Dec2018_quina_modified.xls")

flakes <- as.data.frame(read_excel(file_name, sheet = "flake basics"))
cores <- as.data.frame(read_excel(file_name, sheet = "core basics"))
debris <- as.data.frame(read_excel(file_name, sheet = "chunk&debris"))
retouch <- as.data.frame(read_excel(file_name, sheet = "retouch"))
core_flake_scars_1 <- as.data.frame(read_excel(file_name, sheet = "core flake scars"))
edge_angle_1 <- as.data.frame(read_excel(file_name, sheet = "edge angle"))
retouched_pebble_chunk <- as.data.frame(read_excel(file_name, sheet = "Pebble&chunk"))
scar_dir <- as.data.frame(read_excel(file_name, sheet = 'scar direction'))
core_flake_scars <- core_flake_scars_1[core_flake_scars_1$number %in% cores$number, ]
edge_angle <- edge_angle_1[edge_angle_1$number %in% retouch$number, ]


# basic details of the dataset
artefact_ids <- list(flake_ids = flakes$number,
                     core_ids = cores$number,
                     debris_ids = debris$number)

retouch_ids <- retouch$number

# clean data to help with analysis
flakes$retouched <- ifelse(grepl("ret", flakes$type1), "retouched", "unretouched")
flakes <- flakes[!flakes$material %in% c('sandstone', 'quartz'),]

leva <- flakes %>%
  filter(grepl("LVT|LVF|LVFB", type1)) %>%
  filter(!grepl("LVF_", type1))

non_leva <- flakes %>%
  filter(!grepl("LVT|LVF|LVFB", type1))

flakes_L <-
  mutate(flakes, L = ifelse(grepl("LVT|LVF|LVFB", type1), "L", "N"))


# need a test of uniformity (peak width)

# check the CVs
CV <- function(the_vector){
  mean_ <- mean(the_vector, na.rm = TRUE)
  sd_ <- sd(the_vector, na.rm = TRUE)
  cv <- (sd_/mean_) * 100
  round(cv, 3)
}


cv_res <- flakes_L %>%
  group_by(L) %>%
  dplyr::summarise(cv_25_thick = CV(`Thickness_25%max`),
                   cv_50_thick = CV(`Thickness_50%max`),
                   cv_75_thick = CV(`Thickness_75%max`),
                   cv_25_width = CV(`Width_25%max`),
                   cv_50_width = CV(`Width_50%max`),
                   cv_75_width = CV(`Width_75%max`),
                   n = n())

print("summary of cv for thickness and width for leva and non-leva flakes")

cv_res


# BM

flakes_L %>%
  group_by(L) %>%
  dplyr::summarise(
    cv_max_dim = CV(max_dimension),
    cv_oriented_width = CV(oriented_width),
    cv_25_thick = CV(`Thickness_25%max`),
    cv_50_thick = CV(`Thickness_50%max`),
    cv_length = CV(length),
    cv_75_thick = CV(`Thickness_75%max`),
    cv_25_width = CV(`Width_25%max`),
    cv_50_width = CV(`Width_50%max`),
    cv_75_width = CV(`Width_75%max`),
    cv_oriented_thick = CV(oriented_thickness),
    n = n()
  ) %>%
  pivot_longer(-L) %>%
  pivot_wider(names_from = L)




