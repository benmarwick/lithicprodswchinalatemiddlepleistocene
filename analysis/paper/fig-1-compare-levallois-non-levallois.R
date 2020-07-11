

library(dplyr)
library(knitr)
library(ggplot2)
library(reshape2)
library(readxl)
library(fitdistrplus)
library(plyr)
library(tidyr)
library(ggbeeswarm)
library(knitr)
library(cowplot)

# read in the data --------------------------------------------------

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

# prepare the data --------------------------------------------------

require(Ckmeans.1d.dp)
result <- Ckmeans.1d.dp(flakes$mass[!is.na(flakes$mass)])

n_clusters <- length(result$size)
n_clusters_30 <- length(result$size[result$size > 30])

# assign cluster ID to table
flakes_clustered <-
  flakes %>%
  filter(!is.na(mass)) %>%
  mutate(cluster = result$cluster) %>%
  right_join(flakes) %>%
  filter(cluster %in% 1:n_clusters_30)

# clean data to help with analysis
flakes$retouched <- ifelse(grepl("ret", flakes$type1), "retouched", "unretouched")
flakes <- flakes[!flakes$material %in% c('sandstone', 'quartz'),]

leva <- flakes %>%
  filter(grepl("LVT|LVF|LVFB", type1)) %>%
  filter(!grepl("LVF_", type1))

non_leva <- flakes %>%
  filter(!grepl("LVT|LVF|LVFB", type1))

flakes_L <-
  mutate(flakes, L = ifelse(grepl("LVT|LVF|LVFB", type1), "Levallois", "Non-Levallois"))

# plot Levallois flake mass --------------------------------------------------

# are the levallois pieces bigger?

### compare the mass of leva and non-leva
p_leva_mass <-
  ggplot(flakes_L,
         aes(x = L,
             y = mass)) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.2) +
  xlab("") +
  ylab("Mass (g)") +
  scale_y_log10() + theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

# is it significant?

# print("t-test to see if it is statistically significant?")

# t.test(data = flakes_L, mass ~ L)

# leva is lighter

# plot Levallois flake max dim --------------------------------------------------

### compare the dimension of leva and non-leva
# print("compare the dimension of Leva and non-leva flakes")

p_leva_dim <-
  ggplot(flakes_L,
         aes(x = L,
             y = max_dimension)) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.2) +
  xlab("") +
  ylab("Max dimension (mm)") +
  scale_y_log10() +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

# is it significant?

# print("t-test to see if it is statistically significant?")

# t.test(data = flakes_L, max_dimension ~ L)

# no

# plot Levallois flake thickness --------------------------------------------------

### compare the thickness of leva and non-leva
# print("compare the thickness of Leva and non-leva flakes")

p_leva_thick <-
  ggplot(flakes_L,
         aes(x = L,
             y = `Thickness_50%max`)) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.2) +
  xlab("") + ylab("Thickness (mm)") +
  theme_bw() + scale_y_log10() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

# is it significant?

# print("t-test to see if it is statistically significant?")

# t.test(data = flakes_L, `Thickness_50%max` ~ L)

# yes, leva flakes are much thinner

# plot Quina flake thickness --------------------------------------------------

## compare thickness of quina and other
## find quina and compare their angles.
# calculate the edge angle

edge_angle_2 <- edge_angle

edge_angle_2[, 2:(ncol(edge_angle_2)-1)] <- atan(edge_angle[, 2:(ncol(edge_angle_2)-1)]/2/3)/pi*180*2

df_angle <- melt(edge_angle_2[, 1:(ncol(edge_angle_2)-1)])

df_angle$variable <- substr(df_angle$variable, 1, 9)

colnames(df_angle)[2:3] <- c("section", "angle")

quina_id <- flakes$number[which(flakes$type3 == "quina")]

df_angle$quina <- ifelse(df_angle$number %in% quina_id, "Quina", "Non-Quina")

p_edge_quina <-
  ggplot(df_angle,
         aes(x = angle,
             col = quina)) +
  geom_density(size = 2) +
  theme_bw() +
  xlab("Edge angle (\u00B0)") + ylab("Density") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 16))+
  theme(legend.position = c(0.85, 0.85),
        legend.title = element_blank())+ theme(plot.margin=margin(10,10,10,10))

df_thick <- melt(flakes[, c("number", "Thickness_25%max", "Thickness_50%max", "Thickness_75%max")])

df_thick$quina <- ifelse(df_thick$number %in% quina_id, "Quina", "Non-Quina")

df_thick$label <- substring(df_thick$variable, 11, 13)

p_thick_quina <-
  ggplot(df_thick,
         aes(x = value,
             col = quina)) +
  geom_density(size = 2) +
  theme_bw() +
  facet_wrap(~label) +
  xlab("Thickness (mm)") + ylab("Density") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 18),
        legend.position = c(0.9, 0.9),
        legend.title = element_blank())+ theme(plot.margin=margin(10,10,10,10))

# plot GIUR on Quina  --------------------------------------------------

## calculate GIUR

df_giur <- retouch[, c(1,8,10,12,14,16,18,20,22)]

df_giur[, -1] <- df_giur[, -1] / retouch[, c(9,11,13,15,17,19,21,23)]

df_giur_clustered <- left_join(df_giur, flakes_clustered[, c("number", "cluster")])

df_giur_clustered_melt <- melt(df_giur_clustered[,-1], id.vars = "cluster")

df_giur_clustered$quina <- ifelse(df_giur_clustered$number %in% quina_id, "Quina", "Non-Quina")

df_giur_clustered_melt_2 <- melt(df_giur_clustered[,-1], id.vars = "quina")

p_GIUR_quina <-
  ggplot(na.omit(df_giur_clustered_melt_2[df_giur_clustered_melt_2$value <=1,]), aes(value)) +
  geom_histogram(binwidth = 0.1) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  xlab("GIUR") +
  ylab("Count") +
  facet_wrap(~quina, scales = "free_y") +
  geom_vline(data = ddply(na.omit(df_giur_clustered_melt_2[df_giur_clustered_melt_2$value <=1,]),
                          "quina",
                          summarize,
                          md = median(value)),
             aes(xintercept = md),
             linetype = "dashed") +
  theme(plot.margin=margin(10,10,10,10))


## plot figures together.
figure_leva_quina <-
  cowplot::plot_grid(plot_grid(p_leva_mass, p_leva_dim, p_leva_thick,
                               labels = "auto",
                               label_size = 26,
                               ncol = 3),
                     plot_grid(p_edge_quina, p_thick_quina,
                               labels = c("d", "e"),
                               label_size = 26,
                               ncol = 2,
                               rel_widths = c(1,2)),
                     plot_grid(p_GIUR_quina, labels= "f",
                               ncol = 1,
                               #rel_widths = c(1.5, 1),
                               label_size = 26),
                     nrow = 3,
                     align = 'v',
                     axis = 'lr',
                     vjust = 1.5,
                     hjust = -0.5,
                     scale = 0.9) #

save_plot(here::here("analysis/figures/fig-1-Figures-for-Leva-and-quina.png"),
                     figure_leva_quina,
          base_aspect_ratio = 1,
          base_height = 18,
          base_width = 22)

