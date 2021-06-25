
#install required packages
library(dplyr)
library(readr)
library(knitr)
library(ggplot2)
library(reshape2)
library(readxl)
library(fitdistrplus)
library(tidyr)
library(classInt)
library(cowplot)
library(gridExtra)

# read in the data

file_name <-
  here::here("analysis/data/raw_data/artefacts-after Sam_Dec2018_quina_modified-2019.xls")

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# basic details of the dataset

artefact_ids <- list(flake_ids = flakes$number,
                     core_ids = cores$number,
                     debris_ids = debris$number)

retouch_ids <- retouch$number

# how many unique artefacts?

totals_of_each_type <- lapply(artefact_ids, function(i) length(i))

count_unique_artefacts <- sum(unlist(totals_of_each_type))

# debris pieces that are retouch pieces

debris_with_retouch <- intersect(retouch_ids, artefact_ids$debris_ids)

proportion_debris_retouched <- length(debris_with_retouch) / count_unique_artefacts

# proportion of each type

type_table <- data.frame(type = names(as.data.frame(totals_of_each_type)),
                         count = as.numeric(as.data.frame(totals_of_each_type)),
                         proportion = round(as.numeric(prop.table(as.data.frame(totals_of_each_type))),3))

# add total row at the bottom

type_table$type <- as.character(type_table$type)

type_table <- rbind(type_table, c("total", count_unique_artefacts, round(sum(type_table$proportion),2)))

# print pretty table

# print("table of lithic types")

kable(type_table)

### raw materials

raw_material_table <- data.frame("category" = character(),
                                 'basalt' = numeric(),
                                 'chert' = numeric(),
                                 'limestone' = numeric(),
                                 'quartz' = numeric(),
                                 'sandstone' = numeric(),
                                 stringsAsFactors = FALSE)


res <- flakes %>%
  mutate(complet_flk = ifelse(type1 %in% c("flake", "blade", "KBW", "tablet", "leva", "leva?",
                                           "DBD", "crest"), "complete flake",
                              ifelse(type1 %in% c("flake brk", "blade brk", "LVFB", "proximal"),
                                     "flake breaks", "other"))) %>%
  dplyr :: count(material, complet_flk) %>%
  arrange(complet_flk) %>%
  group_by(complet_flk) %>%
  spread(material, n) %>%
  replace(., is.na(.), 0) %>%
  rename(category = complet_flk)

raw_material_table <- bind_rows(raw_material_table, res[1:2,])

res <- cores %>% filter(!is.na(material)) %>%
  dplyr :: count(material) %>%
  spread(material, n) %>%
  mutate(category = "cores")

raw_material_table <- bind_rows(raw_material_table, res)

res <- debris %>% filter(!is.na(material)) %>%
  dplyr :: count(material) %>%
  spread(material, n) %>%
  mutate(category = "debris")

raw_material_table <- bind_rows(raw_material_table, res)

res <- flakes %>%
  filter(type1 %in% c("ret", "ret brk", "BUR", "LVT", "ret leva?", "ret pro-lepoin")) %>%
  dplyr:: select(material, number) %>%
  dplyr :: count(material) %>%
  spread(material, n) %>%
  mutate(category = "retouched flakes and breaks")

raw_material_table <- bind_rows(raw_material_table, res)

res <- retouched_pebble_chunk %>%
dplyr :: count(material) %>%
  spread(material, n) %>%
  mutate(category = "retouched chunks")

raw_material_table <- bind_rows(raw_material_table, res)

###tools raw materials

res <- flakes %>%
  mutate(type2 = replace(type2, is.na(type2),"unidentifiable" )) %>%
  filter(type1 %in% c("ret", "ret brk", "BUR", "LVT", "ret leva?", "ret pro-lepoin")) %>%
  mutate(tooltype = ifelse(type2 == "backed knife", "backed knife",
                      ifelse(type2 %in% c("bec", "bec,scp"), "bec",
                       ifelse(type2 %in% c("borer","borer,dent", "borer,dent,scp",
                                           "borer,end,scp","borer,notch",
                                           "borer,notch,scp","borer,scp",
                                           "borer,scp,end","borer,strt" ),
                              "borer",
                        ifelse(type2 %in% c("BUR", "BUR? CORE?","BURIN", "burin"),
                               "bur",
                         ifelse(type2 %in% c("cp", "chopper"), "chopper",
                         ifelse(type2 =="cleaver", "cleaver",
                          ifelse(type2 %in% c("dent", "dent,borer,scp","dent,end","dent,notch",  "dent,scp",
                                              "dent/NOT", "dent/POINT"  ),
                                 "denticulate",
                                 ifelse(type2 %in% c("end", "end,borer" ,
                                                     "end,borer,scp" ,"end,scp" ,
                                                     "endscp" ), "endscraper",
                                ifelse(type2 %in% c("leva point","point" ,"POINT"
                                                    , "point break",
                                                    "point,broken", "point?"  ),
                                       "point",
                                       ifelse(type2 %in% c("moon","scp",
                                                           "sidescp" , "sidescp-cvx"  ),
                                              "scraper",
                                   ifelse(type2 %in% c("NATBACK", "nature backed"),
                                          "natural backed",
                                          ifelse(type2 %in% c( "notcch", "notch" , "notch,borer"  , "notch,end","notch,scp" ,
                                             "notch/DENT"),
                                             "notch",
                                             ifelse(type2 %in% c("TANG", "TANGED",  "TANGED POINT", "TANGED POINT /NOT",
                                               "tanged point?", "TANGED POINT?"),
                                               "tanged point", type2)))))))))))))) %>%
  dplyr :: count(material, tooltype) %>%
  spread(material, n) %>%
  replace(., is.na(.), 0) %>%
  rename(category = tooltype)

raw_material_table <- bind_rows(raw_material_table, res)

raw_material_table[is.na(raw_material_table)] <- 0 ##convert na to 0

raw_material_table$total <- rowSums(raw_material_table[,-1])

x <- raw_material_table

prop_raw_material <- cbind(category = x[, 1], round(x[, -1]/rowSums(x[, 2:(ncol(x)-1)])*100, digits = 1))



# for a table like what we have in the paper, SI Table S1, we
# need to do this


# put rows in a specific order
row_order <- c("cores", "complete flake", "flake breaks", "debris", "retouched chunks",
               "retouched flakes and breaks",
               "baked knife", "bec", "borer", "bur", "chopper", "cleaver", "denticulate",
               "endscraper", "natural backed", "notch", "point", "scraper", "tanged point", "unidentifiable")

# for counts
raw_material_table_counts_tbl <-
raw_material_table %>%
  mutate(category = factor(category, levels = row_order)) %>%
  arrange(category) %>%
  filter(!is.na(category)) %>%
  mutate(others = quartz + sandstone) %>%
  dplyr::select(category, chert, basalt, limestone, others, total)

# for proportions
raw_material_table_props_tbl <-
prop_raw_material %>%
  mutate(category = factor(category, levels = row_order)) %>%
  arrange(category) %>%
  filter(!is.na(category)) %>%
  mutate(others = quartz + sandstone) %>%
  dplyr::select(category, chert, basalt, limestone, others, total)

# combine both to produce Table S1
raw_material_table_props_and_counts_tbl <-
tibble(
  type = raw_material_table_counts_tbl$category,
  chert_count = raw_material_table_counts_tbl$chert,
  chert_perc = raw_material_table_props_tbl$chert,
  basalt_count = raw_material_table_counts_tbl$basalt,
  basalt_perc = raw_material_table_props_tbl$basalt,
  limestone_count = raw_material_table_counts_tbl$limestone,
  limestone_perc = raw_material_table_props_tbl$limestone,
  others_count = raw_material_table_counts_tbl$others,
  others_perc = raw_material_table_props_tbl$others,
  total = raw_material_table_counts_tbl$total
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### cores attribute summary
core_summary <- cores[0, c(6:17)]

core_summary["mean",] <- c("mean" = apply(na.omit(cores[,c(6:17)]), 2, mean))

core_summary["sd",] <- c("sd" = apply(na.omit(cores[,c(6:17)]), 2, sd))

core_summary["CV",] <- core_summary["sd",]/core_summary["mean",]

core_summary <- rbind(core_summary,
                      sapply(na.omit(cores[,6:17]), function(x) quantile(x, c(0.25, 0.5, 0.75))))

# This is Table S2

core_summary

# kable(round(core_summary, digits = 1))

########################################################################



# function to compute frequencies and make a plot
f <-  function(the_data, the_column) {
  data.frame(table(the_data[the_column])) %>%
    ggplot(aes(x = reorder(Var1, Freq), y = Freq)) +
    geom_bar(stat = "identity") +
    ylab("count") +
    xlab(the_column) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25)
    )
}

##################################################################################
# These lines will make figure S2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#The following are results for cores


# some plots of frequencies for cores

## geometry
print("summary for core geometry")

p_core_geometry <- f(cores, "core_geometry") + xlab("Core geometry") + ylab("Count")

core_geom_table <-  table(na.omit(cores$core_geometry))

round(prop.table(core_geom_table), digits = 4)*100


## platform number

print("summary for core platform")

f(cores, "platform") + xlab("Number of platform") + ylab("Count")

p_core_platform <- ggplot(cores, aes(platform)) +
  geom_bar() + theme_bw() +
  xlab("Number of platform") +
  ylab("Count") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

core_plat_table <-  table(na.omit(cores$platform))

round(prop.table(core_plat_table), digits = 4)*100

### cortex proportion of cores

print("cortex proportion of cores")

p_core_cortex <- ggplot(cores, aes(cortex_percentage)) +
  geom_histogram(binwidth = 10) + theme_bw() +
  xlab("Cortex proportion (%)") +
  ylab("Number of cores") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

core_cortex_table <-  table(na.omit(cores$cortex_percentage))

round(prop.table(core_cortex_table), digits = 4)*100

## platform type

print("summary for core platform preparation")

# what are these platform types?
unique(cores$platform_preparation)

cores <-
  cores %>%
  mutate(platform_preparation = case_when(
    platform_preparation == "over" ~ 'overhang\nremoval',
    platform_preparation == "plane" ~ 'plain',
    platform_preparation == "faceted" ~ 'facetted',
    platform_preparation == "cortex" ~ 'cortical',
    platform_preparation == "dehid" ~ 'dihedral'
  )) %>%
  drop_na(platform_preparation)

# "over"    "plain"   "plane"   "faceted"  "n"    "cortex"  "dehid".
# 'overhang removal', 'plain' (same as plane?), 'facetted', 'cortical', 'dihedral'

p_core_plat_prep <- f(cores, "platform_preparation") +
  xlab("Platform preparation") +
  ylab("Count") +
  coord_flip()

core_plat_type <-  table(na.omit(cores$platform_preparation))

round(prop.table(core_plat_type), digits = 4)*100

## platform rotation
print("summary for core rotations")

f(cores, "rotations") + xlab("Core rotations") + ylab("Count")

p_core_rotation <- ggplot(cores, aes(rotations)) +
  geom_bar() + theme_bw() +
  xlab("Number of rotation") +
  ylab("Number of cores") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

core_plat_rotat <-  table(na.omit(cores$rotations))

round(prop.table(core_plat_rotat), digits = 4)*100


## core reduction type
print("summary for core reduction type")

p_core_reduction <- f(cores, "type") + xlab("Reduction type") + ylab("Count")

core_type_table <- table(na.omit(cores$type))

round(prop.table(core_type_table),digits = 4)*100

## core scar number

print("summary of core scar number")

p_core_scar_number <- ggplot(cores, aes(scar_number)) +
  geom_bar() + theme_bw() +
  xlab("Scar number on cores") +
  ylab("Count") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))


core_scar_table <- table(na.omit(cores$scar_number))

round(prop.table(core_scar_table), digits = 4)*100

## core flake scar length

print("summary for core flake scar length")

core_flake_scars_melt <- melt(core_flake_scars_1)

colnames(core_flake_scars_melt)[3] <- "scar_length"

p_core_scar_length <- ggplot(core_flake_scars_melt[core_flake_scars_melt$scar_length < 500, ],
       aes(scar_length)) +
  geom_density() + theme_bw() +
  xlab("Length of scar on cores (mm)") +
  ylab("Density") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

print("median of scar length")

median(na.omit(core_flake_scars_melt[core_flake_scars_melt$scar_length < 500, ]$scar_length))

# Figrur S2 is made here
figure_cores <-
  cowplot::plot_grid(p_core_platform,
                     p_core_scar_number,
                     p_core_scar_length,
                     p_core_cortex,
                     p_core_plat_prep,
                     labels = "auto",
                     label_size = 26,
                     ncol = 3,
                     align = 'v',
                     axis = 'lr',
                     #rel_heights = c(1, 1.5),
                     vjust = 1.5,
                     hjust = -0.5,
                     scale = 0.95) #

save_plot("analysis/figures/S2 figures for cores.png", figure_cores,
          base_aspect_ratio = 1,
          base_height = 13,
          base_width = 21)

#########################################################################
#We will try univariate clustering for flake mass:

require(Ckmeans.1d.dp)
result <- Ckmeans.1d.dp(flakes$mass[!is.na(flakes$mass)])

print("flake grouping result according to mass")
plot(result)

n_clusters <- length(result$size)
n_clusters_30 <- length(result$size[result$size > 30])

# assign cluster ID to table
flakes_clustered <-
  flakes %>%
  filter(!is.na(mass)) %>%
  mutate(cluster = result$cluster) %>%
  right_join(flakes) %>%
  filter(cluster %in% 1:n_clusters_30)


############################################################################



#################################################################################
##The following results are for flakes discussion in the SI

# explore flakes

flake_summary <- flakes[0, c(7:17, 19,20,22,23)]

flake_summary["mean",] <- c("mean" = apply(na.omit(flakes[,c(7:17, 19,20,22,23)]), 2, mean))

flake_summary["sd",] <- c("sd" = apply(na.omit(flakes[,c(7:17, 19,20,22,23)]), 2, sd))

flake_summary["cv", ] <- flake_summary["sd",] /flake_summary["mean",]

flake_summary <- rbind(flake_summary,
                       sapply(na.omit(flakes[,c(7:17, 19,20,22,23)]),
                              function(x) quantile(x, c(0.25, 0.5, 0.75))))

print("table for flake attributes")

knitr::kable(round(flake_summary, digits = 1))

## how many flake type1

print("summary for flake type1")

p_flake_type1 <- f(flakes, "type1") + xlab("Flake type") + ylab("Count")

table(na.omit(flakes$type1))
round(prop.table(table(na.omit(flakes$type1))), digits = 3)*100

## flake type2
print("summary for flake type2")

p_flake_type2 <- f(flakes, "type2")

table(na.omit(flakes$type2))
round(prop.table(table(na.omit(flakes$type2))),digits = 3)*100

## how many retouched and unretouched flakes
flakes$retouched <- ifelse(grepl("ret", flakes$type1), "retouched", "unretouched")

flakes <- flakes[!flakes$material %in% c('quartz', 'sandstone'),]

print("summary for flake retouch")

p_flake_ret <- f(flakes, "retouched") + xlab("") + ylab("Count")

table(na.omit(flakes$retouched))
round(prop.table(table(na.omit(flakes$retouched))), digits = 3)*100

## raw material of flakes
print("summary for flake material")

p_flake_material <- f(flakes, "material")

## platform shape of flakes
print("summary for flake platform shape")

p_flake_platform_shape <- f(flakes, "platform_shape") + xlab("Platform shape") +ylab("Count")

table(na.omit(flakes$platform_shape))
round(prop.table(table(na.omit(flakes$platform_shape))), digits = 4)*100

### number of flakes with distinctive platform
## platform of flakes
print("summary for flake platform")

p_flake_platform <- f(flakes, "platform") + xlab("Platform") + ylab("Count")

paste( "number of flakes with distinctive platform =", length(na.omit(flakes$platform)) )

table(na.omit(flakes$platform))
round(prop.table(table(na.omit(flakes$platform))), digits = 3)*100

# mass for all flakes
print("mass of flakes")

p_flake_mass <- ggplot(flakes, aes(mass)) +
  geom_histogram(binwidth = 10) + theme_bw() +
  xlab("Mass (g)") + ylab("Number of flakes") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

## max dimension of all flakes

print("max dimension of all flakes")

p_flake_dimension <- ggplot(flakes, aes(max_dimension)) +
  geom_density() + theme_bw() +
  xlab("Maximum dimension (mm)") + ylab("Density") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

ggplot(flakes, aes(max_dimension)) +
  geom_histogram(binwidth = 5) + theme_bw() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

## length vs width at 50% max dime
print("length vs width at 50% max dim")

p_flake_length_width <- ggplot(flakes, aes(length, `Width_50%max`, colour = material, shape = material)) +
  geom_point(size = 3) +
  #geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Length (mm)") + ylab("Width at 50% maximum dimension (mm)") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 18),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.15))

## platform shape vs thickness at 50% max dime
print("platform shape vs thickness at 50% max dim")

p_flake_shape_thick <- ggplot(flakes[!is.na(flakes$platform_shape),],
       aes(`Thickness_50%max`, x = platform_shape)) +
  geom_boxplot() + scale_y_log10()+ theme_bw() +
  xlab("Platform shape") + ylab("Thickness at 50% max. dim. (mm)") +
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

## platform shape vs max dim
print("platform shape vs max dim")

p_flake_shape_dim <- ggplot(flakes[!is.na(flakes$platform_shape),],
       aes(`max_dimension`, x = platform_shape)) +
  geom_boxplot() + scale_y_log10()+ theme_bw() +
  xlab("Platform shape") + ylab("Maximum dimension (mm)") +
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

# mass for all flakes, retouched or not

print("mass for retouched and non-retouched flakes")

p_flake_ret_mass <- ggplot(flakes, aes(mass, colour = retouched)) +
  geom_density()+ theme_bw()

  # mass for all flakes, by raw material

  print("mass for flakes of different raw materials")

  p_flake_mass_material <- ggplot(flakes, aes(mass, colour = material)) + scale_x_log10() +
  geom_density()+ theme_bw() +theme(text = element_text(size = 20))

## max dim for all flakes, retouched or not

print("max dim for all flakes, retouched or not")

p_flake_ret_dim <- ggplot(flakes, aes(`max_dimension`, colour = retouched)) +
  geom_density()+ theme_bw() +
  xlab("Maximum dimension (mm)") + ylab("Density") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 20))+
  theme(legend.position = c(0.8, 0.8),
        legend.title = element_blank())

## max dim for all flakes, by raw material
print("max dim for all flakes, by raw material")

p_flake_dim_material <- ggplot(flakes, aes(`max_dimension`, colour = material)) +
  geom_density()+ theme_bw() +theme(text = element_text(size = 20))

## scar counts for all flakes, by raw material
print("scar counts for all flakes, by raw material")

p_flake_scar_number <- ggplot(flakes, aes(`scar_number`)) +
  geom_bar()+ theme_bw() +
  xlab("Number of scars") + ylab("Count of flakes")+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

flakes$scar_group <- ifelse(flakes$`scar_number` > 5, "more than 5", "less than 5")

p_flake_scar_number_2 <- ggplot(flakes[!is.na(flakes$scar_group), ],
                                aes(mass, x = scar_group)) +
  geom_boxplot() +
  xlab("Number of scars") +
  ylab("Mass (g)")+
  scale_y_log10()+ theme_bw() +theme(text = element_text(size = 20))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

ggplot(flakes[!is.na(flakes$scar_group), ], aes(mass, colour = scar_group)) +
  geom_density()+ theme_bw() + scale_x_log10() +
  xlab("Mass (g)") + ylab("Count of flakes")+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

# facetted plaforms

print("facetted platforms")

flakes$facetted_platform <- ifelse(grepl("fac", flakes$platform), "fac", "no")

table(na.omit(flakes$facetted_platform))
round(prop.table(table(na.omit(flakes$facetted_platform))), digits = 4)*100

p_flake_facet_dim <- ggplot(flakes, aes(`max_dimension`, colour = facetted_platform)) +
  geom_density()+ theme_bw() +
  xlab("Maximum dimension (mm)") + ylab("Density") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 20))+
  theme(legend.position = c(0.8, 0.8),
        legend.title = element_blank())

### cortex proportion of flakes

print("cortex proportion of flakes")

p_flake_cortex <- ggplot(flakes, aes(cortex_percentage)) +
  geom_histogram(binwidth = 10) + theme_bw() +
  xlab("Cortex proportion (%)") + ylab("Count") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

### compare cortex for ret and non-ret

print("compare cortex for ret and non-retouched flakes")

p_flakes_cortex_ret <- ggplot(flakes, aes(x = cortex_percentage, fill = retouched)) +
  geom_histogram(binwidth = 10, position='dodge') +
  xlab("Cortex percentage (%)") + ylab("Count") +
  theme_bw() + theme(text = element_text(size = 20))

### ratio between length and oriented thickness

  flakes_length_thick <- flakes[!is.na(flakes$length) & !is.na(flakes$oriented_thickness), ]

  flakes_length_thick$ratio <- flakes_length_thick$length/flakes_length_thick$oriented_thickness

  p_flake_length_thick <- ggplot(flakes_length_thick, aes(ratio)) +
    geom_histogram() + theme_bw() +
    xlab("Length/thickness ratio") + ylab("Count") +
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25)) +
    geom_vline(xintercept = median(flakes_length_thick$ratio), linetype = "dashed", colour = "red")

### dimension of flake platform

  flakes_L <-
    mutate(flakes, L = ifelse(grepl("LVT|LVF|LVFB", type1),
                              "Levallois", "Non-Levallois"))

  flk_plat_dim <- melt(flakes_L[, c("platform_thickness",
                                      "platform_width", "L")])

  p_flake_plat_dim <- ggplot(flk_plat_dim, aes(x = value, color = variable)) +
    geom_density(alpha = 0.3) + theme_bw() +
    xlab("Flake platform dimension (mm)") + ylab("Density") +
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          legend.title = element_blank(),
          legend.position = c(0.8, 0.8),
          legend.text = element_text(size = 18)) +
    scale_color_manual(labels = c("thickness", "width"),
                       values = c("red","green")) +
    geom_vline(xintercept = c(median(flakes$platform_thickness, na.rm = TRUE),
                              median(flakes$platform_width, na.rm = TRUE)),
                              linetype = "dashed", colour = c("red", "green"))

  # explore variation in thickness

  flk_thickness_long <- melt(flakes[, c("Thickness_25%max",
                                      "Thickness_50%max",
                                      "Thickness_75%max")])

  p_flake_thick <- ggplot(flk_thickness_long, aes(x = value, color = variable)) +
    geom_density(alpha = 0.3) + theme_bw() +
    xlab("Thickness (mm)") + ylab("Density") +
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          legend.title = element_blank(),
          legend.position = c(0.8, 0.8),
          legend.text = element_text(size = 18)) +
    scale_color_manual(labels = c("25%", "50%", "75%"),
                       values = c("red","green", "blue"))

  print("variation of thickness for all flakes (red = 25%max_dim,green = 50%max,blue=75%max")

  p_flake_thick

  # explore variation in width in these groups

  flk_width_long <- melt(flakes[, c("Width_25%max",
                                  "Width_50%max",
                                  "Width_75%max")])
  p_flake_width <- ggplot(flk_width_long, aes(x = value, color = variable)) +
    geom_density(alpha = 0.3) + theme_bw() +
    xlab("Width (mm)") + ylab("Density") +
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          legend.title = element_blank(),
          legend.position = c(0.8, 0.8),
          legend.text = element_text(size = 18)) +
    scale_color_manual(labels = c("25%", "50%", "75%"),
                       values = c("red","green", "blue"))

  print("variation of width for all flakes (red = 25%max_dim,green = 50%max,blue=75%max")

  p_flake_width

## how many points

print("how many points?")

flakes$point <- ifelse(grepl("point|POINT", flakes$type2), "point",
                       ifelse(grepl("back", flakes$type2), "backed", "other"))

table(na.omit(flakes$point))
round(prop.table(table(na.omit(flakes$point))), digits = 4)*100

# This is figure S4
### plot together
figure_flake_1 <-
  cowplot::plot_grid(p_flake_mass,
                     p_flake_dimension,
                     p_flake_length_thick,
                     p_flake_thick,
                     p_flake_width,
                     p_flake_cortex,
                     p_flake_facet_dim,
                     p_flake_scar_number,
                     p_flake_scar_number_2,
                     labels = "auto",
                     label_size = 26,
                     ncol = 3,
                     align = 'v',
                     axis = 'lr',
                     #rel_heights = c(1, 1.5),
                     vjust = 1.5,
                     hjust = -0.5,
                     scale = 0.95) #

save_plot("analysis/figures/S4 Figures for flakes_1.png", figure_flake_1,
          base_aspect_ratio = 1,
          base_height = 20,
          base_width = 21)

# This is for figure S5
figure_flake_2 <-
  cowplot::plot_grid(p_flake_plat_dim,
                     p_flake_platform_shape,
                     p_flake_shape_dim,
                     p_flake_shape_thick,
                     labels = "auto",
                     label_size = 26,
                     ncol = 2,
                     align = 'v',
                     axis = 'lr',
                     #rel_heights = c(1, 1.5),
                     vjust = 1,
                     hjust = -0.5,
                     scale = 0.9) #

save_plot("Figures for flakes_2.png", figure_flake_2,
          base_aspect_ratio = 1,
          base_height = 14,
          base_width = 15)


###########################################################################

### Results for retouch

# calculate the edge angle

edge_angle_2 <- edge_angle

edge_angle_2[, 2:(ncol(edge_angle_2)-1)] <- atan(edge_angle[, 2:(ncol(edge_angle_2)-1)]/2/3)/pi*180*2

df_angle <- melt(edge_angle_2[, 1:(ncol(edge_angle_2)-1)])

df_angle$variable <- substr(df_angle$variable, 1, 9)

colnames(df_angle)[2:3] <- c("section", "angle")

# plot the angles

print("edge angles")

p_edge_angle <- ggplot(df_angle,
                       aes(x = angle,
                           col = section)) +
  geom_density() + theme_bw() +
  xlab("Edge angle (\u00B0)") + ylab("Density") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 15),
        legend.position = c(0.85, 0.72),
        legend.title = element_blank())+ theme(plot.margin=margin(10,10,10,10))

## find quina and compare their angles.

quina_id <- flakes$number[which(flakes$type3 == "quina")]

df_angle$quina <- ifelse(df_angle$number %in% quina_id, "quina", "non-quina")

p_edge_quina <- ggplot(df_angle, aes(x = angle, col = quina)) + geom_density() +
  theme_bw() +
  xlab("Edge angle (\u00B0)") + ylab("Density") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 16))+
  theme(legend.position = c(0.85, 0.85),
        legend.title = element_blank())+ theme(plot.margin=margin(10,10,10,10))

ggplot(df_angle, aes(x = angle, col = section)) +
  geom_density() +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  facet_wrap(~quina)

## compare thickness of quina and other

df_thick <- melt(flakes[, c("number", "Thickness_25%max", "Thickness_50%max", "Thickness_75%max")])

df_thick$quina <- ifelse(df_thick$number %in% quina_id, "quina", "non_quina")

df_thick$label <- substring(df_thick$variable, 11, 13)

p_thick_quina <- ggplot(df_thick, aes(x = value, col = quina)) + geom_density() +
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
        legend.title = element_blank()) + theme(plot.margin=margin(10,10,10,10))

# plot the thickness of 50% max

print("thickness of 50% max dim")

p_thick_quina_2 <- ggplot(df_thick[df_thick$variable =="Thickness_50%max", ], aes(quina, value)) +
  geom_boxplot() + theme_bw() +
  xlab("Thickness at 50% max. dim. (mm)") + ylab("Density") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+ theme(plot.margin=margin(10,10,10,10))

## count the number of shapes of edges.

edge_shapes <- unlist(strsplit(retouch$`edge shape`, ","))

p_edge_shape <- data.frame(table(edge_shapes)) %>%
  ggplot(aes(x = reorder(edge_shapes, Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  ylab("n") +
  xlab("Edge type") +
  theme_bw() +theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  theme(plot.margin=margin(10,10,10,10))

data.frame(table(edge_shapes))

## number of layers

p_layers <- ggplot(retouch, aes(`number of layers`)) +
  geom_bar() + theme_bw() +
  xlab("Number of layers") +
  ylab("Count") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+ theme(plot.margin=margin(10,10,10,10))

## number of edges

f(retouch, 'number of edge')

p_edge_number <-  ggplot(retouch, aes(`number of edge`)) +
  geom_bar() + theme_bw() +
  xlab("Number of edges") +
  ylab("Count") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))+
  theme(plot.margin=margin(10,10,10,10))

## number of layers for each number of edge

df_layer_edge <- melt(retouch[, 1:3])

retouch$layers <- ifelse(retouch$`number of layers` < 2, "layer = 1", "layer > 1")

retouch$edges <- ifelse(retouch$`number of edge` > 3, "edge no. > 3",
                        paste("edge no. = ", retouch$`number of edge`))

p_edge_layer <- data.frame(table(retouch[,c("layers", "edges")])) %>%
  mutate(edges =  str_remove(edges, "edge no. ")) %>%
  mutate(edges =  str_squish(str_remove(edges, "="))) %>%
  mutate(edges =  factor(edges, levels = c("0", "1", "2", "3", "> 3"))) %>%
  ggplot(aes(x = edges,
             y = Freq,
             fill = layers)) +
  geom_bar(stat = "identity") +
  ylab("n") +
  xlab("Number of edges") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 20),
                    axis.text.y = element_text(size = 20),
                    axis.title.x = element_text(size = 25),
                    axis.title.y = element_text(size = 25)) +
  theme(plot.margin=margin(10,10,10,10)) +
  theme(strip.text = element_text(size = 18),
        legend.position = c(0.8, 0.9),
        legend.title = element_blank())

# scar number and direction

## count how many scars from each direction

print("count how many scars from each direction")

table(unlist(scar_dir[,2:8]))

## How many % flakes that have dorsal scars from at least 3 complete different directions

print("How many % flakes that have dorsal scars from at least 3 complete different directions")

table(unlist(scar_dir[,2:8]))


## calculate GIUR

# text says ". Most specimens were extensively retouched, i.e.,
# more than XX% have a GIUR value greater than 0.X. "

# we need to compute the GIUR as T/t for each sector, then compute the mean of all sectors
# for each artefact

# looking at the GIUR data... there are some duplicate artefact numbers, which is a nuisance

# make the number ID unique
retouch$number <- make.unique(as.character(retouch$number), sep = "_")

retouch_giur_per_section <-
  retouch %>%
  dplyr::select(grep("^number$|_t|_T", names(retouch))) %>%
  gather(variable, value, -number) %>%
  separate(variable, c("section", "tee"), sep = 10) %>%
  pivot_wider(names_from =  tee, values_from = value) %>%
  mutate(t_T = t / T)  %>%
  filter(t_T <= 1)

retouch_giur_per_artefact <-
  retouch_giur_per_section %>%
  group_by(number) %>%
  summarise(mean_giur = mean(t_T, na.rm = TRUE))

mean(retouch_giur_per_artefact$mean_giur)

# how many higher than some value
sum(retouch_giur_per_artefact$mean_giur >= 0.5) / nrow(retouch_giur_per_artefact) * 100

# join metrics variables with GIUR data
library(stringr)
retouch_giur_per_artefact_with_metrics <-
  left_join(retouch_giur_per_artefact,
            flakes,
            by = "number") %>%
  # filter out those with repeated IDs
  filter(!str_detect(number, "_"))

# the average max dimension of retouched piece
average_max_dimension_of_retouch <- round(mean(retouch_giur_per_artefact_with_metrics$max.dimension, na.rm = TRUE),2)
# what percentage of retouched pieces have a GIUR greater than N
N <-  0.5
percentage_with_giur_greater_than_N <-
  round(mean(retouch_giur_per_artefact$mean_giur >= N)  * 100, 0)

#----------
# df_giur <- retouch[, c(1,8,10,12,14,16,18,20,22)]

# df_giur[, -1] <- df_giur[, -1] / retouch[, c(9,11,13,15,17,19,21,23)]

df_giur <- retouch_giur_per_artefact_with_metrics

df_giur_clustered <- left_join(df_giur, flakes_clustered[, c("number", "cluster")])

df_giur_clustered_melt <- melt(df_giur_clustered[,-1], id.vars = "cluster")

## compare GIUR for different mass categories

print("compare GIUR for different mass groups")

df_giur_clustered_melt$label <-
  paste("Cluster",df_giur_clustered_melt$cluster)

df_giur_clustered_melt <-
df_giur_clustered_melt %>%
  mutate(value = parse_number(value)) %>%
  filter(variable == "mean_giur") %>%
  filter(!is.na(cluster))

## calculate the median GIUR for each group

print("the median GIUR for each group")

tapply(df_giur_clustered_melt$value, df_giur_clustered_melt$cluster, median, na.rm=TRUE)

df_giur_clustered_melt_medians <-
df_giur_clustered_melt %>%
  group_by(label) %>%
  summarise(median_giur = median(value))

giur_overall_median <- tibble(giur_overall_median = median(df_giur_clustered_melt$value))

library(ggbeeswarm)
p_GIUR_mass <-
  ggplot(na.omit(df_giur_clustered_melt[df_giur_clustered_melt$value <= 1,]),
                      aes(label, value)) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.2) +
  theme_bw() +
  ylab("GIUR") +
  xlab("") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25)) + theme(plot.margin=margin(10,10,10,10))

# Invasiveness index
print("results for invasiveness index")

df_invasive <- retouch[, c(1, 24:39)]

# need to compute the II as sum of each section divided by the number of sections
df_invasive_ii <-
df_invasive %>%
  rowwise() %>%
  mutate(II = sum(section_1_II,
                  section_2_II,
                  section_3_II,
                  is.numeric(section_4_II),
                  section_5_II,
                  section_6_II,
                  section_7_II,
                  section_8_II,
                  section_9_II,
                  section_10_II,
                  section_11_II,
                  section_12_II,
                  section_13_II,
                  section_14_II,
                  section_15_II,
                  section_16_II,
                  na.rm = TRUE) / 16) %>%
  dplyr::select(number, II)

df_invasive_clustered <- left_join(df_invasive_ii, flakes_clustered[, c("number", "cluster")])

df_invasive_clustered_melt <-
  melt(df_invasive_clustered[,-1], id.vars = "cluster") %>%
  filter(!is.na(cluster)) %>%
  mutate(label = paste0("Cluster ", cluster))

II_overall_median <- tibble(II_overall_median = median(df_invasive_clustered_melt$value))

p_II <-
  ggplot(df_invasive_clustered_melt) +
         aes(as.factor(label),
             value) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.1) +
  theme_bw() +
  xlab("") +
  ylab("II") +
  ylim(0, 0.5) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25)) + theme(plot.margin=margin(10,10,10,10))


### plot together

figure_retouch <-
  cowplot::plot_grid(plot_grid(p_flake_ret_dim,
                               p_edge_shape,
                               p_edge_angle, ncol = 3,
                               labels = "auto",
                               label_size = 26,
                               align = 'vh',
                               axis = 'lr'),
                     plot_grid(p_GIUR_mass,
                               p_II,
                               ncol = 1, labels = c("d", "e"), label_size = 26),
                     plot_grid(
                               p_edge_number,
                               p_edge_layer,
                               ncol = 2, rel_widths = c(1.5,1.5),
                               labels = c( "f", "g"),
                               label_size = 26,
                               align = 'vh',
                               axis = 'lr'),
                     ncol = 1,
                     align = 'vh',
                     axis = 'lr',
                     #rel_heights = c(1, 1.5),
                     vjust = 1,
                     hjust = -0.5,
                     scale = 0.9) #

save_plot("analysis/figures/S8 Figures for retouch.png", figure_retouch,
          base_aspect_ratio = 1,
          base_height = 21,
          base_width = 22)


## figure for quina

#############################################################################
## Levallois characteristics

# how many levallois?
unique(flakes$type1)

leva <- flakes %>%
  filter(grepl("LVT|LVF|LVFB", type1)) %>%
  filter(!grepl("LVF_", type1))

non_leva <- flakes %>%
  filter(!grepl("LVT|LVF|LVFB", type1))

flakes_L <-
  mutate(flakes, L = ifelse(grepl("LVT|LVF|LVFB", type1), "Leva", "Non_leva"))

# are the levallois pieces bigger?

### compare the mass of leva and non-leva
print("compare the mass of Leva and non-leva flakes")

p_leva_mass <- ggplot(flakes_L, aes(x = L,y = mass)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  xlab("Category") + ylab("Mass (g)") +
  scale_y_log10() + theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

# is it significant?

print("t-test to see if it is statistically significant?")

t.test(data = flakes_L, mass ~ L)

# leva is lighter


### compare the dimension of leva and non-leva
print("compare the dimension of Leva and non-leva flakes")

p_leva_dim <- ggplot(flakes_L, aes(x = L,y = max_dimension)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  xlab("Category") + ylab("Max dimension (mm)") +
  scale_y_log10() + theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

# is it significant?

print("t-test to see if it is statistically significant?")

t.test(data = flakes_L, max_dimension ~ L)

# no

### compare the thickness of leva and non-leva
print("compare the thickness of Leva and non-leva flakes")

p_leva_thick <- ggplot(flakes_L, aes(x = L, y = `Thickness_50%max`)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  xlab("Category") + ylab("Thickness (mm)") +
  theme_bw() + scale_y_log10() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25))

# is it significant?

print("t-test to see if it is statistically significant?")

t.test(data = flakes_L, `Thickness_50%max` ~ L)

# yes, leva flakes are much thinner

# explore variation in thickness in these groups

thickness_long <- melt(flakes_L[, c("Thickness_25%max",
                                    "Thickness_50%max",
                                    "Thickness_75%max", "L")])
thick_plot <- ggplot(thickness_long, aes(x = value, color = variable)) +
  geom_density(alpha = 0.3) + theme_bw() +
  xlab("Thickness (mm)") + ylab("Density") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size = 18)) +
  scale_color_manual(labels = c("25%", "50%", "75%"),
                     values = c("red","green", "blue"))

print("variation of thickness for all flakes (red = 25%max_dim,green = 50%max,blue=75%max")

thick_plot


# explore variation in width in these groups

width_long <- melt(flakes_L[, c("Width_25%max",
                                "Width_50%max",
                                "Width_75%max", "L")])
width_plot <- ggplot(width_long, aes(x = value, color = variable)) +
  geom_density(alpha = 0.3) + theme_bw() +
  xlab("Width (mm)") + ylab("Density") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size = 18)) +
  scale_color_manual(labels = c("25%", "50%", "75%"),
                     values = c("red","green", "blue"))

print("variation of width for all flakes (red = 25%max_dim,green = 50%max,blue=75%max")

width_plot

# by size class that we identified earlier

print("compare thickness of leva and non-leva (red = 25%max_dim,
      green = 50%max, blue=75%max")

p_leva_4 <- thick_plot +
  facet_wrap(~ L, ncol = 1) +
  theme(strip.text = element_text(size = 18))

p_leva_5 <- width_plot +
  facet_wrap(~ L, ncol = 1) +
  theme(strip.text = element_text(size = 18))


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
                     plot_grid(p_GIUR_quina, labels= "f", ncol = 2,
                               rel_widths = c(1.5,1),
                               label_size = 26),
                     nrow = 3,
                     align = 'v',
                     axis = 'lr',
                     vjust = 1.5,
                     hjust = -0.5,
                     scale = 0.9) #

save_plot("Figures for Leva & quina.png", figure_leva_quina,
          base_aspect_ratio = 1,
          base_height = 18,
          base_width = 22)


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
                   max_dimension = CV(max_dimension),
                   oriented_width = CV(oriented_width),
                   oriented_thickness = CV(oriented_thickness),
                   length = CV(length),
                   n = n())

print("summary of cv for thickness and width for leva and non-leva flakes")

write.csv(cv_res, "CV of Leva and non-Leva flakes.csv")

# BM: reshape to long and add col showing diff:

cv_res %>%
  pivot_longer(-L) %>%
  pivot_wider( names_from = L ) %>%
  mutate(diff = abs(Leva - Non_leva))



