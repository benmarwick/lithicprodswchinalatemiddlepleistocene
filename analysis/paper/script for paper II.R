
#install required packages
library(dplyr)
library(knitr)
library(ggplot2)
library(readxl)
library(fitdistrplus)
library(plyr)
library(tidyr)
library(classInt)




# read in the data

file_name <- "artefacts-after Sam_Dec2018_quina_modified.xls" 

flakes <- as.data.frame(read_excel(file_name, sheet = "flake basics"))
cores <- as.data.frame(read_excel(file_name, sheet = "core basics"))
debris <- as.data.frame(read_excel(file_name, sheet = "chunk&debris"))
retouch_1 <- as.data.frame(read_excel(file_name, sheet = "retouch"))
core_flake_scars_1 <- as.data.frame(read_excel(file_name, sheet = "core flake scars"))
edge_angle_1 <- as.data.frame(read_excel(file_name, sheet = "edge angle"))
retouched_pebble_chunk <- as.data.frame(read_excel(file_name, sheet = "Pebble&chunk"))


### raw materials

raw_material_table <- data.frame("category" = character(),
                                 'chert' = numeric(),
                                 'limestone' = numeric(),
                                 'basalt' = numeric(),
                                 'quartz' = numeric(),
                                 'sandstone' = numeric(),
                                 stringsAsFactors = FALSE)

row <- 1

raw_material_table[row, names(unlist(table(na.omit(flakes$material))))] <- 
  unlist(table(na.omit(flakes$material)))

raw_material_table[row, 1] <- "flake"
row <- row + 1


raw_material_table[row, names(unlist(table(na.omit(cores$material))))] <- 
  unlist(table(na.omit(cores$material)))
raw_material_table[row, 1] <- "core"
row <- row + 1

raw_material_table[row, names(unlist(table(na.omit(debris$material))))] <- 
  unlist(table(na.omit(debris$material)))
raw_material_table[row, 1] <- "debris"
row <- row + 1


raw_material_table[row, names(unlist(table(flakes$material[flakes$number %in% retouch_1$number])))] <- 
  unlist(table(flakes$material[flakes$number %in% retouch_1$number]))
raw_material_table[row, 1] <- "retouched pieces"
row <- row + 1



###tools raw materials

test <- as.matrix(t(table(flakes[flakes$number %in% retouch_1$number, 
                     c("material", "type2")])))

raw_material_table[row, colnames(test)] <- test["backed knife",]
raw_material_table[row, 1] <- "backed knife"
row <- row + 1


raw_material_table[row, colnames(test)] <- colSums( test[c("bec", "bec,scp"),])
raw_material_table[row, 1] <- "bec"
row <- row + 1


raw_material_table[row, colnames(test)] <- 
  colSums( test[c("borer","borer,dent", "borer,dent,scp","borer,end,scp","borer,notch",
                  "borer,notch,scp","borer,scp", "borer,scp,end","borer,strt" ), ])
raw_material_table[row, 1] <- "borer"
row <- row + 1


raw_material_table[row, colnames(test)] <- 
  colSums( test[c("BUR", "BUR? CORE?","BURIN" ), ])
raw_material_table[row, 1] <- "BUR"
row <- row + 1

raw_material_table[row, colnames(test)] <- test["chopper", ]
raw_material_table[row, 1] <- "chopper"
row <- row + 1


raw_material_table[row, colnames(test)] <- test["cleaver", ]
raw_material_table[row, 1] <- "cleaver"
row <- row + 1


raw_material_table[row, colnames(test)] <- 
  colSums( test[c("dent", "dent,borer,scp","dent,end","dent,notch",  "dent,scp",
                  "dent/NOT", "dent/POINT"  ), ])
raw_material_table[row, 1] <- "Denticular"
row <- row + 1


raw_material_table[row, colnames(test)] <- 
  colSums( test[c("end", "end,borer" , "end,borer,scp" ,"end,scp" ,"endscp" ), ])
raw_material_table[row, 1] <- "Endscraper"
row <- row + 1


raw_material_table[row, colnames(test)] <- 
  colSums( test[c("leva point","point" ,"POINT" , "point break" ,
                  "point,broken", "point?"  ), ])
raw_material_table[row, 1] <- "Point"
row <- row + 1


raw_material_table[row, colnames(test)] <- 
  colSums( test[c("moon","scp", "sidescp" , "sidescp-cvx"  ), ])
raw_material_table[row, 1] <- "scraper"
row <- row + 1


raw_material_table[row, colnames(test)] <- 
  colSums( test[c("NATBACK", "nature backed"), ])
raw_material_table[row, 1] <- "nature backed"
row <- row + 1


raw_material_table[row, colnames(test)] <- 
  colSums( test[c( "notcch", "notch" , "notch,borer"  , "notch,end","notch,scp" ,       
                   "notch/DENT"), ])
raw_material_table[row, 1] <- "notch"
row <- row + 1


raw_material_table[row, colnames(test)] <- 
  colSums( test[c("TANG", "TANGED",  "TANGED POINT", "TANGED POINT /NOT", 
                  "tanged point?", "TANGED POINT?"  ), ])
raw_material_table[row, 1] <- "tanged point"
row <- row + 1




raw_material_table[is.na(raw_material_table)] <- 0 ##convert na to 0

raw_material_table$total <- rowSums(raw_material_table[,-1])

write.csv(raw_material_table, "raw_material_table.csv")


x <- raw_material_table

prop_raw_material <- cbind(id= x[, 1], x[, -1]/rowSums(x[, -1])*100)

write.csv(prop_raw_material, "raw_material_table (percentage).csv")




### cores
core_summary <- cores[0, c(6:17)]

core_summary["mean",] <- c("mean" = apply(na.omit(cores[,c(6:17)]), 2, mean))

core_summary["sd",] <- c("sd" = apply(na.omit(cores[,c(6:17)]), 2, sd))

core_summary <- rbind(core_summary, 
                      sapply(na.omit(cores[,6:17]), function(x) quantile(x, c(0.25, 0.5, 0.75))))



core_number <- length(na.omit(cores$number))



# function to compute frequencies and make a plot
f <-  function(the_data, the_column) {
  data.frame(table(the_data[the_column])) %>% 
    ggplot(aes(x = reorder(Var1, Freq), y = Freq)) +
    geom_bar(stat = "identity") +
    ylab("n") +
    xlab(the_column) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}

# some plots of frequencies

## geometry

f(cores, "core_geometry")

core_geom_table <-  table(na.omit(cores$core_geometry))

prop.table(core_geom_table)


## platform number

f(cores, "platform")

core_plat_table <-  table(na.omit(cores$platform))

prop.table(core_plat_table)

## platform type

f(cores, "platform_preparation")

core_plat_type <-  table(na.omit(cores$platform_preparation))

prop.table(core_plat_type)


## platform rotation

core_plat_rotat <-  table(na.omit(cores$rotations))

prop.table(core_plat_rotat)


## core reduction type

core_type_table <- table(na.omit(cores$type))

prop.table(core_type_table)


### core scar number

core_scar_table <- table(na.omit(cores$scar_number))

prop.table(core_scar_table)


p1 <- hist(na.omit(cores$scar_number[cores$scar_number<20]), probability = TRUE,
     breaks = 1:15) 

p1$density = p1$counts/sum(p1$counts)*100

pdf("===core scar number.pdf", width = 7, height = 6)
plot(p1, freq=FALSE, 
     xlab = "Scar number of cores", ylab = "Density (%)",
     main = NULL,
     xlim = c(1,15), ylim = c(0,50), cex.lab = 1.2)

dev.off()

## cortex

core_cortex_table <- table(na.omit(cores$cortex_percentage))

prop.table(core_cortex_table)

p1 <- hist(na.omit(cores$cortex_percentage), probability = TRUE) 

p1$density = p1$counts/sum(p1$counts)*100

pdf("===core cortex.pdf", width = 7, height = 6)
plot(p1, freq=FALSE, 
     xlab = "Cortex coverage of cores (%)", ylab = "Density (%)",
     main = NULL,
     xlim = c(0,100), ylim = c(0,70), cex.lab = 1.2)

dev.off()


#### flakes

flk_number <- length(na.omit(flakes$number))

# type

# dimension

# platform type and dimension, shape

# scar number, direction (scar direction cake figure)

## cortext



#### retouched

# dimension








#### whole assemblage

## how many flakes

number_of_flakes <- nrow(flakes)

## how many retouched flake and flake breaks

retouch_type <- unique(flakes$type1)[unique(flakes$type1) != "LVF"]

tool_type <- unique(flakes$type2[!(is.na(flakes$type2)) & 
                                   !(flakes$type2 %in% c("blade", "crest", "tablet",
                                                         "tablet break", "PSD POINT", "leva",
                                                         "leva?", "leva point", "kombewa",
                                                         "leva point?")) ])

retouched_flk_pieces <- flakes[(flakes$type1 %in% retouch_type &
                                  flakes$type2 %in% tool_type) |
                                 grepl("ret", flakes$type1), ]

number_of_retouched_flk_pieces <- nrow(retouched_flk_pieces)

## how many complete flakes

complet_flk <- flakes[(flakes$type1 %in% c("flk", "leva", "DBD", "KBW",
                                           "LVF?", "leva flake?", "flake", "tablet",
                                           "crest", "blade", "BLD", "leva?", "kmobewa")) &
                        (flakes$type2 %in% c("tablet", "leva point?", "kombewa",
                                             "blade", "PSD POINT", "crest", "leva",
                                             "leva?", "leva point") |
                           is.na(flakes$type2)) | (flakes$type1 %in% c("LVF")), ]

number_of_complete_flk <- nrow(complet_flk)

## how many flake fragment

flk_fragment <- flakes[flakes$type1 %in% c("LVFB", "flake brk", "blade brk?",
                                           "proximal", "FACETED", "brk") &
                         is.na(flakes$type2), ]

number_of_flk_fragment <- nrow(flk_fragment)


## The average maximum length of the flakes pieces

mean_max_length_flk <- mean(flakes$max_dimension, na.rm = TRUE)

sd_max_length_flk <- sd(complet_flk$max_dimension, na.rm = TRUE)

mean_thickness_flk <- mean(flakes$`Thickness_50%max`, na.rm = TRUE)


## The average scar number on dorsal side of flakes

mean_scar_flk <- mean(complet_flk$scar_number, na.rm = TRUE) 


par(mar = c(5,5,2,5))

dist_scar_flk <- hist(complet_flk$scar_number) ## distribution of scar number

par(new = T)

ec_scar_flk <- ecdf(complet_flk$scar_number) ## cumulative density

plot(x = dist_scar_flk$mids, y=ec_scar_flk(dist_scar_flk$mids)*max(dist_scar_flk$counts), 
     col = rgb(0,0,0,alpha=0), axes=F, xlab=NA, ylab=NA)
lines(x = dist_scar_flk$mids, y=ec_scar_flk(dist_scar_flk$mids)*max(dist_scar_flk$counts), 
      col ='red')
axis(4, at=seq(from = 0, to = max(dist_scar_flk$counts), length.out = 11), labels=seq(0, 1, 0.1), col = 'red', col.axis = 'red')
mtext(side = 4, line = 3, 'Cumulative Density', col = 'red')

## cortext of flakes

dist_cort_percentage_flk <- hist(complet_flk$cortex_percentage) ## distribution of cortext percentage



## retouched edges of tools

retouched_pieces_name <- c(retouched_flk_pieces$number, retouched_pebble_chunk$number)

retouched_pieces <- retouch[retouch$number %in% retouched_pieces_name,]

mean_dim_retouched_pieces <- mean(rbind(retouched_flk_pieces, 
                                        retouched_pebble_chunk)$max_dimension,
                                  na.rm = TRUE)

number_of_retouched_pieces <- length(retouched_pieces_name)


scp_dent_flk <- retouched_flk_pieces %>% 
  filter(grepl(c("scp", "dent"), type2)) ## sidescrapper and denticulate in flakes

scp_dent_pebble <- retouched_pebble_chunk %>% 
  filter(grepl(c("scp", "dent"), type2)) ## sidescrapper and denticulate in pebble & chunk


notch_flk <- retouched_flk_pieces %>% 
  filter(grepl(c("not"), type2))   ## notch in flakes

notch_pebble <- retouched_pebble_chunk[grepl("not", retouched_pebble_chunk$type2),]

borer_flk <- retouched_flk_pieces %>% 
  filter(grepl(c("bor"), type2)) ## borer in flakes

borer_pebble <- retouched_pebble_chunk[grepl("bor", retouched_pebble_chunk$type2),]

## type table for the retouched pieces



##number of edges

number_of_edge <- sum(retouched_pieces$`number of edge`, na.rm = TRUE)

number_of_edge_greater_than_1 <- nrow(retouched_pieces[retouched_pieces$number %in% retouched_pieces_name &
                                                         retouched_pieces$`number of edge` > 1, ])

number_of_straight_edge <- retouched_pieces %>% 
  filter(grepl(c("strt"), `edge shape`)) 

number_of_convex_edge <- retouched_pieces %>% 
  filter(grepl(c("cvx"), `edge shape`)) 

number_of_concave_edge <- retouched_pieces %>% 
  filter(grepl(c("ccv"), `edge shape`)) 


### average dimension of cores

mean_dim_core <- mean(cores$max_dimension, na.rm = TRUE)

mean_mass_core <- mean(as.numeric(cores$mass), na.rm = TRUE)

## raw materials of cores
count_material_cores <- as.data.frame(table(cores$material))
percent_material_core <- as.data.frame(prop.table(table(cores$material)))

## geometry of cores

count_geometry_cores <- as.data.frame(table(cores$core_geometry))
percent_geometry_core <- as.data.frame(prop.table(table(cores$core_geometry)))

## scar of cores

mean_scar_cores <- mean(cores$scar_number, na.rm = TRUE) 

mean_scar_length_core <- mean(as.numeric(unlist(core_flake_scars[, 2:ncol(core_flake_scars)])), 
                              na.rm = TRUE)

par(mar = c(5,5,2,5))

dist_scar_core <- hist(cores$scar_number) ## distribution of scar number

par(new = T)

ec_scar_core <- ecdf(cores$scar_number) ## cumulative density

plot(x = dist_scar_core$mids, y=ec_scar_core(dist_scar_core$mids)*max(dist_scar_core$counts), 
     col = rgb(0,0,0,alpha=0), axes=F, xlab=NA, ylab=NA)
lines(x = dist_scar_core$mids, y=ec_scar_core(dist_scar_core$mids)*max(dist_scar_core$counts), 
      col ='red')
axis(4, at=seq(from = 0, to = max(dist_scar_core$counts), length.out = 11), labels=seq(0, 1, 0.1), col = 'red', col.axis = 'red')
mtext(side = 4, line = 3, 'Cumulative Density', col = 'red')

## cortext of cores

dist_cort_percentage_core <- hist(cores$cortex_percentage) ## distribution of cortext percentage

percent_cortext_core <- as.data.frame(prop.table(table(cores$cortex_percentage)))


## platform of cores

type_platform_core <- as.data.frame(prop.table(table(cores$platform_preparation)))


## number of cores with prepared platform

prepared_core <- cores[cores$platform_preparation %in% c("faceted", "PREPARED") &
                         cores$type %in% c("single", "double", "multp", "blade?", "MUTP",
                                           "?"), ] 

geometry_prepared_core <- as.data.frame(table(prepared_core$core_geometry))

prop_geometry_prepared_core <- as.data.frame(prop.table(table(prepared_core$core_geometry)))

dist_platform_prepared_core <- hist(prepared_core$platform) ## distribution of platform

mean_dim_prepared_core <- mean(prepared_core$max_dimension, na.rm = TRUE)


## number of truncated-faceted pieces

truncated_faceted_cores <- cores[cores$type == "TF", ]


## number of débordants

debordants <- flakes[flakes$type1 == "DBD", ]

## flakes with prepared platform

prepared_flk <-  flakes[flakes$platform %in% c("faceted", 
                                               "FACETED", "facted", "FACETED?", "faceted(LEVA LIKE)") &
                          flakes$type1 != "LVF", ]

prepared_tool <- prepared_flk[!is.na(prepared_flk$type2), ]

prop_plat.shape_prepared_flk <- as.data.frame(prop.table(table(prepared_flk$platform_shape)))

mean_plat.width_prepared_flk <- mean(prepared_flk$platform_width, na.rm = TRUE)

mean_plat.thick_prepared_flk <- mean(prepared_flk$platform_thickness, na.rm = TRUE)

mean_max.dim_prepared_flk <- mean(prepared_flk$max_dimension, na.rm = TRUE)

dist_cort_percentage_prepared_flk <- hist(prepared_flk$cortex_percentage)

## broken flakes

broken_flk <- flakes[flakes$type1 %in% c("ret brk", "LVFB","flake brk", 
                                         "blade brk?", "brk", "proximal", "FACETED"), ]


