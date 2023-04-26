library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(camtrapR)
library(sf)
library(plotrix)
library(here)
library(patchwork)
'%!in%' <- function(x,y)!('%in%'(x,y))

source("New Prep Data.R")

diet_pals <- c("Granivore" = "#ff8339", "Insectivore" = "#9462cd", "Nectar" = "#ffc510",
               "Omnivore" = "#316aac")

nest_pals <- c("Tree" = "#4ac541", "Shrub" = "#ff834a", "cavity_primary" = "#e6a4de",
               "cavity_secondary" = "#c5ac10", "Ground" = "8bbdf6")

setwd("/Users/Kendall/Desktop/AudioMoth_fire_occu")
group_info <- read_csv("data/group info.csv")

metadata <- read_csv("data/metadata.csv") %>%
  dplyr::rename(Site = Camera)

rm(data2020)
rm(data2021)
rm(data2022)
#unburned_cam_IDs <- c("A13", "B12", "D14", "C15", "C17", "E17", "E15", "B14", "D16", "E13", "D12")

data_viz <- data %>%
  filter(confidence > .95)
#rm(data)

species_list <- unique(data_viz$Species)

group_info <- group_info %>%
  filter(Species %in% species_list) %>%
  select(-scientific_name)

data_viz <- left_join(data_viz, group_info, by = "Species") %>%
  filter(`Include?` == "Y") %>%
  mutate(Year = year(Date),
         Site_ID = paste0(Site, "_", Year))

count_filter <- data_viz %>%
  group_by(Site_ID, Species) %>%
  dplyr::count() %>%
  filter(n <= 1) %>%
  mutate(first_filter = paste(Site_ID,Species, sep = "_"))

filter_list <- count_filter$first_filter

data_viz <- data_viz %>% 
  mutate(first_filter = paste(Site_ID, Species, sep = "_"))

data_viz <- data_viz %>% 
  filter(first_filter %notin% filter_list) %>%
  # filter(Year != "2022") %>%
  dplyr::rename(Camera = Site)

#read in operation info
am_oplog <- read_csv("data/Operation Logs/audiomoth_operation_log.csv")# %>%
# dplyr::rename(Site = Camera)

am_oplog$Problem1_from <- as.character(mdy(am_oplog$Problem1_from))
am_oplog$Problem1_to <- as.character(mdy(am_oplog$Problem1_to))
am_oplog$Problem2_from <- as.character(mdy(am_oplog$Problem2_from))
am_oplog$Problem2_to <- as.character(mdy(am_oplog$Problem2_to))
am_oplog$Problem3_from <- as.character(mdy(am_oplog$Problem3_from))
am_oplog$Problem3_to <- as.character(mdy(am_oplog$Problem3_to))
am_oplog$Problem4_from <- as.character(mdy(am_oplog$Problem4_from))
am_oplog$Problem4_to <- as.character(mdy(am_oplog$Problem4_to))
am_oplog$Problem5_from <- as.character(mdy(am_oplog$Problem5_from))
am_oplog$Problem5_to <- as.character(mdy(am_oplog$Problem5_to))
am_oplog$Start <- as.character(mdy(am_oplog$Start))
am_oplog$End <- as.character(mdy(am_oplog$End))


cam_op <- cameraOperation(am_oplog, stationCol = "Camera", setupCol="Start", retrievalCol="End", 
                          hasProblems = TRUE,byCamera = FALSE, allCamsOn=TRUE, camerasIndependent=FALSE,
                          dateFormat = "%Y-%m-%d", writecsv = FALSE)
#Visualize Operation
camopPlot <- function(camOp, 
                      palette = "Heat"){
  
  which.tmp <- grep(as.Date(colnames(camOp)), pattern = "01$")
  label.tmp <- format(as.Date(colnames(camOp))[which.tmp], "%Y-%m")
  at.tmp <- which.tmp / ncol(camOp)
  
  values_tmp <- sort(na.omit(unique(c(camOp))))
  
  # hcl.colors only exists in R >3.6.0, use heat.colors for earlier versions
  if(getRversion() >= "3.6.0") {
    image_colors <- grDevices::hcl.colors(n = length(values_tmp), palette = palette, rev = TRUE)
  } else {
    image_colors <- heat.colors(n = length(values_tmp), rev = TRUE)
  }
  
  image(t(as.matrix(camOp)), xaxt = "n", yaxt = "n", col = image_colors)
  axis(1, at = at.tmp, labels = label.tmp)
  axis(2, at = seq(from = 0, to = 1, length.out = nrow(camOp)), labels = rownames(camOp), las = 1)
  abline(v = at.tmp, col = rgb(0,0,0, 0.2))
  box()
}
cam_plot <- camopPlot(camOp = cam_op)


cam_op_rai <- rownames_to_column(as.data.frame(site_op), var = "Camera")


allclassifications <- c(Omnivore = NA_real_, Granivore = NA_real_, Insectivore = NA_real_,
                        Nectar = NA_real_)

##RAI function

diet.rai.calculate <- function(record.table, start.date, end.date, camop, timeperiod.name) {
  # define names to use in file names (just removes the dashes)
  start.name <- gsub("-", "", start.date)
  end.name <- gsub("-", "", end.date)
  
  # calculate how long the camera was functioning in that time period
  
  # selects columns within specified dates
  camop.subset <- dplyr::select(camop, Camera, start.date:end.date) 
  
  # sum rows within specified dates (there are 1s when camera was operating, NA when not)
  camop.subset$Operation <- rowSums(dplyr::select(camop.subset, start.date:end.date), na.rm=TRUE) 
  
  # get rid of the individual day columns, just select Camera, Operation
  camop.subset <- dplyr::select(camop.subset, Camera, Operation) 
  
  # format start and end dates as dates
  start.date <- as.Date(start.date)
  end.date <- as.Date(end.date)
  # subset record table to date of interest
  record.table.subset <- record.table[record.table$Date >= start.date & record.table$Date <= end.date,]
  # calculate number of observations of each classification type at each camera
  records <- record.table.subset %>%
    dplyr::group_by(Diet, Camera) %>%
    dplyr::summarise(Detections = n()) %>%     # counts number of observations of each species
    spread(key = Diet, value = Detections)  # gets from long to wide format  
  
  # add columns for classes not present
  records <- add_column(records, !!!allclassifications[!names(allclassifications) %in% names(records)])
  
  # gather data so each class-camera is its own row again
  records <- records %>% gather(2:ncol(records), key = "Class", value = "Count")
  
  # replace NA with 0 
  records[is.na(records)] <- 0
  
  # join camera operation dates and observations
  RAI.table <- plyr::join(records, camop.subset)
  
  # calculate RAI
  RAI.table$RAI <- RAI.table$Count / RAI.table$Operation
  
  # add new column for time period
  RAI.table$TimePeriod <- timeperiod.name
  
  # write csv
  write.csv(RAI.table, file = paste(here::here('data'), "/RAI_", timeperiod.name, "_", start.name, "_", end.name, ".csv", collapse = "", sep = ""),row.names=F)
  
  return(RAI.table)
  
}

diet_rai_2020 <- diet.rai.calculate(record.table = data_viz, start.date = "2020-04-23", end.date = "2020-07-31", camop = cam_op_rai, timeperiod.name = "2020")
diet_rai_2021 <- diet.rai.calculate(record.table = data_viz, start.date = "2021-03-01", end.date = "2021-07-31", camop = cam_op_rai, timeperiod.name = "2021")
diet_rai_2022 <- diet.rai.calculate(record.table = data_viz, start.date = "2022-03-01", end.date = "2022-07-08", camop = cam_op_rai, timeperiod.name = "2022")


all_diet_rai <- rbind(diet_rai_2020, diet_rai_2021, diet_rai_2022)
# 
# all_rai$Period <- NA
# for (i in 1:nrow(all_diet_rai)) {
#   if (all_rai$TimePeriod[i] == "2016") {
#     all_rai$Period[i] <- "2016"
#   }
#   if (all_rai$TimePeriod[i] == "2017") {
#     all_rai$Period[i] <- "2017"
#   }
#   if (all_rai$TimePeriod[i] == "2018") {
#     all_rai$Period[i] <- "2018"
#   }
#   if (all_rai$TimePeriod[i] == "2019") {
#     all_rai$Period[i] <- "2019"
#   }
#   if (all_rai$TimePeriod[i] == "2020") {
#     all_rai$Period[i] <- "2020"
#   }
# }

all_diet_rai$TimePeriod <- fct_relevel(all_diet_rai$TimePeriod, "2020", "2021", "2022")

all_diet_rai <- all_diet_rai %>%
  dplyr::rename(Site = Camera)

all_diet_rai <- left_join(all_diet_rai, metadata, by = "Site")

diet_rai_plot <- all_diet_rai %>%
  ggplot(aes(x = Class, y = RAI, fill = fire)) + geom_boxplot()

diet_rai2020_plot <- all_diet_rai %>%
  filter(TimePeriod == "2020") %>%
  ggplot(aes(x = Class, y = RAI, fill = fire)) + geom_boxplot()

diet_rai2021_plot <- all_diet_rai %>%
  filter(TimePeriod == "2021") %>%
  ggplot(aes(x = Class, y = RAI, fill = fire)) + geom_boxplot()

diet_rai2022_plot <- all_diet_rai %>%
  filter(TimePeriod == "2022") %>%
  ggplot(aes(x = Class, y = RAI, fill = fire)) + geom_boxplot()

granivore_diet_rai <- all_diet_rai %>%
  filter(Class == "Granivore") %>%
  ggplot(aes(x = soil.severity.large, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#ff8339") +
  scale_fill_discrete(name = "Diet Group", type = c("#ff8339")) + 
  #scale_color_manual(values=c("#ac835a","#25E811","#2320D8"))
  scale_x_continuous(breaks=c(0, 200, 400, 600),
                     labels=c("Unburned", "Low", "Moderate", "Moderate-High")) +
  #ylim(0,5) +
  xlab("Burn Severity") +
  theme_minimal()
ggsave(granivore_diet_rai, filename = "Figures/granivore_sev.png")

insectivore_diet_rai <- all_diet_rai %>%
  filter(Class == "Insectivore") %>%
  ggplot(aes(x = soil.severity.large, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#9462cd") +
  scale_fill_discrete(name = "Diet Group", type = c("#9462cd")) + 
  #scale_color_manual(values=c("#ac835a","#25E811","#2320D8"))
  scale_x_continuous(breaks=c(0, 200, 400, 600),
                     labels=c("Unburned", "Low", "Moderate", "Moderate-High")) +
  #ylim(0,5) +
  xlab("Burn Severity") +
  theme_minimal()
ggsave(insectivore_diet_rai, filename = "Figures/insectivore_sev.png")

omnivore_diet_rai <- all_diet_rai %>%
  filter(Class == "Omnivore") %>%
  ggplot(aes(x = soil.severity.large, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#316aac") +
  scale_fill_discrete(name = "Diet Group", type = c("#316aac")) + 
  #scale_color_manual(values=c("#ac835a","#25E811","#2320D8"))
  scale_x_continuous(breaks=c(0, 200, 400, 600),
                     labels=c("Unburned", "Low", "Moderate", "Moderate-High")) +
  #ylim(0,5) +
  xlab("Burn Severity") +
  theme_minimal()
ggsave(omnivore_diet_rai, filename = "Figures/omnivore_sev.png")

nectar_diet_rai <- all_diet_rai %>%
  filter(Class == "Nectar") %>%
  ggplot(aes(x = soil.severity.large, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#ffc510") +
  scale_fill_discrete(name = "Diet Group", type = c("#ffc510")) + 
  #scale_color_manual(values=c("#ac835a","#25E811","#2320D8"))
  scale_x_continuous(breaks=c(0, 200, 400, 600),
                     labels=c("Unburned", "Low", "Moderate", "Moderate-High")) +
  #ylim(0,5) +
  xlab("Burn Severity") +
  theme_minimal()
ggsave(nectar_diet_rai, filename = "Figures/nectar_sev.png")

all_diet_sev <- (granivore_diet_rai + insectivore_diet_rai)/
  (omnivore_diet_rai + nectar_diet_rai) +
  plot_annotation(tag_levels = 'a')
ggsave(all_diet_sev, filename = "Figures/all_diet_rai_severity.png")

granivore_pyro_rai <- all_diet_rai %>%
  filter(Class == "Granivore") %>%
  ggplot(aes(x = pyrodiversity.large.scaled, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#ff8339") +
  scale_fill_discrete(name = "Diet Group", type = c("#ff8339")) + 
  #scale_color_manual(values=c("#ac835a","#25E811","#2320D8"))
  ylim(0,5) +
  xlab("Pyrodiversity") +
  theme_minimal()
ggsave(granivore_pyro_rai, filename = "Figures/granivore_pyro.png")

insectivore_pyro_rai <- all_diet_rai %>%
  filter(Class == "Insectivore") %>%
  ggplot(aes(x = pyrodiversity.large.scaled, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#9462cd") +
  scale_fill_discrete(name = "Diet Group", type = c("#9462cd")) + 
  #scale_color_manual(values=c("#ac835a","#25E811","#2320D8"))
  ylim(0,5) +
  xlab("Pyrodiversity") +
  theme_minimal()
ggsave(insectivore_pyro_rai, filename = "Figures/insectivore_pyro.png")

omnivore_pyro_rai <- all_diet_rai %>%
  filter(Class == "Omnivore") %>%
  ggplot(aes(x = pyrodiversity.large.scaled, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#316aac") +
  scale_fill_discrete(name = "Diet Group", type = c("#316aac")) + 
  #scale_color_manual(values=c("#ac835a","#25E811","#2320D8"))
  ylim(0,5) +
  xlab("Pyrodiversity") +
  theme_minimal()
ggsave(omnivore_pyro_rai, filename = "Figures/omnivore_pyro.png")

nectar_pyro_rai <- all_diet_rai %>%
  filter(Class == "Nectar") %>%
  ggplot(aes(x = pyrodiversity.large.scaled, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#ffc510") +
  scale_fill_discrete(name = "Diet Group", type = c("#ffc510")) + 
  #scale_color_manual(values=c("#ac835a","#25E811","#2320D8"))
  ylim(0,5) +
  xlab("Pyrodiversity") +
  theme_minimal()
ggsave(nectar_pyro_rai, filename = "Figures/nectar_pyro.png")

all_diet_pyro <- (granivore_pyro_rai + insectivore_pyro_rai)/
  (omnivore_pyro_rai + nectar_pyro_rai) + plot_annotation('a')
ggsave(all_diet_pyro, filename = "Figures/all diet pyro.png")

allclassifications <- c(cavity_primary = NA_real_, cavity_secondary = NA_real_, Tree = NA_real_,
                        Shrub = NA_real_, Ground = NA_real_)

nest.rai.calculate <- function(record.table, start.date, end.date, camop, timeperiod.name) {
  # define names to use in file names (just removes the dashes)
  start.name <- gsub("-", "", start.date)
  end.name <- gsub("-", "", end.date)
  
  # calculate how long the camera was functioning in that time period
  
  # selects columns within specified dates
  camop.subset <- dplyr::select(camop, Camera, start.date:end.date) 
  
  # sum rows within specified dates (there are 1s when camera was operating, NA when not)
  camop.subset$Operation <- rowSums(dplyr::select(camop.subset, start.date:end.date), na.rm=TRUE) 
  
  # get rid of the individual day columns, just select Camera, Operation
  camop.subset <- dplyr::select(camop.subset, Camera, Operation) 
  
  # format start and end dates as dates
  start.date <- as.Date(start.date)
  end.date <- as.Date(end.date)
  # subset record table to date of interest
  record.table.subset <- record.table[record.table$Date >= start.date & record.table$Date <= end.date,]
  # calculate number of observations of each classification type at each camera
  records <- record.table.subset %>%
    dplyr::group_by(Nesting, Camera) %>%
    dplyr::summarise(Detections = n()) %>%     # counts number of observations of each species
    spread(key = Nesting, value = Detections)  # gets from long to wide format  
  
  # add columns for classes not present
  records <- add_column(records, !!!allclassifications[!names(allclassifications) %in% names(records)])
  
  # gather data so each class-camera is its own row again
  records <- records %>% gather(2:ncol(records), key = "Class", value = "Count")
  
  # replace NA with 0 
  records[is.na(records)] <- 0
  
  # join camera operation dates and observations
  RAI.table <- plyr::join(records, camop.subset)
  
  # calculate RAI
  RAI.table$RAI <- RAI.table$Count / RAI.table$Operation
  
  # add new column for time period
  RAI.table$TimePeriod <- timeperiod.name
  
  # write csv
  write.csv(RAI.table, file = paste(here::here('data'), "/RAI_", timeperiod.name, "_", start.name, "_", end.name, ".csv", collapse = "", sep = ""),row.names=F)
  
  return(RAI.table)
  
}


nest_rai_2020 <- nest.rai.calculate(record.table = data_viz, start.date = "2020-04-23", end.date = "2020-07-31", camop = cam_op_rai, timeperiod.name = "2020")
nest_rai_2021 <- nest.rai.calculate(record.table = data_viz, start.date = "2021-03-01", end.date = "2021-07-31", camop = cam_op_rai, timeperiod.name = "2021")
nest_rai_2022 <- nest.rai.calculate(record.table = data_viz, start.date = "2022-03-01", end.date = "2022-07-08", camop = cam_op_rai, timeperiod.name = "2022")


all_nest_rai <- rbind(nest_rai_2020, nest_rai_2021, nest_rai_2022)


all_nest_rai$TimePeriod <- fct_relevel(all_nest_rai$TimePeriod, "2020", "2021", "2022")

all_nest_rai <- all_nest_rai %>%
  dplyr::rename(Site = Camera)

all_nest_rai <- left_join(all_nest_rai, metadata, by = "Site")

all_nest_rai %>%
  # filter(TimePeriod == "2021") %>%
  ggplot(aes(x = Class, y = RAI, fill = fire)) + geom_boxplot()

all_nest_rai %>%
  #filter(TimePeriod == "2020") %>%
  filter(Class == "Tree") %>%
  ggplot(aes(x = soil.severity.small, y = RAI, color = Class, fill = Class)) + geom_smooth() 

all_nest_rai %>%
  #filter(TimePeriod == "2020") %>%
  filter(Class == "cavity_secondary") %>%
  ggplot(aes(x = pyrodiversity.small, y = RAI, color = Class, fill = Class)) + geom_smooth() 

tree_sev_rai <- all_nest_rai %>%
  filter(Class == "Tree") %>%
  ggplot(aes(x = soil.severity.large, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#4ac541") +
  scale_fill_discrete(name = "Nesting Group", type = c("#4ac541")) + 
  #scale_color_manual(values=c("#ac835a","#25E811","#2320D8"))
  scale_x_continuous(breaks=c(0, 200, 400, 600),
                     labels=c("Unburned", "Low", "Moderate", "Moderate-High")) +
  ylim(0,2) +
  xlab("Burn Severity") +
  theme_minimal()
ggsave(tree_sev_rai, filename = "Figures/tree_sev.png")

shrub_sev_rai <- all_nest_rai %>%
  filter(Class == "Shrub") %>%
  ggplot(aes(x = soil.severity.large, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#ff834a") +
  scale_fill_discrete(name = "Nesting Group", type = c("#ff834a")) + 
  #scale_color_manual(values=c("#ac835a","#25E811","#2320D8"))
  scale_x_continuous(breaks=c(0, 200, 400, 600),
                     labels=c("Unburned", "Low", "Moderate", "Moderate-High")) +
  ylim(0,2) +
  xlab("Burn Severity") +
  theme_minimal()
ggsave(shrub_sev_rai, filename = "Figures/shrub_sev.png")

all_nest_rai$Class <- recode(all_nest_rai$Class,
                             cavity_primary = "Primary Cavity",
                             cavity_secondary = "Secondary Cavity")

cavity1_sev_rai <- all_nest_rai %>%
  filter(Class == "Primary Cavity") %>%
  ggplot(aes(x = soil.severity.large, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#e6a4de") +
  scale_fill_discrete(name = "Nesting Group", type = c("#e6a4de")) + 
  #scale_color_manual(values=c("#ac835a","#25E811","#2320D8"))
  scale_x_continuous(breaks=c(0, 200, 400, 600),
                     labels=c("Unburned", "Low", "Moderate", "Moderate-High")) +
  ylim(0,2) +
  xlab("Burn Severity") +
  theme_minimal()
ggsave(cavity1_sev_rai, filename = "Figures/cavity1_sev.png")

cavity2_sev_rai <- all_nest_rai %>%
  filter(Class == "Secondary Cavity") %>%
  ggplot(aes(x = soil.severity.large, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#c5ac10") +
  scale_fill_discrete(name = "Nesting Group", type = c("#c5ac10")) + 
  #scale_color_manual(values=c("#ac835a","#25E811","#2320D8"))
  scale_x_continuous(breaks=c(0, 200, 400, 600),
                     labels=c("Unburned", "Low", "Moderate", "Moderate-High")) +
  ylim(0,2) +
  xlab("Burn Severity") +
  theme_minimal()
ggsave(cavity2_sev_rai, filename = "Figures/cavity2_sev.png")

ground_sev_rai <- all_nest_rai %>%
  filter(Class == "Ground") %>%
  ggplot(aes(x = soil.severity.large, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#8bbdf6") +
  scale_fill_discrete(name = "Nesting Group", type = c("#8bbdf6")) + 
  #scale_color_manual(values=c("#ac835a","#25E811","#2320D8"))
  scale_x_continuous(breaks=c(0, 200, 400, 600),
                     labels=c("Unburned", "Low", "Moderate", "Moderate-High")) +
  ylim(0,2) +
  xlab("Burn Severity") +
  theme_minimal()
ggsave(ground_sev_rai, filename = "Figures/ground_sev.png")

all_nest_sev <- (tree_sev_rai + shrub_sev_rai + cavity1_sev_rai)/
  (cavity2_sev_rai + ground_sev_rai + plot_spacer()) + plot_annotation('a')
ggsave(all_nest_sev, filename = "Figures/all nest sev.png")

tree_pyro_rai <- all_nest_rai %>%
  filter(Class == "Tree") %>%
  ggplot(aes(x = pyrodiversity.large.scaled, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#4ac541") +
  scale_fill_discrete(name = "Nesting Group", type = c("#4ac541")) + 
  ylim(0,3) +
  xlab("Pyrodiversity") +
  theme_minimal()
ggsave(tree_pyro_rai, filename = "Figures/tree pyro rai.png")

shrub_pyro_rai <- all_nest_rai %>%
  filter(Class == "Shrub") %>%
  ggplot(aes(x = pyrodiversity.large.scaled, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#ff834a") +
  scale_fill_discrete(name = "Nesting Group", type = c("#ff834a")) + 
  ylim(0,3) +
  xlab("Pyrodiversity") +
  theme_minimal()
ggsave(shrub_pyro_rai, filename = "Figures/shrub pyro rai.png")

cavity1_pyro_rai <- all_nest_rai %>%
  filter(Class == "Primary Cavity") %>%
  ggplot(aes(x = pyrodiversity.large.scaled, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#e6a4de" ) +
  scale_fill_discrete(name = "Nesting Group", type = c("#e6a4de" )) + 
  ylim(0,3) +
  xlab("Pyrodiversity") +
  theme_minimal()
ggsave(cavity1_pyro_rai, filename = "Figures/cavity1 pyro rai.png")

cavity2_pyro_rai <- all_nest_rai %>%
  filter(Class == "Secondary Cavity") %>%
  ggplot(aes(x = pyrodiversity.large.scaled, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#c5ac10" ) +
  scale_fill_discrete(name = "Nesting Group", type = c("#c5ac10")) + 
  ylim(0,3) +
  xlab("Pyrodiversity") +
  theme_minimal()
ggsave(cavity2_pyro_rai, filename = "Figures/cavity2 pyro rai.png")

ground_pyro_rai <- all_nest_rai %>%
  filter(Class == "Ground") %>%
  ggplot(aes(x = pyrodiversity.large.scaled, y = RAI, color = Class, fill = Class)) + 
  geom_smooth(color = "#8bbdf6" ) +
  scale_fill_discrete(name = "Nesting Group", type = c("#8bbdf6")) + 
  ylim(0,3) +
  xlab("Pyrodiversity") +
  theme_minimal()
ggsave(ground_pyro_rai, filename = "Figures/ground pyro rai.png")

all_nest_pyro <- (tree_pyro_rai + shrub_pyro_rai + cavity1_pyro_rai)/
  (cavity2_pyro_rai + ground_pyro_rai + plot_spacer()) + plot_annotation('a')

#Species RAI

allclassifications <- c(`Acorn Woodpecker` = NA_real_, `California Scrub-Jay` = NA_real_,
                        `Nuttall's Woodpecker` = NA_real_, `American Robin` = NA_real_,
                        `Northern Flicker` = NA_real_, `Rufous-crowned Sparrow` = NA_real_,
                        `Mourning Dove` = NA_real_, `California Towhee` = NA_real_,
                        `Violet-green Swallow` = NA_real_, `Ash-throated Flycatcher` = NA_real_,
                        `Western Bluebird` = NA_real_, `Lazuli Bunting` = NA_real_,
                        `Black-headed Grosebeak` = NA_real_, `White-breasted Nuthatch` = NA_real_,
                        Bushtit = NA_real_, `Brewer's Blackbird` = NA_real_, `Oak Titmouse` = NA_real_,
                        `Mountain Quail` = NA_real_, `Western Wood-Pewee` = NA_real_,
                        `Black Phoebe` = NA_real_, `Warbling Vireo` = NA_real_, `Pacific-slope Flycatcher` = NA_real_,
                        `Bullock's Oriole` = NA_real_, `California Quail` = NA_real_, `Bewick's Wren` = NA_real_,
                        `House Finch` = NA_real_, `Pileated Woodpecker` = NA_real_,
                        `Hutton's Vireo` = NA_real_, Wrentit = NA_real_, `Orange-crowned Warbler` = NA_real_,
                        `Western Meadowlark` = NA_real_, `Hermit Thrush` = NA_real_, `Steller's Jay` = NA_real_,
                        `Lesser Goldfinch` = NA_real_, `Blue-gray Gnatcatcher` = NA_real_,
                        `Golden-crowned Sparrow` = NA_real_, `Dark-eyed Junco` = NA_real_,
                        `Rock Wren` = NA_real_, `Anna's Hummingbird` = NA_real_, `Spotted Towhee` = NA_real_,
                        `Olive-sided Flycatcher` = NA_real_, `Western Kingbird` = NA_real_) 

rai.calculate <- function(record.table, start.date, end.date, camop, timeperiod.name) {
  # define names to use in file names (just removes the dashes)
  start.name <- gsub("-", "", start.date)
  end.name <- gsub("-", "", end.date)
  
  # calculate how long the camera was functioning in that time period
  
  # selects columns within specified dates
  camop.subset <- dplyr::select(camop, Camera, start.date:end.date) 
  
  # sum rows within specified dates (there are 1s when camera was operating, NA when not)
  camop.subset$Operation <- rowSums(dplyr::select(camop.subset, start.date:end.date), na.rm=TRUE) 
  
  # get rid of the individual day columns, just select Camera, Operation
  camop.subset <- dplyr::select(camop.subset, Camera, Operation) 
  
  # format start and end dates as dates
  start.date <- as.Date(start.date)
  end.date <- as.Date(end.date)
  # subset record table to date of interest
  record.table.subset <- record.table[record.table$Date >= start.date & record.table$Date <= end.date,]
  # calculate number of observations of each classification type at each camera
  records <- record.table.subset %>%
    dplyr::group_by(Species, Camera) %>%
    dplyr::summarise(Detections = n()) %>%     # counts number of observations of each species
    spread(key = Species, value = Detections)  # gets from long to wide format  
  
  # add columns for classes not present
  records <- add_column(records, !!!allclassifications[!names(allclassifications) %in% names(records)])
  
  # gather data so each class-camera is its own row again
  records <- records %>% gather(2:ncol(records), key = "Class", value = "Count")
  
  # replace NA with 0 
  records[is.na(records)] <- 0
  
  # join camera operation dates and observations
  RAI.table <- plyr::join(records, camop.subset)
  
  # calculate RAI
  RAI.table$RAI <- RAI.table$Count / RAI.table$Operation
  
  # add new column for time period
  RAI.table$TimePeriod <- timeperiod.name
  
  # write csv
  write.csv(RAI.table, file = paste(here::here('data'), "/RAI_", timeperiod.name, "_", start.name, "_", end.name, ".csv", collapse = "", sep = ""),row.names=F)
  
  return(RAI.table)
  
}


rai_2020 <- rai.calculate(record.table = data_viz2, start.date = "2020-04-23", end.date = "2020-07-31", camop = cam_op_rai, timeperiod.name = "2020")
rai_2021 <- rai.calculate(record.table = data_viz, start.date = "2021-03-01", end.date = "2021-07-31", camop = cam_op_rai, timeperiod.name = "2021")
rai_2022 <- rai.calculate(record.table = data_viz, start.date = "2022-03-01", end.date = "2022-07-08", camop = cam_op_rai, timeperiod.name = "2022")


all_rai <- rbind(rai_2020, rai_2021, rai_2022)


all_rai$TimePeriod <- fct_relevel(all_rai$TimePeriod, "2020", "2021", "2022")

all_rai <- all_rai %>%
  dplyr::rename(Site = Camera)

all_rai <- left_join(all_rai, metadata, by = "Site")

all_rai %>%
  filter(Class == "Acorn Woodpecker") %>%
  ggplot(aes(x = TimePeriod, y = RAI, fill = fire)) + geom_boxplot()


all_rai %>%
  filter(Class == "Western Bluebird") %>%
  ggplot(aes(x = soil.severity.small, y = RAI)) + geom_smooth()
