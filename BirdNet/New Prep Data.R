library(tidyverse)
library(plyr)
library(stringr)
library(lubridate)
library(camtrapR)
library(patchwork)
`%notin%` <- Negate(`%in%`)

#WRITE THE READ ME (REMINDER)

##Create Continuous and non-detection record tables 

setwd("/Users/Kendall/Desktop/AudioMoth_fire_occu/data")
species_list <- read_tsv("species_list.txt", col_names = FALSE) %>%
  mutate(Species = sub(".*_", "", X1),
         scientific_name = sub("_.*", "", X1) )


# # #202X Record Table
# setwd("new record tables/2022")
# filelist = list.files(pattern = ".*.csv")
# #
# data_raw <- filelist %>%
#   map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>% mutate(filename=gsub(".csv","",basename(x))))
# 
# data_raw$Site = substr(data_raw$filename, 1,3)
# data_raw$Sunrise = substr(data_raw$filename, 5,7)
# data_raw$Sunrise <- recode(data_raw$Sunrise, Aft = "After", Bef = "Before", Dur = "During")
# 
# data_raw$Date <-substr(data_raw$filename,9,16)
# 
# #
# fun_insert <- function(x, pos, insert) {       # Create own function
#   gsub(paste0("^(.{", pos, "})(.*)$"),
#        paste0("\\1", insert, "\\2"),
#        x)
# }
# 
# data_raw$Date2 <- fun_insert(data_raw$Date, pos = 4, insert = "-")
# data_raw$Date <- fun_insert(data_raw$Date2, pos = 7, insert = "-")
# data_raw$Date <- ymd(data_raw$Date)
# data_raw$confidence <- as.numeric(data_raw$confidence)
# data_raw <- data_raw %>%
#   dplyr::rename(Species = "common_name")
# 
# data_raw <- data_raw %>%
#   dplyr::select(Species, scientific_name, filepath, start, end, confidence, filename, Site, Sunrise, Date)
# 
# data_raw <- data_raw %>%
#   mutate(detectionID = paste0(Site,sep = "_", Date, sep = "_", Sunrise, sep = "_", start, sep = "_", end))
# 
# write_csv(data_raw, file = "/Users/Kendall/Desktop/AudioMoth_fire_occu/data/Record Table 2022.csv")
# 
# #setwd("/Users/Kendall/Desktop/AudioMoth_fire_occu")
# # data_raw <- read_csv("data/Record Table 2020.csv")
# #
# temp <- data_raw %>%
#   dplyr::group_by(.,detectionID)
# 
# detectionlist <- group_split(temp)
# 
# datalist = list ()
# 
# for(i in 1:length(detectionlist)){
# 
#  species_list %>%
#     filter(Species %notin% detectionlist[[i]]$Species) %>%
#     mutate(filepath = unique(detectionlist[[i]]$filepath),
#            start = unique(detectionlist[[i]]$start),
#            end = unique(detectionlist[[i]]$end),
#            confidence = 0,
#            filename = unique(detectionlist[[i]]$filename),
#            Site = unique(detectionlist[[i]]$Site),
#            Sunrise = unique(detectionlist[[i]]$Sunrise),
#            Date = unique(detectionlist[[i]]$Date),
#            detectionID = paste0(Site,sep = "_", Date, sep = "_", Sunrise, sep = "_", start, sep = "_", end)
#            ) %>%
#     dplyr::select(-X1) -> datalist[[i]]
# 
# }
# data <- dplyr::bind_rows(datalist)
# #data = do.call(rbind, datalist)
# 
# data <- rbind(data, data_raw)
# 
# write_csv(data, file = "/Users/Kendall/Desktop/AudioMoth_fire_occu/data/Record Table 2022 and non-detections.csv")

setwd("/Users/Kendall/Desktop/AudioMoth_fire_occu")
data2020 <- read_csv("data/Record Table 2020.csv")
data2021 <- read_csv("data/Record Table 2021.csv")
data2022 <- read_csv("data/Record Table 2022.csv")

data <- rbind(data2020, data2021, data2022) 
# data <- data %>%
#   filter(confidence >= 0.95)
###checking for repeats within the same detection

# data_raw <- data_raw %>%
#   mutate(detectionID = paste0(Site,sep = "_", Date, sep = "_", Sunrise, sep = "_", start, sep = "_", end))
#~64000 detection event in 2020!

#Validated Recordings 

test <- data %>%
  filter(confidence >= 0.95)

setwd("data/species validation/Original")
filelist = list.files(pattern = ".*.csv")
#
validation <- filelist %>%
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>% mutate(filename=gsub(".csv","",basename(x))))# %>%
# filter(`Accepted?` == "Y")

validation <- validation %>%
  mutate(Date = mdy(Date))

setwd("/Users/Kendall/Desktop/AudioMoth_fire_occu/data/species validation/New")
filelist = list.files(pattern = ".*.csv")
#
validation2 <- filelist %>%
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>% mutate(filename=gsub(".csv","",basename(x))))# %>%
# filter(`Accepted?` == "Y")

validation2 <- validation2 %>%
  mutate(Date = ymd(Date))

validation <- validation[,-12]

validation <- rbind(validation, validation2)

validation <- validation %>%
  mutate(Valid_ID = paste0(Site, sep = "_",
                           Date, sep = "_",
                           Sunrise))


validation$`Accepted?`[is.na(validation$`Accepted?`)] <- "N"

for(i in 1:nrow(validation)){
  if(validation$`Accepted?`[i] != "Y") {
    validation$`Accepted?`[i] <- "N"
  }
}

validation <- validation %>%
  filter(Species %in% species_list$Species)

data <- data %>%
  mutate(Valid_ID = paste0(Site, sep = "_",
                           Date, sep = "_",
                           Sunrise, sep = "_",
                           Species))

pos_validation <- validation %>%
  filter(`Accepted?` == "Y")

# valid_IDs <- data$Valid_ID
# 
# validation <- validation %>%
#   filter(Valid_ID %in% valid_IDs) 
# 
# validation <- validation %>%
#   select(Site, Species, Date, Valid_ID)
# 
# test <- left_join(validation, data)
# test2 <- distinct(test, across(Valid_ID),.keep_all = TRUE)

