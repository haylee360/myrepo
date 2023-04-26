library(tidyverse)
library(lubridate)
setwd("/Users/Kendall/Desktop/AudioMoth_fire_occu")

data2020 <- read_csv("data/Record Table 2020.csv")
data2021 <- read_csv("data/Record Table 2021.csv")
data2022 <- read_csv("data/Record Table 2022.csv")

data <- rbind(data2020, data2021, data2022) 

data <- data %>%
  mutate(Year = year(Date),
         Site_ID = paste0(Site, sep = "_", Year))

#Filter species of interest

group_info <- read_csv("data/group info.csv") %>%
  filter(`Include?` == "Y")

species_of_interest <- group_info$Species

data <- data %>%
  filter(data$Species %in% species_of_interest)

data <- data %>%
  mutate(Month = month(Date))

data <- data %>%
  filter(Month > 3 & Month <= 7)

data <- data %>%
  filter(confidence >= 0.50)

detection_list <- as.data.frame(matrix(nrow = length(species_of_interest),
                                       ncol = 3))

colnames(detection_list) <- c("Species", "Count", "Unique")
detection_list$Species <- species_of_interest

for(i in 1:length(species_of_interest)) {
  temp <- data %>%
    filter(Species == species_of_interest[i])
  
  count <- nrow(temp)
  
  detection_list$Count[i] <- count
  
  unique <- length(unique(temp$Site_ID))
  
  detection_list$Unique[i] <- unique
}

detection_count <- detection_list %>%
  filter(Count >= 50)

detection_count_IDs <- detection_count$Species  

data <- data %>%
  filter(Species %in% detection_count_IDs)
#Create csv of randomly sampled 40 detections for each species 

detections.df <- vector('list')

for(i in 1:length(detection_count_IDs)){
  
  temp <- data %>%
    filter(Species == detection_count_IDs[i])
  
  temp <- slice_sample(temp, n = 100, replace = FALSE)
  
  
  temp <- temp |>  
    mutate(sec = start %% 60,
           min = (start / 60) |> floor(),
           minsec = paste0(min, sep = " ", "M", sep = " ", sec, sep = " ", "S"),
           Timestamp = ms(minsec)) |> 
    select(-sec, -min)
  
  
  detections.df[[i]] <- temp
}



for(i in 1:length(detection_count_IDs)){
  
  temp <- detections.df[[i]]
  
  
  write.csv(x = temp, file = paste0("/Users/Kendall/Desktop/AudioMoth_fire_occu/New Validation/",
                                    detection_count_IDs[i],
                                    sep = "_", "validata.csv"),
            row.names = TRUE)
  
}

species <- c("Acorn Woodpecker", "American Robin", "Ash-throated Flycatcher",
             "Bewick's Wren", "Black-headed Grosbeak", "Bullock's Oriole",
             "California Quail", "California Scrub-Jay", "California Towhee",
             "Lazuli Bunting", "Mountain Quail", "Mourning Dove", "Nuttall's Woodpecker",
             "Oak Titmouse", "Orange-crowned Warbler", "Rufous-crowned Sparrow", "Spotted Towhee",
             "Steller's Jay", "Violet-green Swallow", "Western Bluebird",
             "White-breasted Nuthatch") #species fairly confident about

data <- data %>%
  filter(Species %in% species)

data %>%
  filter(confidence >= .50) %>%
  ggplot(aes(x = confidence)) + 
  geom_histogram(stat = "bin") +
  facet_wrap(~Species)