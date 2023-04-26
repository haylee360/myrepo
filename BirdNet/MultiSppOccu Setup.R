#Multi-species Occupancy Setup Code
library(tidyverse)
library(abind)
library(lubridate)
library(plyr)
library(camtrapR)


# Occupancy covariate columns
psi_covs <- c("Intercept", "elevation",
              "severity", "severity2",
              #"chap", "grass", "wood",
              "pyrodiversity",
              #"BurnLag2", "BurnLag3", "BurnLag4",
              #"distwater",
              "canopy",
              #"lag",# "severity_x_lag"
              #"BurnLag2_x_severity", "BurnLag3_x_severity", "BurnLag4_x_severity",
              #"BurnLag2_x_pyrodiversity", "BurnLag3_x_pyrodiversity", "BurnLag4_x_pyrodiversity"
              #"pyrodiversity_x_lag",
              "LagTime", "LagTime_x_severity"
              #  "LagTime_x_pyrodiversity"
)

# Detection covariate columns
p_covs <- c("Intercept", "jd", "jdsq", "wind", "temp", "effort", "canopy"
)


# Metadata file
metadata <- read_csv("data/audiomoth_metadata.csv")

metadata$Lag <- NA

for(i in 1:nrow(metadata)){
  if(metadata$Year[i] == "2020"){
    metadata$Lag[i] <- 1
  }
  
  if(metadata$Year[i] == "2021"){
    metadata$Lag[i] <- 2
  }
  
  if(metadata$Year[i] == "2022"){
    metadata$Lag[i] <- 3
  }
}

cor <- metadata %>%
  dplyr::select(Elevation, Canopy, wood, grass, chap, Pyrodiversity, Severity)

cor(cor)
# Fn for getting observations in
file_to_mtx <- function(filename) {
  y_raw <- read_csv(filename)
  
  site_IDs <- y_raw$...1 #double check that this row is working correctly 
  dates <- lapply(
    colnames(y_raw)[-1],
    function(x) {
      strsplit(x, split = "_")[[1]][1]
    }) %>%
    unlist()
  
  y <- as.data.frame(t(y_raw[,-1]))
  
  rownames(y) <- NULL
  colnames(y) <- site_IDs
  y$year <- year_vec <- year(dates)
  y$month <- month_vec <- month(dates)
  y$date <- date_vec <- yday(dates)
  
  y_mtx <- y %>%
    dplyr::select(-month, -year, -date) %>%
    t()
  y_mtx <- y_mtx[, (colSums(is.na(y_mtx)) < nrow(y_mtx))] 
  
  nobs <- unlist(apply(y_mtx, 1, function(x) sum(!is.na(x))))
  
  y_rtn <- matrix(NA, nrow = nrow(y_mtx), ncol = max(nobs))
  
  for (i in 1:nrow(y_rtn)) {
    if (nobs[i] > 0) {
      y_rtn[i, 1:nobs[i]] <- y_mtx[i, !is.na(y_mtx[i, ])]
    }
  }
  rownames(y_rtn) <- rownames(y_mtx)
  return(y_rtn)
}

# Read in detection histories
detection_files <- list.files("detection histories/conf_95", pattern = "*.csv",
                              full.names = TRUE)

detection_histories <- lapply(detection_files, file_to_mtx)
site_vec <- substr(rownames(detection_histories[[1]]), 1, 3)

species <- c("Acorn Woodpecker", "American Robin", "Ash-throated Flycatcher",
             "Bewick's Wren", "Black-headed Grosebeak", "Bullock's Oriole",
             "California Quail", "California Scrub-Jay", "California Towhee",
             "Lazuli Bunting", "Mountain Quail", "Mourning Dove", "Nuttal's Woodpecker",
             "Oak Titmouse", "Orange-crowned Warbler", "Rufous-crowned Sparrow", "Spotted Towhee",
             "Steller's Jay", "Violet-green Swallow", "Western Bluebird",
             "White-breasted Nuthatch")

detection_array <- abind(detection_histories, along = 3)

Dates <- c(1:dim(detection_array)[2])
dimnames(detection_array)[[2]] <- Dates
dimnames(detection_array)[[3]] <- species

Site_Covs <- data.frame(
  Intercept = 1,
  severity = scale(metadata$Severity)[,1],
  elevation = scale(metadata$Elevation)[,1],
  canopy = scale(metadata$Canopy)[,1],
  wood = scale(metadata$wood)[,1],
  chap = scale(metadata$chap)[,1],
  grass = scale(metadata$grass)[,1],
  # Woodland = metadata$Woodland,
  # Grassland = metadata$Grassland,
  # Chaparral = metadata$Chaparral,
  severity2 = scale(metadata$Severity^2)[,1],
  pyrodiversity = scale(metadata$Pyrodiversity)[,1],
  distwater = scale(metadata$DistWater)[,1],
  lag = scale(metadata$Lag)[,1],
  LagTime = scale(metadata$LagTime)
) %>%
  mutate(severity_x_lag = severity * lag,
         pyrodiversity_x_lag = pyrodiversity * lag,
         LagTime_x_severity = LagTime * severity)


Obs_Covs <- data.frame(
  Intercept = 1,
  canopy = metadata$Canopy,
  effort = metadata$Effort
)

# Drop sites with 0 observations
# sites_to_drop <- which(unlist(apply(detection_array[,,1], 1, function(x) sum(!is.na(x)))) < 1)
# metadata <-        metadata[-sites_to_drop,]
# Site_Covs <-       Site_Covs[-sites_to_drop,]
# Obs_Covs <-         Obs_Covs[-sites_to_drop]
# detection_array <- detection_array[-sites_to_drop,,]
#camera_vec <- camera_vec[-sites_to_drop]

site_info <- read.csv("data/Operation Logs/audiomoth_oplog_phase123AprJul.csv")

site_info$Start <- mdy(site_info$Start)
site_info$End <- mdy(site_info$End)

site_info$Site <- as.character(site_info$Site)
site_info$Start <- as.character(site_info$Start)
site_info$End <- as.character(site_info$End)

jd_mtx <- detection_array[,,1]
dimnames(jd_mtx)[[2]] <- Dates 
jd_mtx <- as.data.frame(jd_mtx)

for(i in 1:nrow(jd_mtx)){
  jd_mtx[i,] <- yday(site_info$Start[i]):as.numeric(yday(site_info$Start[i])+61)
}
jd_mtx <- as.matrix(jd_mtx)

jd_mean <- mean(jd_mtx)
jd_sd <- sd(jd_mtx)

p_cov_array <- array(dim = c(nrow(jd_mtx), ncol(jd_mtx), 7))

p_cov_array[,,1] <- matrix(1, nrow = nrow(jd_mtx), ncol = ncol(jd_mtx))
p_cov_array[,,2] <- (jd_mtx - jd_mean) / jd_sd
p_cov_array[,,3] <- p_cov_array[,,2]^2
p_cov_array[,,4] <- scale(metadata$Effort)
p_cov_array[,,5] <- scale(metadata$Canopy)

wind <- read_csv("env_data/am_wind_log.csv")
temp <- read_csv("env_data/am_temp_log.csv")

site_info <- read.csv("data/Operation Logs/audiomoth_oplog_phase123.csv")

site_info$Start <- mdy(site_info$Start)
site_info$End <- mdy(site_info$End)

date_mtx <- detection_array[,,1]
dimnames(date_mtx)[[2]] <- Dates 
date_mtx <- as.data.frame(date_mtx)

for(i in 1:nrow(date_mtx)){
  date_mtx[i,] <- as.character(seq(site_info$Start[i], site_info$Start[i] + days(61), by = "1 day"))
  #define row of dates for comparison 
  #
}

wind_mtx <- detection_array[,,1]
dimnames(wind_mtx)[[2]] <- Dates 
wind_mtx <- as.data.frame(wind_mtx)

for(i in 1:nrow(wind_mtx)) {
  for(j in 1:ncol(wind_mtx)){
    
    date <- date_mtx[i,j]
    
    w <- wind %>%
      filter(Date == date)
    
    if(nrow(w) >= 1){
      
      wind_mtx[i,j] <- w$Wind_Avg
    }
    
    else{
      wind_mtx[i,j] <- NA
    }
  }
}

temp_mtx <- detection_array[,,1]
dimnames(temp_mtx)[[2]] <- Dates 
temp_mtx <- as.data.frame(temp_mtx)

for(i in 1:nrow(temp_mtx)) {
  for(j in 1:ncol(temp_mtx)){
    
    date <- date_mtx[i,j]
    
    t <- temp %>%
      filter(Date == date)
    
    if(nrow(t) >= 1){
      
      temp_mtx[i,j] <- t$Temp_Avg
    }
    
    else{
      temp_mtx[i,j] <- NA
    }
  }
}

wind_mtx <- as.matrix(wind_mtx)
wind_mean <- mean(wind_mtx, na.rm = TRUE)
wind_sd <- sd(wind_mtx, na.rm = TRUE)

temp_mtx <- as.matrix(temp_mtx)
temp_mean <- mean(temp_mtx, na.rm = TRUE)
temp_sd <- sd(temp_mtx, na.rm = TRUE)

p_cov_array[,,6] <- (wind_mtx - wind_mean) / wind_sd
p_cov_array[,,7] <- (temp_mtx - temp_mean) / temp_sd

num.reps.persite = unlist(apply(detection_array[,,1], 1, function(x) sum(!is.na(x))))

dimnames(p_cov_array) <- list(
  metadata$Site_ID,
  1:62,
  c("Intercept", "jd", "jdsq", "effort", "canopy", "wind", "temp"
  )
)



stopifnot(all(p_covs %in% dimnames(p_cov_array)[[3]]))

model.data <- list(#Z = zs,
  X = detection_array,
  psi_cov_dat = Site_Covs[, psi_covs],
  p_cov_dat = p_cov_array[,, p_covs]
  
  
)

n.zeroes <- 0 #with data augmentation

constants <-  list(
  num.species = length(species),
  aug.species = length(species) + n.zeroes,
  num.sites = length(unique(metadata$Site_ID)),
  num.reps.persite = unlist(apply(detection_array[,,1], 1, function(x) sum(!is.na(x)))),
  num.reps = max(unlist(apply(detection_array[,,1], 1, function(x) sum(!is.na(x))))),
  num.camera = length(unique(metadata$Site)),
  camID = as.numeric(as.factor(site_vec)),
  yearID = as.numeric(as.factor(metadata$Year)),
  n_p_covs = length(p_covs),
  n_psi_covs = length(psi_covs)
  # pBurnFacStart = which(p_covs == "facBurnProx0"), 
  #  psiBurnFacStart = which(psi_covs == "facBurnProx0")#,
)


num.species <- constants$num.species
aug.species <- constants$aug.species
num.site <- constants$num.sites
num.reps <- constants$num.reps
num.camera <- constants$num.camera



inits <- list(omega.draw = runif(1, num.species/(num.species + n.zeroes), 1),
              w = c(rep(1, num.species),
                    rbinom(n.zeroes, size = 1, prob = runif(1, 0, 0.5))),
              a = matrix(rnorm((num.species + n.zeroes) * length(psi_covs)),
                         nrow = num.species + n.zeroes, ncol = length(psi_covs)),
              b = matrix(rnorm((num.species + n.zeroes) * length(p_covs)),
                         nrow = num.species + n.zeroes, ncol = length(p_covs)),
              mu.a = rnorm(length(psi_covs)),
              mu.b = rnorm(length(p_covs)),
              sigma.a = rep(1, length(psi_covs)),
              sigma.b = rep(1, length(p_covs)),
              sd_ranef_site = rep(1, num.species + n.zeroes),
              ranef_site = matrix(rnorm((num.species + n.zeroes) * num.camera),
                                  nrow = num.species + n.zeroes, ncol = num.camera),
              sd_ranef_year = rep(1, num.species + n.zeroes),
              ranef_site = matrix(rnorm((num.species + n.zeroes) * num.camera),
                                  nrow = num.species + n.zeroes, ncol = num.camera))


# target_canopy <- rep(seq(min(Site_Covs$canopy), max(Site_Covs$canopy), length.out = 11), 4) #increase for final output?
# target_severity <- rep(seq(min(Site_Covs$severity), max(Site_Covs$severity), length = 11), 4)
# 
# derived_psi_dat <- data.frame(
#   Intercept = 1,
#   ruggedness = mean(Site_Covs$ruggedness),
#   elevation = mean(Site_Covs$elevation),
#   edge = mean(Site_Covs$edge),
#   edge2 = mean(Site_Covs$edge2),
#   canopy = target_canopy,
#   wood = mean(Site_Covs$wood),
#   grass = mean(Site_Covs$grass),
#   chap = mean(Site_Covs$chap),
#   severity = target_severity,
#   Woodland = 1,
#   Grassland = 0,
#   Chaparral = 0,
#   facBurnProx0 = rep(c(0, 1, 0, 0), each = length(target_canopy) / 4),
#   facBurnProx = rep(c(0, 1, 0, 0), each = length(target_canopy) / 4),
#   facBurnLag1 = rep(c(0, 0, 1, 0), each = length(target_canopy) / 4),
#   facBurnLag2 = rep(c(0, 0, 0, 1), each = length(target_canopy) / 4),
#   facBurnLag = rep(c(0, 0, 1, 1), each = length(target_canopy) / 4),
#   Unburned_Prox = 0, facUnburnedLag = 0,
#   facBurn = rep(c(0, 1, 1, 1), each = length(target_canopy) / 4),
#   burnType = rep(c("Unburned", "Post-burn", "Post-burn lag1", "Post-burn lag2"),
#                  each = length(target_canopy) / 4)
#   # canopyType = rep(c("Low", "High"), 4)
# ) %>%
#   dplyr::mutate(
#     facBurnProx0_x_canopy = facBurnProx0 * canopy,
#     facBurnLag1_x_canopy = facBurnLag1 * canopy,
#     facBurnLag2_x_canopy = facBurnLag2 * canopy,
#     facBurnProx_x_canopy = facBurnProx * canopy,
#     facBurnLag_x_canopy = facBurnLag * canopy,
#     facBurn_x_canopy = facBurn * canopy,
#     Unburned_Prox_x_canopy = Unburned_Prox * canopy,
#     facUnburnedLag_x_canopy = facUnburnedLag * canopy,
#     canopy_x_edge2 = canopy * edge2,
#     id = row_number(),
#     facBurnProx0_x_severity = facBurnProx0 * severity,
#     facBurnLag1_x_severity = facBurnLag1 * severity,
#     facBurnLag2_x_severity = facBurnLag2 * severity
#   )


#viewshed_scaled <- scale(metadata$Detection_Distance, center = TRUE)[,1]

#model.data <- c(model.data, list(derived_psi_dat = derived_psi_dat[, psi_covs])
#               )

#constants <- c(constants, list(ndpsi = nrow(derived_psi_dat))
#            )


## for the data augmentation case

if(n.zeroes >= 0){
  #zeroAugment <- function(histories, n.zeroes){
  # X.aug is the augmented version of X.  The first n species were
  # actually observed and the n+1 through n.zeroes species are all
  # zero encounter histories create an empty 3D array with n + n.zero
  # species
  X <- model.data$X
  #X.zero <- histories$X.zero
  X.dim <- dim(X)
  X.dim[3] <- X.dim[3] + n.zeroes
  X.aug <- array( NA, dim = X.dim)
  
  # addMissingData <- function(histories, survey.dates){
  #   ## 'Add' missing data:set X and X.zero for the unsurveyed
  #   ## repetitions to NA
  #   X <- histories$X
  #   X.zero <- histories$X.zero
  #   for(point in 1:length(unique(survey.data$point))){
  #     point.index <- which(survey.dates$point == row.names(X)[point])
  #     missing <- is.na(survey.dates[point.index,][,-(1:1)])
  #     X[point, missing,] <- NA
  #     X.zero[point, missing] <- NA
  #   }
  #   return(list(X=X,X.zero=X.zero))
  # }
  
  ## fill in the array with the occurrence data
  X.aug[,,1:dim(X)[3]] <-  X
  
  ## fill the zero histories
  X.aug[,,-(1:dim(X)[3])] <- rep(0, n.zeroes)
  # return(X.aug)
  #}
  
}



## for the non-data augmented case
if(n.zeroes == 0){
  inits[c("w", "omega")] <- NULL
  constants[c("n.zeroes")] <- NULL 
}




get_summary_from_samples <- function(samples, ni, nb, nt, nc, hpd_prob) {
  b <- (ni - nb) %% nt
  samples_list <- list()
  
  drop_cols <- unlist(apply(do.call(rbind, samples), 2, function(x) length(unique(x)) <= 2))
  
  if (nc > 1) {
    for (i in 1:nc) {
      samples_list[[i]] <- mcmc(data = samples[[i]][, !drop_cols], start = nb + 1, end = ni - b, thin = nt)
    }
  } else {
    samples_list[[1]] <- mcmc(data = samples[, !drop_cols], start = nb + 1, end = ni - b, thin = nt)
  }
  
  samples2_coda <- mcmc.list(samples_list)
  
  
  
  summary <- MCMCsummary(samples2_coda, HPD = TRUE, hpd_prob = hpd_prob)
  summary$param <- rownames(summary); rownames(summary) <- NULL
  
  summary
}



get_parname_from_par <- function(param, psi_covs, p_covs, ssom = TRUE) {
  layer <- ifelse(
    grepl("b\\[", param), "p", 
    ifelse(grepl("a\\[", param), "psi", NA)
  )
  parname_vec <- ifelse(!is.na(layer), NA, param)
  
  if (!ssom) {
    parnum_vec <- strsplit(param, split = ",") %>%
      lapply(function(x) x[length(x)]) %>%
      unlist() %>%
      gsub(pattern = "[^0-9]", replacement = "") %>%
      as.numeric()
    
    for (i in 1:length(param)) {
      if (!is.na(layer[i])) {
        if (layer[i] == "psi") {
          parname_vec[i] <- psi_covs[parnum_vec[i]]
        } else if (layer[i] == "p") {
          parname_vec[i] <- p_covs[parnum_vec[i]]
        }
      }
    }
    
    
  } else {
    parnum_vec <- ifelse(!is.na(layer), -1, NA)
    for (i in 1:length(param)) {
      if (!is.na(layer[i])) {
        parnum_vec[i] <- as.numeric(gsub(param[i],
                                         pattern = "[^0-9.-]", replacement = ""))
        
        if (layer[i] == "psi") {
          parname_vec[i] <- psi_covs[parnum_vec[i]]
        } else if (layer[i] == "p") {
          parname_vec[i] <- p_covs[parnum_vec[i]]
        }
      }
    }
    
  }
  list(parname = parname_vec,
       layer = layer)
}


get_spec_from_par <- function(param, species) {
  if (!is.character(param)) stop("Provide a character string or vector.")
  specnum <- strsplit(param, split = ",") %>%
    lapply(function(x) x[1]) %>%
    unlist() %>%
    gsub(pattern = "[^0-9.-]", replacement = "") %>%
    as.numeric()
  
  specnum[grepl("mu", param) |
            grepl("sigma", param) |
            grepl("sd_ranef", param)] <- NA
  
  species[specnum]
}



get_psiID_from_par <- function(param) {
  ifelse(grepl("^psi\\[", param),
         (strsplit(param, split = ",") %>%
            lapply(function(x) x[length(x)]) %>%
            unlist() %>%
            gsub(pattern = "[^0-9]", replacement = "") %>%
            as.numeric()),
         ifelse(grepl("^rich\\[", param),
                as.numeric(gsub(x = param, pattern = "[^0-9]", replacement = "")),
                ifelse(grepl("^group_psi\\[", param),
                       (strsplit(param, split = ",") %>%
                          lapply(function(x) x[1]) %>%
                          unlist() %>%
                          gsub(pattern = "[^0-9]", replacement = "") %>%
                          as.numeric()),
                       NA))
  )
}


get_group_from_par <- function(param, groups) {
  if (!is.character(param)) stop("Provide a character string or vector.")
  grpnum <- strsplit(param, split = ",") %>%
    lapply(function(x) x[length(x)]) %>%
    unlist() %>%
    gsub(pattern = "[^0-9.-]", replacement = "") %>%
    as.numeric()
  
  rtn <- groups[grpnum]
  rtn[!grepl("group", param)] <- NA
  rtn
}

