#Stacked Multispecies Occupancy Model

library(tidyverse)
library(nimble)
library(mcmcplots)
library(MCMCvis)
library(nimbleEcology)

source("MultiSppOccu Setup.R")

start_time <- Sys.time()

# MCMC params
ni <- 30000
nb <- 2000
nc <- 4
nt <- 1
do_ppc <- FALSE
use_WAIC <- FALSE

ms.occ <- nimbleCode({
  
  
  for (n in 1:n_psi_covs) {
    mu.a[n] ~ dnorm(0, sd = 2.25)
    sigma.a[n] ~ T(dt(0, sigma = 2.25, df = 1), 0, Inf)
  }
  
  for (n in 1:n_p_covs) {
    mu.b[n] ~ dnorm(0, sd = 2.25)
    sigma.b[n] ~ T(dt(0, sigma = 2.25, df = 1), 0, Inf)
  }
  
  
  ## species-specific  parameters
  for(i in 1:num.species) {   
    #Random Effects
    sd_ranef_site[i] ~ T(dt(0, sigma = 2.25, df = 1), 0, Inf)
    #sd_ranef_year[i] ~ T(dt(0, sigma = 2.25, df = 1), 0, Inf)
    
    for (c in 1:num.camera) {
      ranef_site[i, c] ~ dnorm(0, sd = sd_ranef_site[i])
    }
    # for (c in 1:num.year) {
    #   ranef_year[i, c] ~ dnorm(0, sd = sd_ranef_year[i])
    # }
    
    
    for (n in 1:n_psi_covs) {
      a[i, n] ~ dnorm(mu.a[n], sd = sigma.a[n])
    }
    for (n in 1:n_p_covs) {
      b[i, n] ~ dnorm(mu.b[n], sd = sigma.b[n])
    }
    
    for (j in 1:num.sites) {
      logit(psi[i,j]) <-
        inprod(psi_cov_dat[j, 1:n_psi_covs], a[i, 1:n_psi_covs]) +
        ranef_site[i, camID[j]] #+ ranef_year[i, yearID[j]]
      
      logit(p[j,1:num.reps.persite[j],i]) <-
        (p_cov_dat[j, 1:num.reps.persite[j], 1:n_p_covs] %*% b[i, 1:n_p_covs]) +
        ranef_site[i, camID[j]] #+ ranef_year[i, yearID[j]]
      
      X[j,1:num.reps.persite[j],i] ~
        dOcc_v(probOcc = psi[i, j],
               probDetect = p[j, 1:num.reps.persite[j], i],
               len = num.reps.persite[j])
      
    }
    
    # for (j in 1:ndpsi) {
    #   logit(dpsi[i,j]) <-
    #     inprod(derived_psi_dat[j, 1:n_psi_covs], a[i, 1:n_psi_covs])
    #   
    # }
    
    # for (j in 1:ndpsi) {
    #   logit(dpsi2[i,j]) <-
    #     inprod(derived_psi_dat2[j, 1:n_psi_covs], a[i, 1:n_psi_covs])
    #   
    # }
    
    # for (j in 1:ndpsi) {
    #   logit(dp[i,j]) <-
    #     inprod(derived_p_dat[j, 1:n_p_covs], b[i, 1:n_p_covs])
    # 
    # }
  }
  
  # Derived quantities
  for (j in 1:num.site) {
    rich[j] <- sum(psi[1:num.species, j]) # Estimated richness at each site
    
    # for (g in 1:ngroup) {
    #   group_psi[j, g] <- inprod(psi[1:num.species, j], group_dat[g, 1:num.species]) / sum(group_dat[g, 1:num.species])
    # }
  }
  
  
  # for (j in 1:ndpsi) {
  #   drich[j] <- sum(dpsi[1:num.species, j]) # Estimated richness at each site
  #   # for (g in 1:ngroup) {
  #   #   group_dpsi[j, g] <- inprod(dpsi[1:num.species, j], group_dat[g, 1:num.species]) / sum(group_dat[g, 1:num.species])
  #   # }
  #   
  #   # for  (i in 1:num.species) {
  #   #   relative.alpha[i, j] <- dpsi[i, j] / sum(dpsi[1:num.species, j])
  #   #   hill1.input.spec[i, j] <- relative.alpha[i, j] * log(relative.alpha[i, j])
  #   #   hill2.input.spec[i, j] <- relative.alpha[i, j] * relative.alpha[i, j]
  #   # }
  #   # hill1[j] <- exp(-1 * sum(hill1.input.spec[1:num.species, j]))
  #   # hill2[j] <- 1/sum(hill2.input.spec[1:num.species, j])
  # }
}) ## close model
# 
# taxon_grp_info <- read_csv("data/species_groups.csv") %>%
#   filter(Common_Name != "Mountain_Lion" & Common_Name != "Ground_Squirrel" & Common_Name != "Pig")
# taxon_grp_info$grp_as_num <- as.numeric(as.factor(taxon_grp_info$Size_Group))
# 
# constants$ngroup <- max(taxon_grp_info$grp_as_num)
# 
# group_dat <- matrix(NA, nrow = constants$ngroup, ncol = nrow(taxon_grp_info))
# for (i in 1:constants$ngroup) {
#   group_dat[i, ] <- 0#as.numeric(taxon_grp_info$grp_as_num == i)
# }
# constants$group_dat <- group_dat
# constants$num.year <- 5

# group_levels <- taxon_grp_info %>%
#   dplyr::select(Size_Group, grp_as_num) %>%
#   distinct() %>%
#   arrange(grp_as_num)


m1 <- nimbleModel(ms.occ, 
                  constants = constants, 
                  data = model.data, 
                  inits = inits)
cMSOMmodel <- compileNimble(m1)
config_mcmc <- configureMCMC(model = m1, 
                             warnNoSamplerAssigned = TRUE,
                             print = TRUE, enableWAIC = use_WAIC)
config_mcmc$addMonitors(c("a", "b", "psi", "rich" 
                          #"dpsi", 
                          #"drich"
                          #"mu.a", "mu.b"
                          #"dp"
                          #"group_psi",
                          #"dpsi2"
                          #"group_dpsi",
                          #"hill1", "hill2", 
                          # "ranef_site" 
                          #"ranef_year", 
))

config_mcmc$removeSampler(c("mu.a[4]",
                            "mu.a[3]",
                            "mu.a[5",
                            "mu.a[8]"
))
config_mcmc$addSampler(target = c("mu.a[4]",
                                  "mu.a[3]",
                                  "mu.a[5]",
                                  "mu.a[8]",
),
type = "AF_slice")

if (do_ppc) {
  config_mcmc$addMonitors("logProb_X")
}

MCMC <- buildMCMC(m1, conf = config_mcmc)
cMCMC <- compileNimble(MCMC, project = m1)


samples2 <- runMCMC(cMCMC, niter = ni, nburnin = nb, nchains = nc, thin = nt, WAIC = use_WAIC)

if (use_WAIC) {
  WAIC <- samples2$WAIC
  samples2 <- samples2$samples
}



if (do_ppc) {
  
  dataNodes <- m1$getNodeNames(dataOnly = TRUE)
  parentNodes <- m1$getParents(dataNodes, stochOnly = TRUE)
  simNodes <- m1$getDependencies(parentNodes, self = FALSE)
  
  all_samples <- do.call(rbind, samples2)
  
  nSamp <- nrow(all_samples)
  
  # ppSamples <- matrix(0, nrow = nSamp, ncol =
  #                       length(m1$expandNodeNames(dataNodes, returnScalarComponents = TRUE)))
  ppDev <- numeric(nSamp)
  postNames <- colnames(all_samples)
  
  pb <- progress::progress_bar$new(total = nSamp)
  system.time({
    for(j in 1:nSamp) {
      pb$tick()
      values(cMSOMmodel, postNames) <- all_samples[j, ]
      cMSOMmodel$simulate(simNodes, includeData = TRUE)
      cMSOMmodel$calculate()
      ppDev[j] <- -2 * sum(cMSOMmodel$logProb_X)
      # ppSamples[i, ] <- values(cSSOMmodel, dataNodes)
    }
  })
  
  observed_deviance <- -2 * 
    rowSums(all_samples[, grepl("logProb_X", colnames(all_samples))])
  mean_obs_dev <- mean(observed_deviance)
  
  gof_df <- data.frame(
    type = c("Observed", rep("PPC", nSamp)),
    val = c(mean_obs_dev, ppDev),
    species = "MSOM"
  )
  write_csv(gof_df, "intermediate/msom_deviance_gof.csv")
}

b <- (ni - nb) %% nt
samples_list <- list()
if (nc > 1) {
  for (i in 1:nc) {
    samples_list[[i]] <- mcmc(data = samples2[[i]], start = nb + 1, end = ni - b, thin = nt)
  }
} else {
  samples_list[[1]] <- mcmc(data = samples2, start = nb + 1, end = ni - b, thin = nt)
}

samples2_coda <- mcmc.list(samples_list)

#MCMCtrace(samples2_coda, params = params_species, pdf = FALSE)
# MCMCplot(samples2_coda, params = "mu.B.road", ref_ovl = TRUE)

summary <- MCMCsummary(samples2_coda, HPD = TRUE, hpd_prob = 0.9) %>% 
  mutate(nonzero = sign(`90%_HPDL`) == sign(`90%_HPDU`))


summary$param <- rownames(summary)
rownames(summary) <- NULL

summary$species <- get_spec_from_par(summary$param, species = species)
par_info <- get_parname_from_par(summary$param, psi_covs, p_covs, ssom = FALSE)



summary$layer <- par_info$layer
summary$parname <- par_info$parname

summary$psiID <- get_psiID_from_par(summary$param)
#summary$group <- get_group_from_par(param = summary$param, groups = group_levels$Size_Group)

write_csv(summary, "intermediate/msom_summary_conf95.csv")

end_time <- Sys.time()

end_time - start_time

#Trace Plots

# params <- c("mu.a[1]", "mu.a[2]", "mu.a[3]", "mu.a[4]", "mu.a[5]", "mu.a[6]",
#             "mu.a[7]", "mu.a[8]", "mu.a[9]", "mu.a[10]", "mu.a[11]", "mu.a[12]",
#             "mu.b[1]", "mu.b[2]", "mu.b[3]", "mu.b[4]", "mu.b[5]", "mu.b[6]")

params <- c("mu.a", "mu.b")

# den_title <- c("Intercept (psi)", "Ruggedness (psi)", "Elevation (psi)",
#                "Canopy (psi)", "% Chaparral (psi)",
#                "% Grassland (psi)", "Burned (psi)", "BurnLag1 (psi)", "BurnLag2 (psi)",
#                "Burned*Canopy (psi)", "BurnLag1*Canopy (psi)", "BurnLag2*Canopy (psi)",
#                "Intercept (p)", "Attractant (p)", "Viewshed (p)", "Burned (p)", "BurnLag1 (p)", "BurnLag2 (p)")
# 
# tr_title <- c("Intercept (psi)", "Ruggedness (psi)", "Elevation (psi)",
#               "Canopy (psi)", "% Chaparral (psi)",
#               "% Grassland (psi)", "Burned (psi)", "BurnLag1 (psi)", "BurnLag2 (psi)",
#               "Burned*Canopy (psi)", "BurnLag1*Canopy (psi)", "BurnLag2*Canopy (psi)",
#               "Intercept (p)", "Attractant (p)", "Viewshed (p)", "Burned (p)", "BurnLag1 (p)", "BurnLag2 (p)")


# MCMCtrace(samples2_coda,
#           params = params,
#           ISB = TRUE,
#           #main_den = den_title,
#           #main_tr = tr_title,
#           #col_den = col_den,
#           #exact = TRUE,
#           pdf = TRUE)
