## An individual-based model of green sturgeon spawning migrations in the Sacramento River

## Initially, this model DOES NOT incorporate effects of environmental conditions (flow, temps)
## sturgeon behavior. This functionality will be added later.

## Developed by: Jonathan Walter, jawalter@ucdavis.edu
## With support from: Erin Tracy, Fran Bellido-Leiva, Scott Colborne, Sarah Yarnell, Karrigan Bork

## Start date: 2026-08-07
## Last updated: 2026-08-11

## MODEL STRUCTURE — matches multistate model occasions:
## PRE  = pre-migration (ocean/estuary staging)
## M1   = Benicia/Carquinez to Rio Vista (occ1 to occ2)
## M2   = Rio Vista to SR_MOUTH/Steamboat junction (occ2 to occ3) — mainstem fish only
## G1   = Georgiana Slough transit (occ2 to occ4)
## G2   = Georgiana rejoining mainstem (occ3-4 equivalent)
## SS1  = Steamboat/Sutter Slough entry (occ3 to occ4)
## SS2  = Steamboat/Sutter Slough mid (occ4)
## SS3  = Steamboat/Sutter rejoining mainstem (occ4 to occ5)
## M3   = Sacramento mainstem bypass of SS junction (occ3 to occ4)
## M4   = All routes rejoined — Sacramento city area (occ4 to occ5)
## M5   = Upper Sacramento through Feather confluence (occ5 to occ6)
## M6   = Upper Sacramento to spawning grounds (occ6 to occ7)
## SG   = Spawning grounds (occ7+)
## F    = Incomplete migration (absorbing)
## D    = Dead (absorbing)

rm(list=ls())

### Define inputs ---------------------------------------------------------------------------------

# model environment
nfish <- 1000
tmax  <- 365

# all possible states
states <- c("PRE","M1","M2","M3","M4","M5","M6",
            "G1","G2","SS1","SS2","SS3","SG","D","F")

# state transition parameters — from multistate model 12b at mean conditions
psi_geo  <- 0.106  # probability of routing into Georgiana Slough at Rio Vista (occ2)
psi_ss   <- 0.346  # probability of routing into Steamboat/Sutter at SR_MOUTH (occ3)
S_sac    <- 0.994  # per-transition survival — Sacramento mainstem
S_geo    <- 0.951  # per-transition survival — Georgiana Slough
S_ss     <- 0.976  # per-transition survival — Steamboat/Sutter
p_fail   <- 0.072  # per-transition incomplete migration probability (at mean temperature)
p_arrive <- 0.986  # probability of spawning ground arrival given survival to upper Sacramento

# TODO: replace fixed parameters with flow/temperature dependent functions
# psi_geo[i] = plogis(alpha_geo + beta_geo * flow_occ2[i])    beta_geo   = -0.784
# psi_ss[i]  = plogis(alpha_ss  + beta_ss  * flow_occ3[i])    beta_ss    = -0.823
# p_fail[i]  = plogis(alpha_fail + beta_fail_t * temp_7day[i]) beta_fail_t = 2.040

# transition time parameters
# TODO: replace with distributions informed by observed transit times from telemetry data
ttime_min <- 3
ttime_max <- 14

### Initialize state matrix -----------------------------------------------------------------------

fish_states <- matrix(NA, nrow = nfish, ncol = tmax)

### Main simulation loop --------------------------------------------------------------------------

for(ii in 1:nfish){
  
  #--- PRE-MIGRATION ---
  dstart <- round(runif(n = 1, min = 60, max = 120)) # TODO replace with data-informed dist'n
  fish_states[ii, 1:(dstart - 1)] <- "PRE"
  
  #--- M1: Benicia/Carquinez to Rio Vista (occ1 to occ2) ---
  dd    <- min(which(is.na(fish_states[ii,])))
  ttime <- round(runif(n = 1, min = ttime_min, max = ttime_max))
  fish_states[ii, dd:min(dd + ttime, tmax)] <- "M1"
  if(runif(1) > S_sac){
    fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "D"; next
  }
  if(runif(1) < p_fail){
    fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "F"; next
  }
  
  #--- JUNCTION 1: Georgiana Slough vs. continue mainstem (occ2 — Rio Vista) ---
  route <- runif(1)
  
  if(route < psi_geo){
    # ~~~~~ GEORGIANA ROUTE ~~~~~
    
    #--- G1: Georgiana Slough transit ---
    dd    <- min(which(is.na(fish_states[ii,])))
    ttime <- round(runif(n = 1, min = ttime_min, max = ttime_max))
    fish_states[ii, dd:min(dd + ttime, tmax)] <- "G1"
    if(runif(1) > S_geo){
      fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "D"; next
    }
    if(runif(1) < p_fail){
      fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "F"; next
    }
    
    #--- G2: Georgiana rejoining mainstem ---
    dd    <- min(which(is.na(fish_states[ii,])))
    ttime <- round(runif(n = 1, min = ttime_min, max = ttime_max))
    fish_states[ii, dd:min(dd + ttime, tmax)] <- "G2"
    if(runif(1) > S_geo){
      fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "D"; next
    }
    if(runif(1) < p_fail){
      fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "F"; next
    }
    
    # Georgiana fish rejoin mainstem at M4
    
  } else {
    # ~~~~~ CONTINUE ON MAINSTEM toward SR_MOUTH junction ~~~~~
    
    #--- M2: Rio Vista to SR_MOUTH junction (occ2 to occ3) ---
    dd    <- min(which(is.na(fish_states[ii,])))
    ttime <- round(runif(n = 1, min = ttime_min, max = ttime_max))
    fish_states[ii, dd:min(dd + ttime, tmax)] <- "M2"
    if(runif(1) > S_sac){
      fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "D"; next
    }
    if(runif(1) < p_fail){
      fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "F"; next
    }
    
    #--- JUNCTION 2: Steamboat/Sutter vs. continue mainstem (occ3 — SR_MOUTH) ---
    route2 <- runif(1)
    
    if(route2 < psi_ss){
      # ~~~~~ STEAMBOAT/SUTTER ROUTE ~~~~~
      
      #--- SS1: Steamboat/Sutter entry (occ3) ---
      dd    <- min(which(is.na(fish_states[ii,])))
      ttime <- round(runif(n = 1, min = ttime_min, max = ttime_max))
      fish_states[ii, dd:min(dd + ttime, tmax)] <- "SS1"
      if(runif(1) > S_ss){
        fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "D"; next
      }
      if(runif(1) < p_fail){
        fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "F"; next
      }
      
      #--- SS2: Steamboat/Sutter mid (occ4) ---
      dd    <- min(which(is.na(fish_states[ii,])))
      ttime <- round(runif(n = 1, min = ttime_min, max = ttime_max))
      fish_states[ii, dd:min(dd + ttime, tmax)] <- "SS2"
      if(runif(1) > S_ss){
        fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "D"; next
      }
      if(runif(1) < p_fail){
        fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "F"; next
      }
      
      #--- SS3: Steamboat/Sutter rejoining mainstem (occ4 to occ5) ---
      dd    <- min(which(is.na(fish_states[ii,])))
      ttime <- round(runif(n = 1, min = ttime_min, max = ttime_max))
      fish_states[ii, dd:min(dd + ttime, tmax)] <- "SS3"
      if(runif(1) > S_ss){
        fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "D"; next
      }
      if(runif(1) < p_fail){
        fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "F"; next
      }
      
      # SS fish rejoin mainstem at M4
      
    } else {
      # ~~~~~ CONTINUE ON MAINSTEM bypass of SS junction ~~~~~
      
      #--- M3: Mainstem bypass of SS junction (occ3 to occ4) ---
      dd    <- min(which(is.na(fish_states[ii,])))
      ttime <- round(runif(n = 1, min = ttime_min, max = ttime_max))
      fish_states[ii, dd:min(dd + ttime, tmax)] <- "M3"
      if(runif(1) > S_sac){
        fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "D"; next
      }
      if(runif(1) < p_fail){
        fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "F"; next
      }
      
    } # end junction 2
  } # end junction 1
  
  #--- ALL ROUTES REJOINED MAINSTEM ---
  
  #--- M4: Sacramento city area — all routes rejoined (occ4 to occ5) ---
  dd    <- min(which(is.na(fish_states[ii,])))
  ttime <- round(runif(n = 1, min = ttime_min, max = ttime_max))
  fish_states[ii, dd:min(dd + ttime, tmax)] <- "M4"
  if(runif(1) > S_sac){
    fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "D"; next
  }
  if(runif(1) < p_fail){
    fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "F"; next
  }
  
  #--- M5: Upper Sacramento through Feather River confluence (occ5 to occ6) ---
  dd    <- min(which(is.na(fish_states[ii,])))
  ttime <- round(runif(n = 1, min = ttime_min, max = ttime_max))
  fish_states[ii, dd:min(dd + ttime, tmax)] <- "M5"
  if(runif(1) > S_sac){
    fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "D"; next
  }
  if(runif(1) < p_fail){
    fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "F"; next
  }
  
  #--- M6: Upper Sacramento to spawning grounds (occ6 to occ7) ---
  dd    <- min(which(is.na(fish_states[ii,])))
  ttime <- round(runif(n = 1, min = ttime_min, max = ttime_max))
  fish_states[ii, dd:min(dd + ttime, tmax)] <- "M6"
  if(runif(1) > S_sac){
    fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "D"; next
  }
  if(runif(1) < p_fail){
    fish_states[ii, min(which(is.na(fish_states[ii,]))):tmax] <- "F"; next
  }
  
  #--- SPAWNING GROUND ARRIVAL (lambda — occ7) ---
  dd <- min(which(is.na(fish_states[ii,])))
  if(runif(1) < p_arrive){
    fish_states[ii, dd:tmax] <- "SG"
  } else {
    fish_states[ii, dd:tmax] <- "F"
  }
  
} # end fish loop

### Validation checks -----------------------------------------------------------------------------

cat("All states valid:", all(unique(c(fish_states)) %in% c(states, NA)), "\n")

cat("\nFinal states:\n")
print(table(fish_states[, tmax]))

cat("\nRoute breakdown:\n")
took_geo <- apply(fish_states, 1, function(x) any(x == "G1", na.rm = TRUE))
took_ss  <- apply(fish_states, 1, function(x) any(x == "SS1", na.rm = TRUE))
took_sac <- !took_geo & !took_ss
cat("Georgiana:", sum(took_geo), "(", round(mean(took_geo) * 100, 1), "%)\n")
cat("Steamboat/Sutter:", sum(took_ss), "(", round(mean(took_ss) * 100, 1), "%)\n")
cat("Sacramento mainstem:", sum(took_sac), "(", round(mean(took_sac) * 100, 1), "%)\n")

