## An individual-based model of green sturgeon spawning migrations in the Sacramento River

## Initially, this model DOES NOT incorporate effects of environmental conditions (flow, temps) 
## sturgeon behavior. This functionality will be added later.

## Developed by: Jonathan Walter, jawalter@ucdavis.edu
## With support from: Erin Tracy, Fran Bellido-Leiva, Scott Colborne, Sarah Yarnell, Karrigan Bork

## Start date: 2026-08-07
## Last updated: 

rm(list=ls())

### Define inputs ---------------------------------------------------------------------------------

#model environment
nfish = 10 #number of fish to simulate
tmax = 365 #(days) number of timesteps in simulation, assumes sim starts on Jan 1
states <- c("PRE","M1","M2","M3","M4","M5","M6","SS1","SS2","SS3","G1","G2","SG","D","F") #vector of states a fish can take

#state transition parameters (routing, survival, migration failure)
psi_geo = 0.106 # probability of routing through Georgiana Slough
psi_ss = 0.346 #probability of routing through Sutter/Steambat
S_all = 0.975 #per-transition survival probability (spatiotemporally constant)
p_fail = 0.074 #per-transition failure probability
p_arrive = 0.986 #probability of spawning ground arrival (conditioned on other outcomes)

#transition time parameters




### Information needs ------------------------------------------------------------------
## Distribution of migration start dates
## Distribution of routing timings between all sampling occasions


fish_states <- matrix(NA, nrow=nfish, ncol=tmax) #matrix to hold modeled fish states

for(ii in 1:nfish){ #loop over fish
  
  #Migration start date
  dstart.ii <- round(runif(n=1, min=30, max=60)) #TODO replace with dist'n informed by data
  fish_states[ii, 1:(dstart.ii-1)] <- "PRE" #update fish states matrix
  
  #Transit through segment M1 (Mainstem Sacramento River)
  ttime <- round(runif(n=1, min=5, max=10)) #TODO replace with dist'n informed by data
  fish_states[ii, dstart.ii:(dstart.ii + ttime)] <- "M1" #update fish states matrix
  if(runif(1,0,1) > S_all){ #implement per-segment mortality
    fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "D"
    next
  }
  if(runif(1,0,1) < p_fail){ #implement per-segment migration failure
    fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "F"
    next
  }
  
  #Junction routing
  route <- runif(1,0,1)
  if(route < psi_geo){ #route to Georgiana Slough
    
    #Transit through segment G1 (Georgiana Slough)
    dd <- min(which(is.na(fish_states[ii,])))
    ttime <- round(runif(n=1, min=5, max=10)) #TODO replace with dist'n informed by data
    fish_states[ii, dd:(dd + ttime)] <- "G1" #update fish states matrix
    if(runif(1,0,1) > S_all){ #implement per-segment mortality
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "D"
      next
    }
    if(runif(1,0,1) < p_fail){ #implement per-segment migration failure
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "F"
      next
    }
    
    #Transit through segment G2 (Georgiana Slough)
    dd <- min(which(is.na(fish_states[ii,])))
    ttime <- round(runif(n=1, min=5, max=10)) #TODO replace with dist'n informed by data
    fish_states[ii, dd:(dd + ttime)] <- "G2" #update fish states matrix
    if(runif(1,0,1) > S_all){ #implement per-segment mortality
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "D"
      next
    }
    if(runif(1,0,1) < p_fail){ #implement per-segment migration failure
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "F"
      next
    }
    
    #Transit through segment S4 (rejoining mainstem Sacramento River)
    dd <- min(which(is.na(fish_states[ii,])))
    ttime <- round(runif(n=1, min=5, max=10)) #TODO replace with dist'n informed by data
    fish_states[ii, dd:(dd + ttime)] <- "M4" #update fish states matrix
    if(runif(1,0,1) > S_all){ #implement per-segment mortality
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "D"
      next
    }
    if(runif(1,0,1) < p_fail){ #implement per-segment migration failure
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "F"
      next
    }
  }
  
  if(route > psi_geo & route <= (psi_geo + psi_ss)){ #route to Sutter/Steamboat Slough
    
    #Transit through segment SS1 (Steamboat/Sutter)
    dd <- min(which(is.na(fish_states[ii,])))
    ttime <- round(runif(n=1, min=5, max=10)) #TODO replace with dist'n informed by data
    fish_states[ii, dd:(dd + ttime)] <- "SS1" #update fish states matrix
    if(runif(1,0,1) > S_all){ #implement per-segment mortality
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "D"
      next
    }
    if(runif(1,0,1) < p_fail){ #implement per-segment migration failure
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "F"
      next
    }
    
    #Transit through segment SS2 (Steamboat/Sutter)
    dd <- min(which(is.na(fish_states[ii,])))
    ttime <- round(runif(n=1, min=5, max=10)) #TODO replace with dist'n informed by data
    fish_states[ii, dd:(dd + ttime)] <- "SS2" #update fish states matrix
    if(runif(1,0,1) > S_all){ #implement per-segment mortality
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "D"
      next
    }
    if(runif(1,0,1) < p_fail){ #implement per-segment migration failure
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "F"
      next
    }
    
    #Transit through segment SS3 (Steamboat/Sutter)
    dd <- min(which(is.na(fish_states[ii,])))
    ttime <- round(runif(n=1, min=5, max=10)) #TODO replace with dist'n informed by data
    fish_states[ii, dd:(dd + ttime)] <- "SS3" #update fish states matrix
    if(runif(1,0,1) > S_all){ #implement per-segment mortality
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "D"
      next
    }
    if(runif(1,0,1) < p_fail){ #implement per-segment migration failure
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "F"
      next
    }
  }
  
  if(route > (psi_geo + psi_ss)){ #stay on mainstem Sacramento River
    
    #Transit through segment M2 (mainstem)
    dd <- min(which(is.na(fish_states[ii,])))
    ttime <- round(runif(n=1, min=5, max=10)) #TODO replace with dist'n informed by data
    fish_states[ii, dd:(dd + ttime)] <- "M2" #update fish states matrix
    if(runif(1,0,1) > S_all){ #implement per-segment mortality
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "D"
      next
    }
    if(runif(1,0,1) < p_fail){ #implement per-segment migration failure
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "F"
      next
    }
    
    #Transit through segment M3 (mainstem)
    dd <- min(which(is.na(fish_states[ii,])))
    ttime <- round(runif(n=1, min=5, max=10)) #TODO replace with dist'n informed by data
    fish_states[ii, dd:(dd + ttime)] <- "M3" #update fish states matrix
    if(runif(1,0,1) > S_all){ #implement per-segment mortality
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "D"
      next
    }
    if(runif(1,0,1) < p_fail){ #implement per-segment migration failure
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "F"
      next
    }
    
    #Transit through segment M4 (mainstem)
    dd <- min(which(is.na(fish_states[ii,])))
    ttime <- round(runif(n=1, min=5, max=10)) #TODO replace with dist'n informed by data
    fish_states[ii, dd:(dd + ttime)] <- "M4" #update fish states matrix
    if(runif(1,0,1) > S_all){ #implement per-segment mortality
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "D"
      next
    }
    if(runif(1,0,1) < p_fail){ #implement per-segment migration failure
      fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "F"
      next
    }
  }
  
  #all routes now rejoined mainstem Sacremento river
  
  #Transit through segment M5 (mainstem)
  dd <- min(which(is.na(fish_states[ii,])))
  ttime <- round(runif(n=1, min=5, max=10)) #TODO replace with dist'n informed by data
  fish_states[ii, dd:(dd + ttime)] <- "M5" #update fish states matrix
  if(runif(1,0,1) > S_all){ #implement per-segment mortality
    fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "D"
    next
  }
  if(runif(1,0,1) < p_fail){ #implement per-segment migration failure
    fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "F"
    next
  }
  
  #Transit through segment M6 (mainstem)
  dd <- min(which(is.na(fish_states[ii,])))
  ttime <- round(runif(n=1, min=5, max=10)) #TODO replace with dist'n informed by data
  fish_states[ii, dd:(dd + ttime)] <- "M6" #update fish states matrix
  if(runif(1,0,1) > S_all){ #implement per-segment mortality
    fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "D"
    next
  }
  if(runif(1,0,1) < p_fail){ #implement per-segment migration failure
    fish_states[ii, min(which(is.na(fish_states[ii,]))):ncol(fish_states)] <- "F"
    next
  }
  
  #spawning ground arrival
  if(runif(1,0,1) < p_arrive){
    dd <- min(which(is.na(fish_states[ii,])))
    fish_states[ii, dd:ncol(fish_states)] <- "SG"
  }
}


# some validation checks
all(unique(c(fish_states)) %in% states)
table(fish_states[,ncol(fish_states)])
