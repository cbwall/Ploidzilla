
########
######## Calculate Coral Dry Weights from Buoyant Weight (Davies 1989)
########

## dry weight of object = weight in water + ((weight in air * Density of water) / Density of object)

## Density of aragonite = 2.93 g/cm-3 (Jokiel et al. 1978)
## avg sea water density = 1.023 g cm-3


#### Using function rho from seacarb package
library(seacarb)
## rho(S = 35, T = 25, P = 0)

# load dataset 
bw <- read.csv("C:\\Users\\jglaz\\Desktop\\Datasets\\BW_data.csv")

bw <- bw %>% 
  select(frag_id:salinity) %>% 
  mutate(sw_dens = rho(S = salinity, T = temp, P = 0), # calculate density of seawater
         sw_dens = sw_dens * 0.001) %>%  # convert from kg cm-3 to g cm-3
  drop_na(sw_dens) %>%
  mutate(n = 1:nrow(.))

# add skeletal densities
bw <- bw %>% 
  select(-n) %>%
  mutate(skel_dens = 2.93) %>% # add skeletal densities
  mutate(dry_weight = wet_weight / (1 - (sw_dens/skel_dens)))
