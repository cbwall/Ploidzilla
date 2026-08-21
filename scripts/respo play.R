#### Respirometry carpentry #######
install.packages("respR")
library("respR")
library("hms")

########### import data csv from software
r.dat<-read.csv("data/respirometry all days.csv", skip=1) # skip top line from export

#### Clean and format data
# all samples here have both respiration (~15min) followed by photosynthesis (~15 min)
# rename and clean up to keep only columns we need

r.dat <- r.dat %>% rename_with(tolower)

# reduce columns
r.dat <- r.dat %>% 
  select(date, time, channel, sensorid, sensor_name, delta_t, value, o2_unit,
         temp, salinity)

r.dat<-  r.dat %>% 
  rename(sensorID = sensorid,
         delta.t.min = delta_t,
         temp.C = temp,
         O2 = value,
         O2.unit = o2_unit)

# format data
r.dat$date<-as.Date(r.dat$date, "%m/%d/%y") # fix date format
r.dat$time<-as_hms(r.dat$time) # time format to give proper "leading 0"
r.dat$time<-as.character(r.dat$time) # return to character string for parsing

# remove first day attempts, and make a new column separating analysis days
r.dat<- r.dat[(r.dat$date > "2025-07-15"),]
day1<-r.dat[(r.dat$date == "2025-07-16"),]
day2<-r.dat[(r.dat$date == "2025-07-17"),]

# separate each day into its individual batch "runs" of 10 chamber
# this sucks, but ultimately a human eye and note-taking is the best appraoch

# first day: runs 1-7
day1<- day1 %>%
  mutate(run = case_when(time < "11:32:00" ~ "run.1",
                                time > "11:32:00" & time < "12:15:00" ~ "run.2",
                                time > "12:45:00" & time < "13:25:00" ~ "run.3",
                                time > "13:52:00" & time < "14:27:00" ~ "run.4",
                                time > "14:43:00" & time < "15:16:00" ~ "run.5",
                                time > "15:43:00" & time < "16:10:00" ~ "run.6",
                                time > "16:44:00" & time < "17:11:00" ~ "run.7"))

# second day: runs 8-13
day2<- day2 %>%
  mutate(run = case_when(time < "10:05:00" ~ "run.8",
                         time > "10:27:00" & time < "11:07:00" ~ "run.9",
                         time > "11:54:00" & time < "12:35:00" ~ "run.10",
                         time > "12:55:00" & time < "13:45:00" ~ "run.11",
                         time > "14:28:00" & time < "14:47:00" ~ "run.12",
                         time > "14:54:00" & time < "15:12:00" ~ "run.13"))

# re-join the two data from the 2 days
r.dat2<-rbind(day1, day2)
r.dat2$time<-as.character(r.dat2$time) # return to  time format

# pull in metadata
run.info<-read.csv("data/respo run data.csv")
run.info.simple<- run.info %>% 
  select(day, run, tank.ID, tank.treatment, plug.ID, channel)

# merge, fist by run, then by channel
merge.dat<-merge(r.dat2, run.info.simple, by = c("run", "channel"))

# format metadata
make.fac<-c("day", "run", "channel", "tank.ID", "tank.treatment", "plug.ID")
merge.dat[make.fac]<-lapply(merge.dat[make.fac], factor) # make all these factors
merge.dat$time<-as_hms(merge.dat$time) # retun to time format

# arrange the data by the by run, then channel, then time as hms
merge.dat <- merge.dat %>% arrange(run, channel, time)

# make a sequence for these data to reflect the interval between measurements (3 seconds)
merge.dat <- merge.dat %>% group_by(plug.ID) %>% 
  mutate(log.seq = row_number() - 1, # start row #s at 0 for first measurement
         time.min = (log.seq*3)/60)

# organize
merge.dat<- merge.dat %>%
  select(date, day, run, time, log.seq, time.min, channel, tank.ID, tank.treatment, plug.ID, 
         O2, O2.unit, temp.C, salinity)

write.csv(merge.dat, "output/cleaned_resp_dat.csv")

# split runs up to individual dfs based on "run" as factor levels
run_list <- split(merge.dat, merge.dat$run)
list2env(run_list, envir = .GlobalEnv) # now as "run.1", "run.2"... "run.13" in environment


###########################################################
# make wide format for RespR
RespR.simple<- merge.dat %>%
  select(run, plug.ID, time.min, O2)

wide_RespR <- RespR.simple %>%
  pivot_wider(
    names_from = c(run, plug.ID), 
    values_from = O2     # Column containing the actual data points
  ) %>%
  arrange(time.min) 

write.csv(wide_RespR, "output/cleaned_resp_dat_wide.csv")

################ ################ 
# not the most elegant, but it preserved the wide-format needed for RespR
# be aware a number of samples appear to have been recorded in %a.s., so will need to recalculate these
# isolate the runs of 10 and make their own dfs with nas removed.

###########################################################
##### separate data into individual runs of 10 channels ### 
########################################################### 

run1_wide <- wide_RespR %>% 
 select(time.min, starts_with("run.1_"), -contains("control"))
run1_wide<-na.omit(run1_wide)

run2_wide <- wide_RespR %>% 
  select(time.min,starts_with("run.2"), -contains("control"))
run2_wide<-na.omit(run2_wide)

run3_wide <- wide_RespR %>% 
  select(time.min,starts_with("run.3"), -contains("control"))
run3_wide<-na.omit(run3_wide)

run4_wide <- wide_RespR %>% 
  select(time.min,starts_with("run.4"), -contains("control"))
run4_wide<-na.omit(run4_wide)

run5_wide <- wide_RespR %>%   ## ALL controls
  select(time.min,starts_with("run.5"))
run5_wide<-na.omit(run5_wide)

run6_wide <- wide_RespR %>% 
  select(time.min,starts_with("run.6"), -contains("control"))
run6_wide<-na.omit(run6_wide)

run7_wide <- wide_RespR %>% 
  select(time.min,starts_with("run.7"), -contains("control"))
run7_wide<-na.omit(run7_wide)

run8_wide <- wide_RespR %>%   ## ALL controls
  select(time.min,starts_with("run.8"))
run8_wide<-na.omit(run8_wide)

run9_wide <- wide_RespR %>% 
  select(time.min,starts_with("run.9"), -contains("control"))
run9_wide<-na.omit(run9_wide)

run10_wide <- wide_RespR %>% 
  select(time.min,starts_with("run.10"), -contains("control"))
run10_wide<-na.omit(run10_wide)

run11_wide <- wide_RespR %>% 
  select(time.min,starts_with("run.11"), -contains("control"))
run11_wide<-na.omit(run11_wide)

run12_wide <- wide_RespR %>%  ## ALL controls
  select(time.min,starts_with("run.12"))
run12_wide<-na.omit(run12_wide)

run13_wide <- wide_RespR %>%  ## controls
  select(time.min,starts_with("run.13"))
run13_wide<-na.omit(run13_wide) 


######################## ######################## ##############
###### ###### ###### isolate bg controls ######## ##############
######################## ######################## ##############

amb.bg <- wide_RespR %>% 
  select(time.min, contains("AT"))
amb.bg<- amb.bg %>% select(-"run.10_AT.control.10") %>% na.omit(amb.bg)

HT.bg <- wide_RespR %>% 
  select(time.min, contains("HT"))  %>% 
      select(-"run.1_HT.control.1",  # issue with this control., drop it.
             -"run.5_HT.control.2.a",
             -"run.3_HT.control.3")  %>% na.omit(HT.bg)

######################## ######################## ##############
###### background adjust and inspect
# no clear light effect but the probes show different response time, so need to inspect manually
# ambient 
amb_bg_insp<-inspect(amb.bg, time = 1, oxygen = 2:29)
amb.bg.calc<-calc_rate.bg(amb_bg_insp)

# heated
HT_bg_insp<-inspect(HT.bg, time = 1, oxygen = 2:21)
HT.bg.calc<-calc_rate.bg(HT_bg_insp)


######################## ######################## ################
############### AMBIENT TEMP background rates ########### 
######################## ######################## ################

# an attempt to make this less "user defined" each time...
df<-amb.bg # define df
output_list<- vector("list") # make a blank list to store results
ncol(df) # see how many column you have, this will be your length of the list

## run a sample, change df # and output list #
var<-colnames(df[,2]) # set your column each time, starts at "2", time is "1"
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time") # unfortuantely this takes a human eye
output_list[[1]]<-calc$summary

## run a sample
var<-colnames(df[,3]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time") # unfortuantely this takes a human eye
output_list[[2]]<-calc$summary

## run a sample
var<-colnames(df[,4]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time") # unfortuantely this takes a human eye
output_list[[3]]<-calc$summary

## run a sample
var<-colnames(df[,5]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 10, to = 20, by = "time")
output_list[[4]]<-calc$summary

## run a sample
var<-colnames(df[,6]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 24, by = "time")
output_list[[5]]<-calc$summary

## run a sample
var<-colnames(df[,7]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 20, by = "time")
output_list[[6]]<-calc$summary

## run a sample
var<-colnames(df[,8]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 10, to = 22, by = "time")
output_list[[7]]<-calc$summary

## run a sample
var<-colnames(df[,9]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 10, to = 25, by = "time")
output_list[[8]]<-calc$summary

## run a sample
var<-colnames(df[,10]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 20, by = "time")
output_list[[9]]<-calc$summary

## run a sample
var<-colnames(df[,11]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 20, by = "time")
output_list[[10]]<-calc$summary

## run a sample
var<-colnames(df[,12]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 20, by = "time")
output_list[[11]]<-calc$summary

## run a sample
var<-colnames(df[,13]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var) # a bit wonky
calc<-calc_rate(insp, from =2, to = 10, by = "time")
output_list[[12]]<-calc$summary

## run a sample
var<-colnames(df[,14]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 2, to = 15, by = "time")
output_list[[13]]<-calc$summary

## run a sample
var<-colnames(df[,15]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 2, to = 15, by = "time")
output_list[[14]]<-calc$summary

## run a sample
var<-colnames(df[,16]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 2, to = 15, by = "time")
output_list[[15]]<-calc$summary

## run a sample
var<-colnames(df[,17]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 2, to = 15, by = "time")
output_list[[16]]<-calc$summary

## run a sample 
var<-colnames(df[,18]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 3, to = 10, by = "time") #### this one is weird, maybe check notes
output_list[[17]]<-calc$summary

## run a sample
var<-colnames(df[,19]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 2, to = 15, by = "time")
output_list[[18]]<-calc$summary

## run a sample
var<-colnames(df[,20]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 2, to = 15, by = "time")
output_list[[19]]<-calc$summary

## run a sample
var<-colnames(df[,21]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 4, to = 15, by = "time")
output_list[[20]]<-calc$summary

## run a sample
var<-colnames(df[,22]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 18, by = "time")
output_list[[21]]<-calc$summary

## run a sample
var<-colnames(df[,23]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 8, to = 20, by = "time")
output_list[[22]]<-calc$summary

## run a sample
var<-colnames(df[,24]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[23]]<-calc$summary

## run a sample
var<-colnames(df[,25]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[24]]<-calc$summary

## run a sample
var<-colnames(df[,26]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[25]]<-calc$summary

## run a sample
var<-colnames(df[,27]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[26]]<-calc$summary

## run a sample
var<-colnames(df[,28]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 8, to = 15, by = "time")
output_list[[27]]<-calc$summary

## run a sample
var<-colnames(df[,29]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[28]]<-calc$summary

################# ################# ################# ################# 
################# working with the output ################# ###########
# now that all the samples are run, add in the names as column 1 in the list
# this works if you go in the order of the columns.
names(output_list)<-colnames(df[-1])

#combine outputs into a df and add names
output_df<-data.frame(do.call(rbind, output_list))
output_df<- output_df %>%
  mutate(names= names(output_list),
         sample.or.bg= "bg")

####### #######  RENAME and get rate means #######  ####### 
bg.amb.rates<-output_df  # rename to match df at the top
bg.amb.rates.means<-mean(bg.amb.rates$rate); print(bg.amb.rates.means) # -0.0324

######################## ######################## ################
############### HIGH TEMP background rates ########### 
######################## ######################## ################

df<-HT.bg # define df
output_list<- vector("list") # make a blank list to store results
ncol(df) # see how many column you have, this will be your length of the list

## run a sample
var<-colnames(df[2]) # set your column each time
insp<-inspect(HT.bg, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[1]]<-calc$summary

## run a sample
var<-colnames(df[,3]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[2]]<-calc$summary

## run a sample
var<-colnames(df[,4]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[3]]<-calc$summary

## run a sample
var<-colnames(df[,5]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[4]]<-calc$summary

## run a sample
var<-colnames(df[,6]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[5]]<-calc$summary

## run a sample
var<-colnames(df[,7]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 8, to = 15, by = "time")
output_list[[6]]<-calc$summary

## run a sample
var<-colnames(df[,8]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 8, to = 15, by = "time")
output_list[[7]]<-calc$summary

## run a sample
var<-colnames(df[,9]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[8]]<-calc$summary

## run a sample
var<-colnames(df[,10]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[9]]<-calc$summary

## run a sample
var<-colnames(df[,11]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 8, to = 15, by = "time")
output_list[[10]]<-calc$summary

## run a sample
var<-colnames(df[,12]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[11]]<-calc$summary

## run a sample
var<-colnames(df[,13]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var) # note out of sequence
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[12]]<-calc$summary

## run a sample
var<-colnames(df[,14]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[13]]<-calc$summary

## run a sample
var<-colnames(df[,15]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 14, by = "time")
output_list[[14]]<-calc$summary

## run a sample
var<-colnames(df[,16]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 3, to = 12, by = "time")
output_list[[15]]<-calc$summary

## run a sample
var<-colnames(df[,17]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[16]]<-calc$summary

## run a sample
var<-colnames(df[,18]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 3, to = 11, by = "time")
output_list[[17]]<-calc$summary

## run a sample
var<-colnames(df[,19]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[18]]<-calc$summary

## run a sample
var<-colnames(df[,20]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 12, by = "time")
output_list[[19]]<-calc$summary

## run a sample
var<-colnames(df[,21]) # set your column each time
insp<-inspect(df, time = "time.min", oxygen = var)
calc<-calc_rate(insp, from = 5, to = 15, by = "time")
output_list[[20]]<-calc$summary


################# ################# ################# ################# 
################# working with the output ################# ###########
names(output_list)<-colnames(df[-1])

#combine outputs into a df and add names
output_df<-data.frame(do.call(rbind, output_list))
output_df<- output_df %>%
  mutate(names= names(output_list),
         sample.or.bg= "bg")

####### #######  RENAME and get rate means #######  ####### 
bg.HT.rates<-output_df  # rename to match df at the top
bg.HT.rates.means<-mean(bg.HT.rates$rate); print(bg.HT.rates.means) # -0.0841
######################## ######################## ##########





######################## ######################## ################
############### RUNN SAMPLES for R and Pnet rates ################ 
######################## ######################## ################


######## run 3 ######## 

# resp output
out.ls.run.3.resp <- vector("list")

# pnet output
out.ls.run.3.pnet <- vector("list")

##
r3.61<-inspect(run3_wide, time = "time.min", oxygen = "run.3_61")
r3.61.resp<-calc_rate(r3.61, from = 5, to = 15, by = "time")
r3.61.pnet<-calc_rate(r3.61, from = 25, to = 35, by = "time")

out.ls.run.3.resp[[1]]<-r3.61.resp$summary
out.ls.run.3.pnet[[1]]<-r3.61.pnet$summary

##
r3.46<-inspect(run3_wide, time = "time.min", oxygen = "run.3_46") # note out of sequence
r3.46.resp<-calc_rate(r3.46, from = 5, to = 15, by = "time")
r3.46.pnet<-calc_rate(r3.46, from = 28, to = 38, by = "time")

out.ls.run.3.resp[[2]]<-r3.46.resp$summary
out.ls.run.3.pnet[[2]]<-r3.46.pnet$summary

##
r3.69<-inspect(run3_wide, time = "time.min", oxygen = "run.3_69")
r3.69.resp<-calc_rate(r3.69, from = 5, to = 15, by = "time")
r3.69.pnet<-calc_rate(r3.69, from = 25, to = 37, by = "time")

out.ls.run.3.resp[[3]]<-r3.69.resp$summary
out.ls.run.3.pnet[[3]]<-r3.69.pnet$summary

##
r3.45<-inspect(run3_wide, time = "time.min", oxygen = "run.3_45")
r3.45.resp<-calc_rate(r3.45, from = 5, to = 16, by = "time")
r3.45.pnet<-calc_rate(r3.45, from = 28, to = 38, by = "time")

out.ls.run.3.resp[[4]]<-r3.45.resp$summary
out.ls.run.3.pnet[[4]]<-r3.45.pnet$summary

##
r3.10<-inspect(run3_wide, time = "time.min", oxygen = "run.3_10")
r3.10.resp<-calc_rate(r3.10, from = 5, to = 18, by = "time")
r3.10.pnet<-calc_rate(r3.10, from = 25, to = 38, by = "time")

out.ls.run.3.resp[[5]]<-r3.10.resp$summary
out.ls.run.3.pnet[[5]]<-r3.10.pnet$summary

##
r3.J4.2<-inspect(run3_wide, time = "time.min", oxygen = "run.3_J4-2")
r3.J4.2.resp<-calc_rate(r3.J4.2, from = 5, to = 15, by = "time")
r3.J4.2.pnet<-calc_rate(r3.J4.2, from = 28, to = 37, by = "time")

out.ls.run.3.resp[[6]]<-r3.J4.2.resp$summary
out.ls.run.3.pnet[[6]]<-r3.J4.2.pnet$summary

##
r3.34<-inspect(run3_wide, time = "time.min", oxygen = "run.3_34")
r3.34.resp<-calc_rate(r3.34, from = 5, to = 15, by = "time")
r3.34.pnet<-calc_rate(r3.34, from = 25, to = 35, by = "time")

out.ls.run.3.resp[[7]]<-r3.34.resp$summary
out.ls.run.3.pnet[[7]]<-r3.34.pnet$summary

##
r3.72<-inspect(run3_wide, time = "time.min", oxygen = "run.3_72")
r3.72.resp<-calc_rate(r3.72, from = 5, to = 15, by = "time")
r3.72.pnet<-calc_rate(r3.72, from = 25, to = 35, by = "time")

out.ls.run.3.resp[[8]]<-r3.34.resp$summary
out.ls.run.3.pnet[[8]]<-r3.34.pnet$summary

##
r3.21<-inspect(run3_wide, time = "time.min", oxygen = "run.3_21")
r3.21.resp<-calc_rate(r3.21, from = 5, to = 15, by = "time")
r3.21.pnet<-calc_rate(r3.21, from = 25, to = 37, by = "time")

out.ls.run.3.resp[[9]]<-r3.21.resp$summary
out.ls.run.3.pnet[[9]]<-r3.21.pnet$summary


######## run 4 ######## 

# resp output
out.ls.run.4.resp <- vector("list")

# pnet output
out.ls.run.4.pnet <- vector("list")

##
r4.4<-inspect(run4_wide, time = "time.min", oxygen = "run.4_4")
r4.4.resp<-calc_rate(r4.4, from = 5, to = 15, by = "time")
r4.4.pnet<-calc_rate(r4.4, from = 22, to = 32, by = "time")

out.ls.run.4.resp[[1]]<-r4.4.resp$summary
out.ls.run.4.pnet[[1]]<-r4.4.pnet$summary

##
r4.30<-inspect(run4_wide, time = "time.min", oxygen = "run.4_30") # note out of sequence
r4.30.resp<-calc_rate(r4.30, from = 5, to = 15, by = "time")
r4.30.pnet<-calc_rate(r4.30, from = 21, to = 32, by = "time")

out.ls.run.4.resp[[2]]<-r4.30.resp$summary
out.ls.run.4.pnet[[2]]<-r4.30.pnet$summary

##
r4.40<-inspect(run4_wide, time = "time.min", oxygen = "run.4_40")
r4.40.resp<-calc_rate(r4.40, from = 5, to = 15, by = "time")
r4.40.pnet<-calc_rate(r4.40, from = 21, to = 32, by = "time")

out.ls.run.4.resp[[3]]<-r4.40.resp$summary
out.ls.run.4.pnet[[3]]<-r4.40.pnet$summary

##
r4.17<-inspect(run4_wide, time = "time.min", oxygen = "run.4_17")
r4.17.resp<-calc_rate(r4.17, from = 5, to = 16, by = "time")
r4.17.pnet<-calc_rate(r4.17, from = 21, to = 32, by = "time")

out.ls.run.4.resp[[4]]<-r4.17.resp$summary
out.ls.run.4.pnet[[4]]<-r4.17.pnet$summary

##
r4.2<-inspect(run4_wide, time = "time.min", oxygen = "run.4_2")
r4.2.resp<-calc_rate(r4.2, from = 5, to = 16, by = "time")
r4.2.pnet<-calc_rate(r4.2, from = 20, to = 32, by = "time")

out.ls.run.4.resp[[5]]<-r4.2.resp$summary
out.ls.run.4.pnet[[5]]<-r4.2.pnet$summary

##
r4.51<-inspect(run4_wide, time = "time.min", oxygen = "run.4_51")
r4.51.resp<-calc_rate(r4.51, from = 5, to = 18, by = "time")
r4.51.pnet<-calc_rate(r4.51, from = 23, to = 32, by = "time")

out.ls.run.4.resp[[6]]<-r4.51.resp$summary
out.ls.run.4.pnet[[6]]<-r4.51.pnet$summary

##
r4.20<-inspect(run4_wide, time = "time.min", oxygen = "run.4_20")
r4.20.resp<-calc_rate(r4.20, from = 5, to = 13, by = "time")
r4.20.pnet<-calc_rate(r4.20, from = 20, to = 32, by = "time")

out.ls.run.4.resp[[7]]<-r4.20.resp$summary
out.ls.run.4.pnet[[7]]<-r4.20.pnet$summary

##
r4.44<-inspect(run4_wide, time = "time.min", oxygen = "run.4_44")
r4.44.resp<-calc_rate(r4.44, from = 5, to = 15, by = "time")
r4.44.pnet<-calc_rate(r4.44, from = 20, to = 32, by = "time")

out.ls.run.4.resp[[8]]<-r4.44.resp$summary
out.ls.run.4.pnet[[8]]<-r4.44.pnet$summary

##
r4.28<-inspect(run4_wide, time = "time.min", oxygen = "run.4_28")
r4.28.resp<-calc_rate(r4.28, from = 5, to = 15, by = "time")
r4.28.pnet<-calc_rate(r4.28, from = 20, to = 32, by = "time")

out.ls.run.4.resp[[9]]<-r4.28.resp$summary
out.ls.run.4.pnet[[9]]<-r4.28.pnet$summary






###########################################################
##### separate data into individual runs and channels ##### 
########################################################### 

# probes can sometimes log incorrect data units for some reason as a glitch. 
# make sure all data for O2 is in same units (umol O2/L)
run.to.parse<- merge.dat[(merge.dat$O2_Unit=="μmol/L"),]

# note that run 1 has some unit issues, overwrite and save the manual version
run.1.manual<-merge.dat[(merge.dat$run=="run.1"),]

#############
# Alternative to above....
# split all the data by runs
# now all named as "run.#_respo" 
# for (group_name in names(run_list)) {
#  assign(paste0(group_name, "_respo"), run_list[[group_name]])}
#################

# now all named as "run.#_respo", with all channels
# split the run by channel
run.2.split<- split(run.2, run.2$Channel)

# split the run by channel
# now all as "run#_ch#"
for (group_name in names(run.2.split)) {
  assign(paste0("run.2_ch", group_name), run.2.split[[group_name]])
}

# inspect and observe full data
# add rownames for later calc assistance
rownames(run.2_ch10) <- 1:nrow(run.2_ch10)
run2.ch10.reg<-inspect(run.2_ch10, time = "delta.t.min", oxygen = "O2")

# rate for the entire dataset
calc_rate(run2.ch10.reg)

# reduce areas to plot and focus on regions
calc_rate(run2.ch10.reg, from = 1203.667, to = 1222.567, by = "time")
calc_rate(run2.ch10.reg, from = 50, to = 300, by = "row")
