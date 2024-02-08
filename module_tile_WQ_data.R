#Module for calculating flow weighted daily C for monitored tile outlets
#described in Dolph et al 
#edge of field and tile WQ data from MN Discovery Farms: https://discoveryfarmsmn.org/

#set working directory
setwd(input_dir)

#read in WQ files for all farms
files <- fs::dir_ls(path = "./MN_DF_tile_WQ_data", glob = "*C_Q*csv")
files

WQ.all<-readr::read_csv(files, id = "path")
head(WQ.all)
names(WQ.all)

#Date range across all sites
summary(WQ.all$DateTime)

#check number of sites; note this includes edge of field (EOF) and tile outlets
levels(factor(WQ.all$Site_ID))

#Calculating daily flow weighted C (daily C) for EOF and tile outlets
#Group by site, then:
#1) multiply C by the Q measurements to estimate continuous loads
#2) sum into daily loads;
#3) divide daily load by summed daily Q to compute
#a daily flow-weighted C in milligrams per liter = daily C
#4) calculate the ratio of DRP/TP for each day using the daily C data


#This gives daily flow weighted C:
FWC<-WQ.all %>%
  mutate(Flow_L=Flow_cfs*28.32) %>% #convert cfs to liters per s
  mutate(across(all_of( 
    c('TSS_mgL', 'TP_mgL', 'DOP_mgL', 'NO2.NO3_mgL', 'NH3_mgL', 'TKN_mgL', 'Chloride_mgL')), ~ .*Flow_L)) %>% #calculate instantaneous load: #note load units in mg
  mutate(Date=as.Date(DateTime)) %>% #need a date column so can sum to daily loads
  group_by(Site_ID, Date) %>% #sum loads by date for each Site
  summarise_at(c('Flow_L', 'TSS_mgL', 'TP_mgL', 'DOP_mgL', 'NO2.NO3_mgL', 'NH3_mgL', 'TKN_mgL', 'Chloride_mgL'), sum) %>% 
  mutate(across(all_of(
    c('TSS_mgL', 'TP_mgL', 'DOP_mgL', 'NO2.NO3_mgL', 'NH3_mgL', 'TKN_mgL', 'Chloride_mgL')), ~ ./Flow_L)) %>% #divide daily load by summed daily Q
  mutate(Year=format(as.Date(Date, format="%d/%m/%Y"),"%Y")) %>% #create Year variable 
  #mutate(Flow_cfs=Flow_L/28.32) %>% #add flow back in as cfs
  filter(Flow_L>0) %>%  #exclude days with 0 flow
  mutate(PP_mgL=TP_mgL - DOP_mgL) %>% #calculate PP as TP - DOP
  filter(PP_mgL>0) %>% #omit rows where PP is negative - consider these erroneous measurements for DOP or TP
  mutate(Month=month(Date)) %>% 
  mutate(Season = case_when(
    Month==12|Month==11|Month==1~"Early Winter",
    Month >=2 & Month<=3 ~"Late Winter",
    Month >=4 & Month<=5~"Spring",
    Month >=6 & Month <=7~"Early Summer",
    Month==8|Month==9~"Late Summer",
    Month ==10~"Fall")) %>% #create seasonal attribute
  mutate(Season=factor(Season, levels=c("Early Winter", "Late Winter", "Spring", "Early Summer", "Late Summer", "Fall"))) %>% 
  mutate(Type=ifelse(grepl('-F', Site_ID), "Flume", "Tile"))#recreate flume vs tile column
head(FWC)

setwd(output_dir)
write.table(FWC, "Flow_weighted_daily_C_MN_DF.csv", sep=",", row.names=FALSE)


#Generate mean SRP values by season, for each tile outlet
#Table for manuscript
mean.seasonal<-FWC %>%
  filter(Type=="Tile") %>% 
  group_by(Site_ID, Season) %>% 
  summarise(mean.SRP.tile=round(mean(DOP_mgL),3)) %>% 
  pivot_wider(names_from=Season, values_from=mean.SRP.tile)
#View(mean.seasonal)

#Calculate Seasonal mean DOP across all sites:
#filter for Tile outlets only (not EOF monitoring locations)
mean.monthly<-FWC %>%
  filter(Type=="Tile") %>% 
  group_by(Season) %>% 
  summarise(mean.SRP.tile=round(mean(DOP_mgL),3), sd.SRP.tile=round(sd(DOP_mgL),3))
#View(mean.monthly)

#annual mean
#mean(mean.monthly$mean.SRP.tile)

#calculate mean for every site
mean.site<-FWC %>%
  filter(Type=="Tile") %>% 
  group_by(Site_ID) %>% 
  summarise(mean.SRP.tile=round(mean(DOP_mgL),3), sd.SRP.tile=round(sd(DOP_mgL),3))
#View(mean.site)

