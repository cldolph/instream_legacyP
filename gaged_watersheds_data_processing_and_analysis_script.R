
#Script for analyses and figures in Dolph et al
#Title: 'Phosphorus transport in a hotter and drier Midwest: in-channel release of legacy phosphorus during summer low flow conditions' 
#Updated 2.5.24

#This script does the following:
#loads concentration-discharge data for gaged watersheds in MN
#calculates transport behavior metrics based on C-Q relationships
#identifies low flow/baseflow conditions for all gages
#calculates transport dynamics when low flow conditions are removed

#remove everything from working environment
rm(list=ls()) #if needed

#load libraries
library(tidyverse)
library(readxl)
library(purrr) 
library(janitor)
library(stringr)
library(lubridate)
library(data.table)
library(broom)
library(rebus)
library(gridExtra)
library(gt)


#########################################################################
# SET DIRECTORIES #
########################################################################
# Define the input & output directory (change as appropriate to your machine)
input_dir <- "C:/Users/dolph/OneDrive/Documents/USDA Legacy P/DF_C_Q_analysis/instream_legacyP"
output_dir <- "C:/Users/dolph/OneDrive/Documents/USDA Legacy P/DF_C_Q_analysis/instream_legacyP/output"

#########################################################################
# LOAD IN & PREPROCESS CONCENTRATION & DISCHARGE DATA FOR GAGED WATERSHEDS
#########################################################################
#Data from: https://public.tableau.com/app/profile/mpca.data.services/viz/WatershedPollutantLoadMonitoringNetworkWPLMNDataViewer/ProgramOverview [Accessed March 17, 2023]
#Note: the original dataset contains both measured concentration data (i.e., grab samples) AND modeled concentrations
#For C-Q relationships we will used only measured concentration data
#All flow data is measured 

setwd(input_dir)
Discharge<-fread("All_gages_discharge_WPLMN.csv")
names(Discharge)

#Format data
Discharge2<-Discharge %>%
  na.omit() %>% #omit all NAs
  mutate(across(c('Station number'), \(x) substr(x, 2, nchar('Station number')))) %>%   #remove first letter from station ID so can match it to C-Q data
  rename(SRP="Daily Conc. (mg/L)", Flow_cfs="Flow (cfs)", Station_name="Station", Date="Sample date",
         Data_type="Data type", Station_number="Station number") %>% 
  mutate(Date=as.Date(as.character(Date), "%m/%d/%Y")) %>% 
  mutate_at(c('SRP', 'Flow_cfs'), as.numeric) %>%  #convert columns to numeric
  mutate_at(c('Station_number'), as.character) %>% #make sure station is character ID
  select(Station_name, SRP, Flow_cfs, Date, Month, Year, Data_type, Station_number)

#Calculate percentile rank for all for flows for each gage:
#(this is needed later for identifying low flow conditions)
Discharge.rank<-Discharge2 %>% 
  group_by(Station_name) %>% 
  mutate(percent_rank=rank(Flow_cfs)/length(Flow_cfs))
names(Discharge.rank)

#Set threshold for percent rank of flows that are considered "low flow"
Discharge.rank$lowflowpoint<-ifelse(Discharge.rank$percent_rank<=0.25, 'yes', 'no')

#Pre-process concentration-discharge data
#select only measured concentration data 
#note units for SRP are mg/L
#restrict to sites for which watershed areas have been delineated

#convert to wide format and select P data:
names(Discharge.rank)
levels(factor(Discharge.rank$Data_type))
conc<-Discharge.rank %>% 
  filter(Data_type=="discrete sample point") %>% 
  na.omit() %>% #remove NAs from all columns
  filter(Flow_cfs>0) %>% #remove records where Flow is 0
  filter(SRP>0) %>% #remove records where SRP is 0
  mutate(Month=month(Date)) %>% 
  mutate(Season = case_when(
    Month==12|Month==11|Month==1~"Early Winter",
    Month >=2 & Month<=3 ~"Late Winter",
    Month >=4 & Month<=5~"Spring",
    Month >=6 & Month <=7~"Early Summer",
    Month==8|Month==9~"Late Summer",
    Month ==10~"Fall")) %>% #create seasonal attribute
  add_count(Station_name) %>%  #count number of records for each gage
  filter(n>=20) #restrict to sites with at least 20 samples
nrow(conc)

#Calculate geometric mean of flow for each gage (based on total flow record for each gage)
#for geometric mean function, use psych package
library(psych)
names(Discharge.rank)
QGM<-Discharge.rank %>% 
  select(Station_number, Flow_cfs) %>% 
  filter(!Flow_cfs==0) %>% #omit zero values
  group_by(Station_number) %>% 
  summarise(QGM=geometric.mean(Flow_cfs))
head(QGM)

#Normalize flow data
gage_normalized<-merge(conc, QGM, by=c('Station_number'))
levels(factor(gage_normalized$Station_name)) #check number of sites 
names(gage_normalized)
gage_normalized<- gage_normalized %>% 
  mutate(normalized_flow=Flow_cfs/QGM)
head(gage_normalized)

levels(factor(gage_normalized$Station_number))
nrow(gage_normalized)
#########################################################################
# MERGE CONCENTRATION-DISCHARGE DATA TO IDENTIFYING SPATIAL INFORMATION #
#########################################################################
#Merge concentration-discharge data for gaged watersheds
#to delineated watershed areas and NHDv2Plus catchment IDs (COMID)
#see Dolph et al for more details on watershed delineations and NHDv2Plus

#NHDv2Plus COMID and watershed delineations were assigned in ArcGIS Pro
#NHDv2Plus data obtained from: https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data

#Catchment IDs (COMIDs) are used to merge to US EPA StreamCat attributes

#read in gage identifying info (processed in ArcGIS Pro)
setwd(input_dir)
gages<-read.table("Gages_watershed_NHD_ID.csv", sep=",", header=TRUE)

#format Station number for correct matching
gages2<-gages %>% 
mutate(Station_number=sprintf("%08.0f", Station_number)) #pad Station  Number with leading 0s as needed; need 8 digits total

#check # number of gages:
levels(factor(gages2$Station_number)) #ok 144 gages

#look at range in watershed size across gaged watersheds
summary(gages$Area_Km2)

#merge gage COMID to lowflow water chem
CQ.att<-merge(gage_normalized, gages2 %>% select(-Site_name), by=c('Station_number'))
names(CQ.att)
levels(factor(CQ.att$Station_name)) ##check number of gages
nrow(CQ.att)
####################################################################
#LOAD ADDITIONAL GEOSPATIAL ATTRIBUTES & MERGE TO C-Q DATA
####################################################################

# First: run model to download and pre-process StreamCat attributes
#Note: StreamCat available from https://www.epa.gov/national-aquatic-resource-surveys/streamcat-dataset [accessed on 9/26/23]
#takes ~ 2.5 minutes
#generates an intermediate output file: 'MN_StreamCat_attributes.csv'

setwd(input_dir)
source('module_preprocess_StreamCat.R')

#if you have already run the stream module above, load in attributes directly:
#streamcat<-read.table('MN_StreamCat_attributes.csv', sep=",", header=TRUE)
names(streamcat) #look at all attributes

#Merge to C-Q data: 
CQ.att2<-merge(CQ.att, streamcat, by=c('COMID'))
nrow(CQ.att2) #check number of records
levels(factor(CQ.att2$Station_name)) #check number of sites

#load in tile density data
#Note: tile density was estimated as described in Dolph et al 
setwd(input_dir)
tile<-read.csv('Gaged_watersheds_tile_density.csv', sep=",", header=TRUE)

#subset to relevant columns
tile2<-tile %>% 
  select(station_ALL_clean, name_ALL_clean, Area_Km2, Tile_density) %>% #select relevant columns
  mutate(station=sprintf("%08.0f", tile$station_ALL_clean)) %>% #pad with leading 0s as needed
  mutate(Station_number=as.character(station))  #make station number a character so will match correctly later

#merge tile density data to CQ data
CQ.att3<-merge(CQ.att2, tile2, by=c('Station_number'))
levels(factor(CQ.att3$Station_name)) #check number of sites


#identify sites with substantial human impacts - >50% ag or >10% high intensity urban
#as described in Dolph et al 
CQ.att3$Impacted<-ifelse(CQ.att3$PctCrop2019Ws>=0.5|CQ.att3$PctUrbHi2019Ws>=0.1, "Impacted", "Less Impacted")

#Create attribute to differentiate between low/no WWTP sites and sites with higher WWTP density
#as described in Dolph et al
CQ.att3<-CQ.att3 %>% 
  mutate(WWTP_none=ifelse(WWTPAllDensWs==0, "No WWTP", "WWTP present")) %>% 
  mutate(WWTP_lim=ifelse(WWTPAllDensWs<0.005, "Limited", "Higher")) %>% 
  mutate(WWTP_category=WWTP_lim) %>% 
  mutate(WWTP_category=ifelse(WWTPAllDensWs==0, "None", WWTP_category))
nrow(CQ.att3)
View(head(CQ.att3))
#####################################################################
# PLOT SRP CONCENTRATIONS DURING LOW FLOW, BY SEASON #
#Figure for Manuscript

#format Season label in season order
CQ.att3$Season<-factor(CQ.att3$Season, levels=c("Early Winter", "Late Winter", "Spring", "Early Summer", "Late Summer", "Fall"))


ggplot(CQ.att3 %>% filter(lowflowpoint=="yes"))+
  geom_boxplot(aes(Season, SRP))+
  facet_wrap(~Impacted)+
  theme(panel.grid=element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle=90, hjust=1))+
  ylab("SRP (mg/L")

#Look at mean SRP for impacted and less impacted, by season
View(CQ.att3 %>% filter(lowflowpoint=="yes") %>% 
  #filter(Impacted=="Less Impacted") %>%  #option to look only at impacted sites
  group_by(Impacted, Season) %>% 
  summarise(meanSRP=round(mean(SRP), 4), maxSRP=max(SRP), minSRP=min(SRP)))

#######################################################################
# COMPUTE STATS FOR C-Q REGERSSIONS BEFORE AND AFTER HOLDING OUT LOW FLOWS #
#separately for each gage

###
#First calculate C-Q regression with all flow data for each gage:
CQ.all<-CQ.att3
names(CQ.all)

regressions <- CQ.all %>%
  nest(data = -Station_name) %>%
  mutate(
    fit = map(data, ~ lm(log10(SRP) ~ log10(normalized_flow), data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

GageResults<-regressions %>%
  unnest(tidied)

#info on what tidy(lm object) returns:
#https://broom.tidymodels.org/reference/tidy.lm.html

#Return R2 value:
GageStats<-regressions %>%
  unnest(glanced)
#View(GageStats)

#Create a table with Gage name slope, p value, R2, n 
Stats1<-GageResults %>% 
  filter(term=="log10(normalized_flow)") %>% 
  #filter(p.value<0.05) %>% #don't want to restrict to only sig relationships, in case significance changes when hold out later summer 
  select(Station_name, estimate, statistic, p.value, glanced)
names(Stats1)

#unnest glanced stats (R2 values, etc)
#https://mpn.metworx.com/packages/tidyr/1.0.3/reference/hoist.html
Stats2<-Stats1 %>% unnest_wider(glanced, names_repair="universal")
Stats2<-Stats2 %>% 
  select(1,2,3,4,5,16) %>% 
  rename(Slope="estimate", T.stat="statistic...3", p="p.value...4", n="nobs")
head(Stats2)


###
#Recalculate C-Q relationships after witholding low flow or seasonally low flow samples:

reg.nolow <-CQ.all %>% 
  filter(!SRP==0) %>% 
  filter(!Flow_cfs==0) %>% 
  filter(!(lowflowpoint=="yes"&Season=="Late Summer"))%>% #hold out lowflow samples - seasonally or all)
  #filter(!(lowflowpoint=="yes"&Season=="Fall")) %>% 
  nest(data = -Station_name) %>%
  mutate(
    fit = map(data, ~ lm(log10(SRP) ~ log10(normalized_flow), data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )
nrow(reg.nolow) #one gage is lost 
GageRes.nolow<-reg.nolow %>%
  unnest(tidied)

Stats.nolow<-GageRes.nolow %>% 
  filter(term=="log10(normalized_flow)") %>% 
  #filter(p.value<0.05) %>% 
  select('Station_name', 'estimate', 'statistic', 'p.value', 'glanced')

#unnest glanced stats (R2 values, etc)
Stats2.nolow<-Stats.nolow %>% unnest_wider(glanced, names_repair="universal")

Stats2.nolow<-Stats2.nolow %>% 
  select(1,2,3,4,5,16) %>% 
  rename(Slope="estimate", T.stat="statistic...3", p="p.value...4", n="nobs")

###
#Compare regression with and without (seasonal) low flows: 

#Merge the two regression datasets
Stats.compare.LOW<-merge(Stats2, Stats2.nolow, by=c("Station_name"))
nrow(Stats.compare.LOW) #check number of sites

#Flag sites where p got lower and slope increased when summer low flow was excluded
Stats.compare2.LOW<-Stats.compare.LOW %>% 
  mutate(slope_change=ifelse(Slope.y>Slope.x, "Yes", "No")) %>%
  mutate(p_change=ifelse(p.y<p.x, "Yes", "No")) %>% 
  mutate(r2_change=ifelse(r.squared.y>r.squared.x, "Yes", "No")) %>% 
  mutate(Sig1=ifelse(p.x<0.05, "Yes", "No")) %>% 
  mutate(Sig2=ifelse(p.y<0.05, "Yes", "No")) %>% 
  rename(Slope1=Slope.x, Tstat1=T.stat.x, p1=p.x, R2v1=r.squared.x, n1=n.x, 
         Slope2=Slope.y, Tstat2=T.stat.y, p2=p.y, R2v2=r.squared.y, n2=n.y) %>%  #clean up column names 
  mutate(Pct_slope_change=((Slope2-Slope1)/Slope1)*100) #Add column for percent change in slope

#Match back to station ID 
#for matching in GIS to gage locations
Stats.compare2.LOW.ID<-merge(Stats.compare2.LOW, 
                             unique(CQ.all[,c("Station_number", "Station_name")]), 
                             by=c("Station_name"))
   
#fix names
head(Stats.compare2.LOW.ID)
#View(Stats.compare2.LOW.ID)

#############################################################
# LOOK AT C-Q PLOTS AND TRENDLINE WITH AND WITHOUT SUMMER LOWFLOW #

names(CQ.all)

#Figure for Manuscript 

#color options for plot: 
library(RColorBrewer)

#list of all sites
levels(factor(CQ.all$Station_name))

#Sites for paper figure showing example C-Q relationships before and after holding out summer low flows:                                                    
"Big Fork River at Big Falls, MN"
"Little Beauford Ditch nr Beauford, MN22"
"Le Sueur River at St. Clair, CSAH28"
"Pomme de Terre River nr Hoffman, CR76"

#site.list<-c("Big Cobb River nr Beauford, CSAH16",
#             "Little Beauford Ditch nr Beauford, MN22",
#            "Le Sueur River at St. Clair, CSAH28",
#            "East Branch Chippewa River nr Benson, CR78")

#Alternative site list (no NPDES or WWTP impacts):
site.list<-c("Shakopee Creek nr Benson, 20th Ave SW",
             "Little Beauford Ditch nr Beauford, MN22", #**big difference
             #"Redwood River at Russell, CR15",
             "High Island Creek nr Arlington, CR9",
             "Mustinka River nr Norcross, MN9") #**big difference

#Or, select single site to plot
#site.list<-c("Buffalo Creek nr Glencoe, CSAH1")

plotA<-ggplot()+
  geom_point(data=(CQ.all %>% filter(Station_name %in% site.list)),
             aes(log10(normalized_flow), log10(SRP), color=Season), size=2) +
  geom_smooth(data=(CQ.all %>% filter(Station_name %in% site.list)),
              aes(log10(normalized_flow), log10(SRP)), method=lm, se=FALSE, color="black")+
  geom_point(data=(CQ.all %>% 
                     filter(Station_name %in% site.list) %>% 
                     filter(!(lowflowpoint=="yes"))),
             aes(log10(normalized_flow), log10(SRP)), size=2, color="gray") +
  geom_smooth(data=(CQ.all %>% filter(Station_name %in% site.list) %>% 
                      filter(!(lowflowpoint=="yes"&Season=="Late Summer")) %>% 
                      filter(!(lowflowpoint=="yes"&Season=="Fall"))),
              aes(log10(normalized_flow), log10(SRP)), method=lm, color="black", linetype=3, se=FALSE)+
  #ylim(-3, 0)+
  #xlim(-2.1, 2.5)+
  ylab("Log10 SRP (mg/L")+
  xlab("Log10 Q/Qgm")+
  facet_wrap(~Station_name)+
  scale_color_manual(values = c("#edf8fb","#bfd3e6",
                                "#9ebcda",
                                "#8c96c6",
                                "#8856a7",
                                "#810f7c"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_abline(slope=0, intercept=log10(0.03))
#geom_abline(slope=0, intercept=log10(0.09))
plotA

#Print Figure to file 
setwd(output_dir)
jpeg(
  file="./Figures/Examples_CQ_change.jpeg",
  units='in', height=6.5, width=8, res=300)
plotA
dev.off()

###############################################################
# ADD IN CVc/CVq TO CHARACTERIZE TRANSPORT BEHAVIOR #

#Daily data available from WQPLM data viewer
#you can get daily flow on the daily tab, if you include modeled points
#have to select at least 1 water quality attribute 
#selecting all gages

#Load in discharge data (If not already loaded above)
setwd(input_dir)
Discharge<-fread("All_gages_discharge_WPLMN.csv")

Discharge<-Discharge %>%
  na.omit() %>% #omit all NAs
  mutate(across(c('Station number'), substr, 2, nchar('Station number'))) %>%   #remove first letter from station ID
  rename(SRP="Daily Conc. (mg/L)", Flow_cfs="Flow (cfs)", Site_name="Station", Date="Sample date",
         Data_type="Data type", station="Station number") %>% 
  mutate(Date=as.Date(as.character(Date), "%m/%d/%Y")) %>% 
  mutate_at(c('SRP', 'Flow_cfs'), as.numeric) %>%  #convert columns to numeric
  mutate_at(c('station'), as.character) %>% #make sure station is character ID
  select(Site_name, SRP, Flow_cfs, Date, Month, Year, Data_type, station)

#Note that this includes modeled SRP values!
#Need to restrict to measured SRP values for calculation of mean C (see below)

#CVc/CVq= mean(Q)*stdev(C)/mean(C)*stdev(Q)

#Calculate mean & sd of Flow
#note: Ok to include modeled and discrete points, all Q values are measured
CVQ<-Discharge %>% 
  group_by(station) %>% 
  summarise(Flow.mean=mean(Flow_cfs),
            Flow.SD=sd(Flow_cfs)) 
head(CVQ)

#Calculate mean & sd of SRP
#restrict to measured points (aka "discrete sample point", omit modeled values)
CVC<-Discharge %>% 
  filter(Data_type=="discrete sample point") %>% 
  group_by(station) %>% 
  summarise(SRP.mean=mean(SRP),
            SRP.SD=sd(SRP)) 
head(CVC)

#merge flow and concentration data
CVcCVq<-merge(CVQ, CVC, by="station")
head(CVcCVq)

CVcCVq<-CVcCVq %>% 
  mutate(CVc.CVq=(Flow.mean*SRP.SD)/(SRP.mean*Flow.SD)) %>% 
  rename(Station_number=station)
head(CVcCVq)
summary(CVcCVq$CVc.CVq)

#merge to C-Q regression stats
names(CVcCVq)
names(Stats.compare2.LOW.ID)
Transport.stats<-merge(Stats.compare2.LOW.ID, CVcCVq, by=c("Station_number"))
levels(factor(Transport.stats$Station_name)) #check number of sites
#View(Transport.stats)

#Can make a factor for transport regime (see P paper)
Transport.stats2<-Transport.stats %>% 
  mutate(Behavior=ifelse(p1<0.05&Slope1>0, "mobilizing", NA)) %>% 
  mutate(Behavior=ifelse(p1<0.05&Slope1<0, "diluting", Behavior)) %>% 
  mutate(Behavior=ifelse(CVc.CVq <=0.3, "chemostatic", Behavior)) %>% 
  mutate(Behavior=ifelse(p1>=0.05 & CVc.CVq >0.3, "chemodynamic", Behavior)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% #round all numeric columns to 3 decimal places
  select(Station_name, Behavior, CVc.CVq, Slope1, p1, R2v1, n1, 
         Slope2, p2, R2v2, n2, Pct_slope_change) 
#additional formattign for gt table if desired:
#%>%   #change column order for select columns
  #rename("% slope change" = Pct_slope_change, "Site name" =Station_name,
      #   "CVc/CVq"=CVc.CVq, "Slope(1)"=Slope1, "p(1)"=p1, "R2(1)"=R2v1,
      #   "Slope(2)"=Slope2, "p(2)"=p2, "R2(2)"=R2v2) %>%  #make nicer column headings %>% 
  #gt()

#summary of key stats
Transport.stats2 %>% 
  filter(Behavior=="mobilizing") %>% 
  summarise(meanR2=mean(R2v1), minR2=min(R2v1), maxR2=max(R2v1))

#Write table for Appendix
#Transport behavior + CQ regression stats with and without late summer low flows
setwd(output_dir)
write.table(Transport.stats2, "CQ_stats_beforeafter_LOWFLOW25pct_wTransportBehavior_noLateSummer.csv", sep=",", row.names = FALSE)

###Plot slope (b) vs CVcCVq (Appendix Figure)
cbbPalette <- c("#E69F00","#D55E00", "#56B4E9","#000000", "#F0E442", "#CC79A7", "#009E73","#0072B2")

Transport.plot<-ggplot(Transport.stats)+
  geom_point(aes(CVc.CVq, Slope1, colour = Behavior))+
  scale_colour_manual(values=cbbPalette)+
  ylab(bquote('Parameter "b" of log-log c-Q relationship'))+
  labs(x=expression(CV[C]/CV[Q]))+
  #geom_vline(xintercept=0.3, linetype="dashed")+
  theme_bw()+
  theme(panel.grid=element_blank())
Transport.plot

#For paper - Check number of sites where slope is stronger after holding out low flows:
names(Transport.stats)
Transport.stats %>% 
  filter(Slope1>0&Slope2>Slope1&p2<0.05) %>% 
  #count()
  summarise(meanslopechange=mean(Pct_slope_change), minslopechange=min(Pct_slope_change),
            maxslopechange=max(Pct_slope_change))

#Print Figure to file 
setwd(output_dir)
jpeg(
  file="./Figures/Transport_behavior.jpeg",
  units='in', height=6, width=8, res=300)
Transport.plot
dev.off()

#############################################################################
# CALCULATE MEAN SRP DURING SEASONAL LOW FLOW CONDITIONS FOR GAGED WATERSHEDS #
#Table 2 in manuscript
#Note adjustments for calculating mean and sd in long form for further analysis below

#Set season factor order
CQ.all$Season<-factor(CQ.all$Season, levels=c("Early Winter", "Late Winter", "Spring", "Early Summer", "Late Summer", "Fall"))

#View(CQ.all) 

Gage.lowflow.summary<-CQ.all %>% 
  filter(!SRP==0) %>% 
  filter(!Flow_cfs==0) %>% 
  filter(lowflowpoint=="yes") %>%
  filter(!is.na(SRP)) %>% 
  group_by(Station_number, Season) %>% 
  summarise(mean.SRP=round(mean(SRP), 3), sd.SRP=sd(SRP))# %>% 
  #pivot_wider(names_from = Season, values_from = c(mean.SRP)) #convert to wide form for manuscript 
#View(Gage.lowflow.summary)                                                              #Table AND omit SD

#Note that some gages only have 1 value for each season and therefore no sd!

levels(factor(CQ.all$Station_number)) #144 sites
levels(factor(Gage.lowflow.summary$Station_number)) #only 143 sites 

#Find sites that are lost
unique(CQ.all[!CQ.all$Station_number %in% Gage.lowflow.summary$Station_number,c(1,3)])

#Sites to take out (because of insuffient SRP samples in all seasons):
#Kettle River nr Willow River, Long Lake Rd (35051002)

#Add transport behavior to season mean SRP 

Lowflow.sum.transport<-merge(Gage.lowflow.summary, Transport.stats %>% select(Station_name, Station_number, Behavior), by=c("Station_number"))
levels(factor(Lowflow.sum.transport$Station_name)) 

#####################################################################
#MEAN SRP and/or TRANSPORT STATS IN RELATION TO PREDICTOR VARIABLES #

#Merge attributes to mean lowflow SRP (via COMID from gages2, loaded above)
names(gages2)
names(Lowflow.sum.transport)
gage.lowflow<-merge(Lowflow.sum.transport, (gages2 %>% select(Station_number, COMID)), by=c('Station_number'))
names(gage.lowflow)
levels(factor(gage.lowflow$Station_name)) ##check number of sites

#merge mean lowflow SRP to streamcat attributes (loaded above)
lowflow.att<-merge(gage.lowflow, streamcat, by=c('COMID'))
levels(factor(lowflow.att$Station_name)) #check number of sites

#merge mean lowflow SRP to tile data
lowflow.att2<-merge(lowflow.att, tile2, by=c('Station_number'))
levels(factor(lowflow.att2$Station_name)) #check number of sites

#separate sites with substantial human impacts - >50% ag or >10% high intensity urban?
#lowflow.att2$Impacted<-ifelse(lowflow.att2$PctCrop2019Ws>=0.5|lowflow.att2$PctUrbHi2019Ws>=0.1, "Impacted", "Less Impacted")

#create attribute for degree of WWTP influence
#lowflow.att2<-lowflow.att2 %>% 
#  mutate(WWTP_none=ifelse(WWTPAllDensWs==0, "No WWTP", "WWTP present")) %>% 
#  mutate(WWTP_lim=ifelse(WWTPAllDensWs<0.005, "Limited", "Higher"))
 
#look at sites with high WWTP influence
lowflow.att2 %>% filter(WWTPAllDensWs>0.005) %>% 
  select(Station_name) %>% 
  unique() %>% 
  arrange(Station_name)

names(lowflow.att2)
                                                                                     
#write table for Appendix
#Seasonal mean SRP for each gage + WWTP influence
setwd(output_dir)
write.table(lowflow.att2 %>% 
              select(Station_name, Season, mean.SRP, Impacted, WWTPAllDensWs) %>% 
              pivot_wider(names_from = Season, values_from = mean.SRP),
            "Lowflow_meanSRP_by_gage_and_Season_with_WWTPinfo.csv", sep=",", row.names=FALSE)

#Write table with all attributes, for use in regression models:
setwd(output_dir)
write.table(lowflow.att2, "Lowflow_meanSRP_by_gage_and_Season_with_attributes.csv", sep=",")

###########################################################
# PLOT SRP IN RELATION TO SELECT ATTRIBUTES #

#Need season in long form for faceting (if left in wide form from Table 2, otherwise not needed)
#if not needed an set lowflow.att3 to lowflow.att2
lowflow.att3<-lowflow.att2
levels(factor(lowflow.att3$Station_name))

#lowflow.att3<-lowflow.att2 %>% 
#pivot_longer(
#  cols = c("Early Winter", "Late Winter", "Spring", "Early Summer", "Late Summer", "Fall"),
#  names_to = "Season",
#  values_to = "mean.SRP")
#names(lowflow.att3)
#head(lowflow.att3[,c(3,297,298)])

#format season labels

lowflow.att3$Season=factor(lowflow.att3$Season, levels=c("Early Winter",
                                                         "Late Winter", "Spring",
                                                         "Early Summer", "Late Summer", 
                                                         "Fall"))

#Manuscript Figure (Wastewater)
#View(lowflow.att3)

WWTP.SRP.plot<-ggplot(lowflow.att3)+
  geom_point(aes(WWTPAllDensWs, log(mean.SRP)))+
  #geom_text(aes(WWTPAllDensCat, log(mean.SRP), label=Site_name))+
  #only had trendlines where significant:
  geom_smooth(data=lowflow.att3 %>% filter(!Season=="Spring"), aes(WWTPAllDensWs, log(mean.SRP)), method="lm", se=FALSE)+
  #xlim(0,0.005) +#see effect when sites with higher density of point discharges removed
  facet_wrap(~Season)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  #xlab(bquote(Density of wastewater treatment plants (sites/km^2))+
  #xlab(expression(Channel~Density~(km/km^2))+
  #          labs(y = bquote(Total~density~(Individuals%.%L^-1)))+
  labs(x=bquote(Density~of~wastewater~treatment~plants~(sites/km^2)))+        
  ylab("log10 SRP (mg/L)")+
  geom_vline(xintercept=0.005, linetype="dashed")
WWTP.SRP.plot

#Print Figure to file
setwd(output_dir)
jpeg(
  file="./Figures/SRP_vs_WWTP.jpeg",
  units='in', height=6, width=9, res=300)
WWTP.SRP.plot
dev.off()

#significant relationship with lowflow SRP and wastewater discharges:
#Early Winter, Late Winter, Early Summer, Late Summer, Fall
#Spring, not significant

#relationship between seasonal lowflow SRP and WWTP, with and without high Pt Discharge sites
summary(with(lowflow.att3 %>% filter(Season=="Early Winter"), lm(log(mean.SRP)~WWTPAllDensWs)))
summary(with(lowflow.att3 %>% filter(Season=="Early Winter"&WWTPAllDensWs<0.005), lm(log(mean.SRP)~WWTPAllDensWs)))

summary(with(lowflow.att3 %>% filter(Season=="Late Winter"), lm(log(mean.SRP)~WWTPAllDensWs)))
summary(with(lowflow.att3 %>% filter(Season=="Late Winter"&WWTPAllDensWs<0.005), lm(log(mean.SRP)~WWTPAllDensWs)))

summary(with(lowflow.att3 %>% filter(Season=="Spring"), lm(log(mean.SRP)~WWTPAllDensWs)))
summary(with(lowflow.att3 %>% filter(Season=="Spring"&WWTPAllDensWs<0.005), lm(log(mean.SRP)~WWTPAllDensWs)))

summary(with(lowflow.att3 %>% filter(Season=="Early Summer"&!mean.SRP==0), lm(log(mean.SRP)~WWTPAllDensWs)))
summary(with(lowflow.att3 %>% filter(Season=="Early Summer"&!mean.SRP==0&WWTPAllDensWs<0.005), lm(log(mean.SRP)~WWTPAllDensWs)))

summary(with(lowflow.att3 %>% filter(Season=="Late Summer"), lm(log(mean.SRP)~WWTPAllDensWs)))
summary(with(lowflow.att3 %>% filter(Season=="Late Summer"&WWTPAllDensWs<0.005), lm(log(mean.SRP)~WWTPAllDensWs)))

summary(with(lowflow.att3 %>% filter(Season=="Fall"), lm(log(mean.SRP)~WWTPAllDensWs)))
summary(with(lowflow.att3 %>% filter(Season=="Fall"&WWTPAllDensWs<0.005), lm(log(mean.SRP)~WWTPAllDensWs)))


#Ag land use impacts on low flow SRP: Cropland and Manure inputs
plot1<-ggplot(lowflow.att3) +
#ggplot(lowflow.att3 %>% filter(WWTPAllDensWs==0))+
  #geom_point(aes(ManureWs, log(mean.SRP),colour=WWTPAllDensWs), size=3)+
  #geom_smooth(data=lowflow.att3, aes(ManureWs, log(mean.SRP)), method="lm", se=FALSE)+
  geom_point(aes(PctCrop2019Ws, log(mean.SRP), colour=WWTPAllDensWs), size=3)+
  #only add trendlines where significant:
  geom_smooth(data=lowflow.att3, aes(PctCrop2019Ws, log(mean.SRP)), method="lm", se=FALSE)+
  facet_wrap(~Season)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  xlab("% cropland")+
  #xlab("Mean manure application rate (kg N/ha/yr)")+
  ylab("log10 SRP (mg/L)")+
  scale_color_gradient(low="orange", high="green")+
  labs(colour=bquote(Density~of~wastewater~treatment~plants~(sites/km^2)))+
  theme(legend.position="top")
plot1

#Print Figure to file
setwd(output_dir)
jpeg(
  file="./Figures/SRP_vs_PctCrop_alldata.jpeg",
  units='in', height=6, width=9, res=300)
plot1
dev.off()

#relationship between seasonal lowflow SRP and crop cover, with and without highly WWTP influenced sites 
summary(with(lowflow.att3 %>% filter(Season=="Fall") %>% filter(!mean.SRP==0), lm(log(mean.SRP)~PctCrop2019Ws)))
#without high Pt Discharge sites
summary(with(lowflow.att3 %>% filter(Season=="Fall"&WWTPAllDensWs==0&!mean.SRP==0), lm(log(mean.SRP)~PctCrop2019Ws)))

plot2<-ggplot(lowflow.att3 %>% filter(WWTPAllDensWs==0)) +
  #ggplot(lowflow.att3 %>% filter(WWTPAllDensWs==0))+
  geom_point(aes(PctCrop2019Ws, log(mean.SRP)), size=3, color="orange")+
  #only had trendlines where significant:
  geom_smooth(data=lowflow.att3 %>% filter(WWTPAllDensWs==0), aes(PctCrop2019Ws, log(mean.SRP)), method="lm", se=FALSE)+
  facet_wrap(~Season)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  xlab("% cropland")+      
  ylab("log10 SRP (mg/L)")+
  #scale_color_gradient(low="orange", high="green")+
  labs(colour=bquote(Density~of~wastewater~treatment~plants~(sites/km^2)))+
  ggtitle("Only sites without wastewater treatment plants")+
  theme(plot.title = element_text(hjust = 0.5, size=12))
plot2

#Print Figure to file 
setwd(output_dir)
jpeg(
  file="./Figures/SRP_vs_PctCrop_NoWWTP.jpeg",
  units='in', height=6, width=9, res=300)
plot2
dev.off()


#############################################################################
# COMPARE MEAN SEASONAL LOW FLOW SRP FROM GAGED WATERSHEDS TO TILE SRP # 
#Figure for manuscript 

#Load and preprocess tile WQ monitoring data from MN Discovery Farms
setwd(input_dir)
source('module_tile_WQ_data.R')

#this produces an output called FWC (for flow weighted mean concentrations from tile) 

##Plot tile concentrations for each site in each season
#Note: select season manually by specifying 'Season.label' to produce plot for each season

#for plot formatting make a dummy label that matches format of tile labels
#(This CQ.att3 is from legacy_P_script_clean_11_8_23.R)
names(CQ.att3)
levels(factor(CQ.att3$Impacted))
CQ.att3<-CQ.att3 %>% 
  mutate(DummyID=substr(Station_name,1,7))

#set Season for Plots
Season.label<-"Early Winter"
#set intercept for mean tile SRP for appropriate Season
mean.tile<-0.03

plotA<-ggplot(FWC %>% filter(Type=="Tile"&Season==Season.label))+
  geom_boxplot(aes(Site_ID, (DOP_mgL)))+
  ylim(0,1.25)+
  theme(panel.grid=element_blank(),
        #axis.text.x = element_text(angle = -45, hjust=-0.1),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_text(size=18),
        axis.text.y=element_text(size=18))+
  geom_abline(slope=0, intercept=(mean.tile))+
  #xlab("Monitored tile outlets")
  ylab("SRP (mg/L)")
plotA

plotB<-ggplot(CQ.att3 %>% 
                filter(Season==Season.label&lowflowpoint=="yes"))+
  geom_boxplot(aes(DummyID, (SRP), fill=WWTP_category, color=WWTP_category))+
  ylim(0,1.25)+
  theme(panel.grid=element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        #axis.text.x=element_text(angle = -45, hjust=-0.1, color="white"), 
        axis.text.y=element_text(size=18),
        #legend.position="none")+
  legend.position=c(.225,.875),
  legend.title=element_text(size=18),
  legend.text = element_text(size=18))+
  geom_abline(slope=0, intercept=mean.tile)+
  scale_fill_manual(values=c("#E69F00", "#56B4E9","#D55E00"))+
  scale_color_manual(values=c("#E69F00", "#56B4E9","#D55E00"))+
  guides(fill = guide_legend(title="WWTP influence"), 
         color = guide_legend(title="WWTP influence"))+
  #ylab("SRP (mg/L)")+
  #xlab("Gaged watersheds")+
  ylab("")

grid.arrange(plotA, plotB, ncol=2, nrow=1)

#Print Figures to file
setwd(output_dir)
jpeg(
  file="./Figures/EarlyWinter_tile_vs_riverSRP.jpeg",
  units='in', height=6, width=12, res=300)
grid.arrange(plotA, plotB, ncol=2, nrow=1)
dev.off()

#Check number of outliers removed with shorter yaxis (limited to 0,1.25)
CQ.att3 %>% 
  filter(lowflowpoint=="yes") %>% 
  filter(SRP>1.25) %>% 
  nrow()#9 samples total
nrow(CQ.att3 %>% filter(lowflowpoint=="yes"))
FWC %>% 
  filter(DOP_mgL>1.25) %>% 
  nrow()
nrow(FWC)


#Count number of gaged watersheds with mean SRP above mean tile SRP concentration, for each season

#mean tile concentrations from Tile module:
mean.monthly

#merge tile thresholds to CQ data for gaged watersheds
names(lowflow.att3)
lowflow.att4<-merge(lowflow.att3, mean.monthly, by=c("Season"))
View(lowflow.att4 %>% 
  select(Season, mean.SRP, mean.SRP.tile) %>% 
  group_by(Season) %>% 
  mutate(rows_above_tile=sum(round(mean.SRP, 3)>round(mean.SRP.tile,3))) %>% 
  select(Season, rows_above_tile) %>% 
  unique())

#Check in the context of WWTP
#figure out how many sites with mean SRP higher than mean tile have strong WWTP influence
View(lowflow.att4 %>% 
  filter(WWTPAllDensWs > 0.005) %>% 
  select(Season, mean.SRP, mean.SRP.tile) %>% 
  group_by(Season) %>% 
  mutate(rows_above_tile=sum(round(mean.SRP,3)>round(mean.SRP.tile,3))) %>% 
  select(Season, rows_above_tile) %>% 
  unique())





