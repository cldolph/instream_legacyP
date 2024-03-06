#Module for SRP data for additional field sites
#described in Dolph et al
#data available here: https://doi.org/10.13020/D6FH44

#set working directory
setwd(input_dir)

#read in REACH water chem data
chem<-read.table('MRB_Water_Chem_2013_2016.txt', sep="\t", header=TRUE)
head(chem)

#read in site attributes:

ws<-read.table('MRB_study_sites_watershed_area.csv', sep=",", header=TRUE)
head(ws)
names(ws)
att<-read.table('MRB_study_sites_wNHD_attributes.csv', sep=",", header=TRUE)
head(att)
names(att)

#merge attributes to water chem
chem.att<-merge(chem, ws[,c(3,6)], by=c('Site_ID'))
head(chem.att)
chem.att2<-merge(chem.att, att[,c(4,5,8,9,10,19,23,24)], by=c('Site_ID'))
head(chem.att2)

#ID levels of NHD stream Types
#FCode designations from https://hydro.nationalmap.gov/arcgis/rest/services/nhd/MapServer/layers 
levels(factor(chem.att2$FCode))
#NOTE: there are some sites with no FCode, these are lake/wetland study sites
#33400 = CONNECTOR (not sure exactly what this means?)
#33600 = CANAL/DITCH
#46006 = Stream/River - Perennial
#46003 = Stream/River - Intermittent

#Create StreamType attribute
chem.att2<-chem.att2 %>% 
  mutate(StreamType=factor(FCode, levels=c("33400", "33600", "46006", "46003"),
                           labels=c("Connector", "Ditch", "Perennial Stream/River", "Intermittent Stream/River"))) %>% 
  mutate(Date=as.Date(Date, format="%m/%d/%Y")) %>% 
  mutate(Month=month(Date)) %>% 
  mutate(Year=year(Date))
head(chem.att2)
#View(chem.att2)
nrow(chem.att2)

#Look at late summer (Aug, Sept) SRP in ditches
#Note units in original REACH dataset for SRP are ug/L!
#convert to mg/L for comparison to gages
#NOTE: want to restrict to lower flow conditions
#at least 1 Sept sampling event was very high flow
#Check EP for each sampling events from WRR P paper 
#Events to include (estimated ~50% EP or higher):
#Sept 2015 - Le Sueur
#Aug 2013 - Le Sueur
#Aug 2014 - Cottonwood, Le Sueur 
names(chem.att2)
class(chem.att2$SRP)


chem.att2 %>% 
  filter(Basin=="LS"|Basin=="CO") %>% #restrict to Le Sueur and Cottonwood
  filter(Month==8&Year==2013|Month==8&Year==2014|Month==9&Year==2015) %>% 
  group_by(StreamType, Outlet_typ) %>% 
  mutate(SRP_mgL=SRP/1000) %>% 
  summarise(MeanSRP=mean(SRP_mgL, na.rm=TRUE),n = n())

#Note that this includes some sites that were sampled repeatedly
#Can I summarize that site info? Table with number of unique sites by event
chem.att2 %>% 
  filter(Basin=="LS"|Basin=="CO") %>% #restrict to Le Sueur and Cottonwood
  filter(Month==8&Year==2013|Month==8&Year==2014|Month==9&Year==2015) %>% 
  group_by(Month, Year) %>% 
  summarise(n=n())

#Number of reach study sites 
length(unique(chem.att2$Site_ID))
summary(chem.att2$Date)


#UPDATE 11.10.23
#low flows at gages defined as lowest 25% of flows 

#Only one sampling event is close to 25% percentile rank (75% EP)
#Aug 2014 - Le Sueur
#days sampled in August:
#8/14, 8/17 8/20, 8/26
#Percent rank of flows for these dates are 19-25%, 
#so any of these dates fits the parameter of
#"low flow" < 25% of flows as currently defined 

#For REACH Field dataset, check flow rank on 8/26/2014 in the Le Sueur
#Note Discharge.rank is from main project script 'gaged_watersheds_data_processing_and_analysis_script.R'
names(Discharge.rank)
#View(Discharge.rank %>% 
#       filter(Station_name=="Le Sueur River nr Rapidan, MN") %>% 
#       filter(Date==as.Date("2014-08-14")|Date==as.Date("2014-08-17")|Date==
#                as.Date("2014-08-20")|Date==as.Date("2014-08-26")))
#Rank flows are 19-25%


#Summarise SRP by site type, during August low flows:
Event.summary<-chem.att2 %>% 
       filter(Basin=="LS") %>% #restrict to Le Sueur
       filter(Month==8&Year==2014) %>% #filter for sampling date 
       #filter(between(Date, as.Date("2014-08-03"),as.Date("2014-08-07"))) %>% 
       select(Site_ID, Date, StreamType, Outlet_typ, SRP) %>% 
       mutate(SRP_mgL=SRP/1000) %>% 
  group_by(Site_ID) %>% #take mean values by site 
  summarise(MeanSRP = mean(SRP_mgL, na.rm=TRUE), StreamType=unique(StreamType), Outlet_typ=unique(Outlet_typ)) 

#write to table so you can use in Arc GIS to filter sites:
setwd(output_dir)
write.table(Event.summary, 'REACH_sites_latesummer_lowflow_25pct.csv', sep=",", row.names=FALSE)

#fix label order for Stream Type
Event.summary$StreamType<-factor(Event.summary$StreamType, levels=c("Ditch",
                                                                    "Intermittent Stream/River",
                                                                    "Perennial Stream/River"),
                                 labels=c("Ditch", "Intermittent Stream", "Perennial Stream"))
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#Manuscript Figure: SRP for field sites, compared to tile 

ggplot(Event.summary)+
  geom_boxplot(aes(StreamType, MeanSRP))+
  geom_jitter(aes(StreamType, MeanSRP, color=Outlet_typ))+
  geom_abline(slope=0, intercept=0.03, linetype="dashed")+
  ylab("SRP (mg/L)")+
  xlab("Stream Type")+
  theme(panel.grid=element_blank())
#But remember that 'perennial stream/river could have wastewater influence!'
#might want to ID 'bigger' sites 


#################################################################################
##Compare stream SRP concentrations with tile concentrations

#Note FWC is from script: 'flow_weighted_daily_C_script.R'
#these are tile concentrations

##################################################
##Plot tile concentrations for each site in each season

#set Season for Plots
levels(FWC$Season)
Season.label<-"Late Summer"
#set intercept for mean tile SRP 
mean.tile<-0.033

#Make nicer label for tile sites (omit the "-T")
names(FWC)
FWC2<-FWC %>% 
  #mutate(Site_ID=str_remove(Site_ID, "[-T]"))
  mutate(Site_ID=gsub('-T','',Site_ID))

#Annual mean value for tile outlets
FWC2 %>% 
  filter(Type=="Tile") %>% 
  group_by(Site_ID) %>% 
  summarise(mean.SRP=mean(DOP_mgL))

FWC2 %>% 
  filter(Type=="Tile") %>% 
  summarise(mean.DOP=mean(DOP_mgL))

#function to make nicer labels
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

plotA<-ggplot(FWC2 %>% filter(Type=="Tile"&Season==Season.label))+
  geom_boxplot(aes(Site_ID, (DOP_mgL)))+
  ylim(0,1.5)+
  theme(panel.grid=element_blank(),
        axis.text.x = element_text(angle = -45, hjust=-0.1),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        axis.title=element_text(size=18),
        axis.text=element_text(size=18))+
  geom_abline(slope=0, intercept=(mean.tile), linetype="dashed")+
  xlab("Monitored tile outlets")+
  #xlab("")+
  ylab("SRP (mg/L)")
plotA

plotB<-ggplot(Event.summary)+
  geom_boxplot(aes(StreamType, MeanSRP))+
  geom_jitter(aes(StreamType, MeanSRP))+
  ylim(0,1.5)+
  geom_abline(slope=0, intercept=0.03, linetype="dashed")+
  ylab("")+
  xlab("Stream Type")+
  theme(panel.grid=element_blank(),
        axis.title=element_text(size=18),
        axis.text=element_text(size=18))+
  scale_x_discrete(breaks=levels(Event.summary$StreamType), 
                   labels=addline_format(c("Ditch", 
                                           "Intermittent Stream", "Perennial Stream")))
plotB


grid.arrange(plotA, plotB, ncol=2, nrow=1)

#Print Figures to file
setwd(output_dir)
png(
  file="./Figures/Fig6_Field_sites_LateSummer_tile_vs_riverSRP.png",
  units='in', height=7, width=15, res=300)
grid.arrange(plotA, plotB, ncol=2, nrow=1)
dev.off()