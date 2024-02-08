#Module for loading StreamCat attributes
#Updated: 2.5.24

#watershed and catchment scale variables from U.S. EPA StreamCat
#https://www.epa.gov/national-aquatic-resource-surveys/streamcat-dataset

#set working directory
setwd(input_dir)

start.time<-Sys.time()

#Scrape StreamCat Files from FTP site:
download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/CanalDensity_MN.zip", destfile = "CanalDensity_MN.zip")
unzip("CanalDensity_MN.zip")
CanalDensityMN<-read.csv("CanalDensity_MN.csv")
head(CanalDensityMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/Elevation_MN.zip", destfile = "Elevation_MN.zip")
unzip("Elevation_MN.zip")
ElevationMN<-read.csv("Elevation_MN.csv")
head(ElevationMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/NLCD2019_MN.zip", destfile = "NLCD2019_MN.zip")
unzip("NLCD2019_MN.zip")
NLCD2019MN<-read.csv("NLCD2019_MN.csv")
head(NLCD2019MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/NLCD2019RipBuf100_MN.zip", destfile = "NLCD2019RipBuf100_MN.zip")
unzip("NLCD2019RipBuf100_MN.zip")
NLCD2019RipBuf100MN<-read.csv("NLCD2019RipBuf100_MN.csv")
head(NLCD2019RipBuf100MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/AgMidHiSlopes_MN.zip", destfile = "AgMidHiSlopes_MN.zip")
unzip("AgMidHiSlopes_MN.zip")
AgMidHiSlopesMN<-read.csv("AgMidHiSlopes_MN.csv")
head(AgMidHiSlopesMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/ImperviousSurfaces_MN.zip", destfile = "ImperviousSurfaces_MN.zip")
unzip("ImperviousSurfaces_MN.zip")
ImperviousSurfacesMN<-read.csv("ImperviousSurfaces_MN.csv")
head(ImperviousSurfacesMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/ImperviousSurfacesRipBuf100_MN.zip", destfile = "ImperviousSurfacesRipBuf100_MN.zip")
unzip("ImperviousSurfacesRipBuf100_MN.zip")
ImperviousSurfacesRipBuf100MN<-read.csv("ImperviousSurfacesRipBuf100_MN.csv")
head(ImperviousSurfacesRipBuf100MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/STATSGO_Set2_MN.zip", destfile = "STATSGO_Set2_MN.zip")
unzip("STATSGO_Set2_MN.zip")
STATSGO_Set2MN<-read.csv("STATSGO_Set2_MN.csv")
head(STATSGO_Set2MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/STATSGO_Set1_MN.zip", destfile = "STATSGO_Set1_MN.zip")
unzip("STATSGO_Set1_MN.zip")
STATSGO_Set1MN<-read.csv("STATSGO_Set1_MN.csv")
head(STATSGO_Set1MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/Dams_MN.zip", destfile = "Dams_MN.zip")
unzip("Dams_MN.zip")
DamsMN<-read.csv("Dams_MN.csv")
head(DamsMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/EPA_FRS_MN.zip", destfile = "EPA_FRS_MN.zip")
unzip("EPA_FRS_MN.zip")
EPA_FRSMN<-read.csv("EPA_FRS_MN.csv")
head(EPA_FRSMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/EPA_FRSRipBuf100_MN.zip", destfile = "EPA_FRSRipBuf100_MN.zip")
unzip("EPA_FRSRipBuf100_MN.zip")
EPA_FRSRipBuf100MN<-read.csv("EPA_FRSRipBuf100_MN.csv")
head(EPA_FRSRipBuf100MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/Mines_MN.zip", destfile = "Mines_MN.zip")
unzip("Mines_MN.zip")
MinesMN<-read.csv("Mines_MN.csv")
head(MinesMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/MinesRipBuf100_MN.zip", destfile = "MinesRipBuf100_MN.zip")
unzip("MinesRipBuf100_MN.zip")
MinesRipBuf100MN<-read.csv("MinesRipBuf100_MN.csv")
head(MinesRipBuf100MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/Lithology_MN.zip", destfile = "Lithology_MN.zip")
unzip("Lithology_MN.zip")
LithologyMN<-read.csv("Lithology_MN.csv")
head(LithologyMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/RoadDensity_MN.zip", destfile = "RoadDensity_MN.zip")
unzip("RoadDensity_MN.zip")
RoadDensityMN<-read.csv("RoadDensity_MN.csv")
head(RoadDensityMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/RoadDensityRipBuf100_MN.zip", destfile = "RoadDensityRipBuf100_MN.zip")
unzip("RoadDensityRipBuf100_MN.zip")
RoadDensityRipBuf100MN<-read.csv("RoadDensityRipBuf100_MN.csv")
head(RoadDensityRipBuf100MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/RoadStreamCrossings_MN.zip", destfile = "RoadStreamCrossings_MN.zip")
unzip("RoadStreamCrossings_MN.zip")
RoadStreamCrossingsMN<-read.csv("RoadStreamCrossings_MN.csv")
head(RoadStreamCrossingsMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/Runoff_MN.zip", destfile = "Runoff_MN.zip")
unzip("Runoff_MN.zip")
RunoffMN<-read.csv("Runoff_MN.csv")
head(RunoffMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/Pesticides97_MN.zip", destfile = "Pesticides97_MN.zip")
unzip("Pesticides97_MN.zip")
Pesticides97MN<-read.csv("Pesticides97_MN.csv")
head(Pesticides97MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/BFI_MN.zip", destfile = "BFI_MN.zip")
unzip("BFI_MN.zip")
BFIMN<-read.csv("BFI_MN.csv")
head(BFIMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/NADP_MN.zip", destfile = "NADP_MN.zip")
unzip("NADP_MN.zip")
NADPMN<-read.csv("NADP_MN.csv")
head(NADPMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/Kffact_MN.zip", destfile = "Kffact_MN.zip")
unzip("Kffact_MN.zip")
KffactMN<-read.csv("Kffact_MN.csv")
head(KffactMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/PRISM_1981_2010_MN.zip", destfile = "PRISM_1981_2010_MN.zip")
unzip("PRISM_1981_2010_MN.zip")
PRISM_1981_2010MN<-read.csv("./PRIV/CPHEA/PESD/COR/CORFILES/Geospatial_Library_Projects/StreamCat/FTP_Staging/States/PRISM_1981_2010_MN.csv")
head(PRISM_1981_2010MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/NABD_MN.zip", destfile = "NABD_MN.zip")
unzip("NABD_MN.zip")
NABDMN<-read.csv("NABD_MN.csv")
head(NABDMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/GeoChemPhys1_MN.zip", destfile = "GeoChemPhys1_MN.zip")
unzip("GeoChemPhys1_MN.zip")
GeoChemPhys1MN<-read.csv("GeoChemPhys1_MN.csv")
head(GeoChemPhys1MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/GeoChemPhys2_MN.zip", destfile = "GeoChemPhys2_MN.zip")
unzip("GeoChemPhys2_MN.zip")
GeoChemPhys2MN<-read.csv("GeoChemPhys2_MN.csv")
head(GeoChemPhys2MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/GeoChemPhys3_MN.zip", destfile = "GeoChemPhys3_MN.zip")
unzip("GeoChemPhys3_MN.zip")
GeoChemPhys3MN<-read.csv("GeoChemPhys3_MN.csv")
head(GeoChemPhys3MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/GeoChemPhys4_MN.zip", destfile = "GeoChemPhys4_MN.zip")
unzip("GeoChemPhys4_MN.zip")
GeoChemPhys4MN<-read.csv("GeoChemPhys4_MN.csv")
head(GeoChemPhys4MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/CoalMines_MN.zip", destfile = "CoalMines_MN.zip")
unzip("CoalMines_MN.zip")
CoalMinesMN<-read.csv("CoalMines_MN.csv")
head(CoalMinesMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/AgriculturalNitrogen_MN.zip", destfile = "AgriculturalNitrogen_MN.zip")
unzip("AgriculturalNitrogen_MN.zip")
AgriculturalNitrogenMN<-read.csv("AgriculturalNitrogen_MN.csv")
head(AgriculturalNitrogenMN)
names(AgriculturalNitrogenMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/RefStreamTempPred_MN.zip", destfile = "RefStreamTempPred_MN.zip")
unzip("RefStreamTempPred_MN.zip")
RefStreamTempPredMN<-read.csv("RefStreamTempPred_MN.csv")
head(RefStreamTempPredMN)
names(RefStreamTempPredMN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/Compton_Phos_Inputs_MN.zip", destfile = "Compton_Phos_Inputs_MN.zip")
unzip("Compton_Phos_Inputs_MN.zip")
Comptom_Phos_Inputs_MN<-read.csv("Compton_Phos_Inputs_MN.csv")
head(Comptom_Phos_Inputs_MN)
names(Comptom_Phos_Inputs_MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/Nsurp_NANI_MN.zip", destfile = "Nsurp_NANI_MN.zip")
unzip("Nsurp_NANI_MN.zip")
Nsurp_NANI_MN<-read.csv("Nsurp_NANI_MN.csv")
head(Nsurp_NANI_MN)
names(Nsurp_NANI_MN)


download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/Precip_Minus_EVT_MN.zip", destfile = "Precip_Minus_EVT_MN.zip")
unzip("Precip_Minus_EVT_MN.zip")
Precip_Minus_EVT_MN<-read.csv("Precip_Minus_EVT_MN.csv")
head(Precip_Minus_EVT_MN)
names(Precip_Minus_EVT_MN)


download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/RockN_MN.zip", destfile = "RockN_MN.zip")
unzip("RockN_MN.zip")
RockN_MN<-read.csv("RockN_MN.csv")
head(RockN_MN)
names(RockN_MN)


download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/Septic_MN.zip", destfile = "Septic_MN.zip")
unzip("Septic_MN.zip")
Septic_MN<-read.csv("Septic_MN.csv")
head(Septic_MN)
names(Septic_MN)


download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/WWTP_MN.zip", destfile = "WWTP_MN.zip")
unzip("WWTP_MN.zip")
WWTP_MN<-read.csv("WWTP_MN.csv")
head(WWTP_MN)
names(WWTP_MN)

download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/WaterInput_MN.zip", destfile = "WaterInput_MN.zip")
unzip("WaterInput_MN.zip")
WaterInput_MN<-read.csv("WaterInput_MN.csv")
head(WaterInput_MN)
names(WaterInput_MN)

#as of 8.9.23, this table does not appear to be available in downloadable list:
#download.file("https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/States/AgDrain_MN.zip", destfile = "AgDrain_MN.zip")
#unzip("AgDrain_MN.zip")
#AgDrain_MN<-read.csv("AgDrain_MN.csv")
#head(AgDrain_MN)
#names(AgDrain_MN)


#################################################
#Remove redundant columns
#(WsAreaSqKm, CatAreaSqKm)
library(purrr)
library(dplyr)

list_dfs <- list(CanalDensityMN, 
                      ElevationMN, NLCD2019MN, NLCD2019RipBuf100MN, 
                      AgMidHiSlopesMN,
                      ImperviousSurfacesMN, ImperviousSurfacesRipBuf100MN, 
                      STATSGO_Set2MN, STATSGO_Set1MN, DamsMN, EPA_FRSMN, EPA_FRSRipBuf100MN, 
                      MinesMN, MinesRipBuf100MN, LithologyMN, RoadDensityMN, RoadDensityRipBuf100MN, RoadStreamCrossingsMN, 
                      RunoffMN, Pesticides97MN, BFIMN, NADPMN, KffactMN, PRISM_1981_2010MN, NABDMN, 
                      GeoChemPhys1MN, GeoChemPhys2MN,GeoChemPhys3MN, GeoChemPhys4MN, 
                      CoalMinesMN, AgriculturalNitrogenMN, RefStreamTempPredMN,
                      Comptom_Phos_Inputs_MN, Nsurp_NANI_MN, Precip_Minus_EVT_MN, RockN_MN, 
                      Septic_MN, WWTP_MN, WaterInput_MN)
list_dfs

list_dfs2 <- map(list_dfs, ~ .x %>%
                   select(-any_of(c("WsAreaSqKm", "CatAreaSqKm", "CatPctFull", "WsPctFull",
                                    "CatPctFullRp100", "WsPctFullRp100", "WsAreaSqKmRp100",
                                    "CatAreaSqKmRp100"))))

#################################################
#use Reduce function to merge multiple tabes

StreamCat<-
  Reduce(function(x,y) merge(x = x, y = y, by = "COMID"), 
         list_dfs2)
names(StreamCat)
nrow(StreamCat)
##LEFT OFF 3.29.23 - Neeed to merge area back in 
#Keep one set to retain watershed area information
names(ElevationMN)
Area<-ElevationMN[,c(1,2,3)]

StreamCat2<-merge(StreamCat, Area, by=c('COMID'))
nrow(StreamCat2)

setwd(input_dir)
write.table(StreamCat2, 'MN_StreamCat_attributes.csv', sep=",", row.names = FALSE)


end.time<-Sys.time()

#check total run time 
end.time-start.time
