# Overview

This repository includes input data and R scripts to reproduce analyses, figures and tables described in the manuscript by Dolph et al. entitled "Phosphorus transport in a hotter and drier climate: in-channel release of legacy phosphorus during summer low flow conditions". This manuscript has been published in the journal Hydrology and Earth System Sciences (HESS).

# Files:

"gaged_watersheds_data_processing_and_analysis_script.R" is the main data processing and analysis script file. It contains R script and references additional R modules for:

-   inputting, pre processing and analyzing concentration-discharge data for 143 gaged watersheds in Minnesota;

-   comparing mean SRP among gaged watersheds to mean SRP among monitoried tile outlets from 10 farm fields;

-   developing a random forest model for predicting mean SRP during late summer low flows for gaged watersheds, using 253 geospatial attributes derived predominantly from [EPA StreamCat](https://www.epa.gov/national-aquatic-resource-surveys/streamcat-dataset)

-   analysis of independent SRP concentration data collected in the field during late summer from ditches, streams and rivers in the heavily agricultural Le Sueur River Basin, Minnesota.

All additional R modules (for analysis of field data, for preprocessing of StreamCat variables, for random forest regression, and for preprocessing of tile data) are referenced in the main project script.

Input data files necessary to run the analysis include:

-   "All_gages_discharge_WPLMN.csv": concentration-discharge data for gaged Minnesota watersheds.

-   "Gages_watershed_tile_density.csv": Estimated tile density for each watershed in m2/km2.

-   "Gages_watershed_NHD_ID.csv": gage information including station ID, lat/long coordinates, and COMID (unique identifier from the NHDPlusv2).

-   MN_StreamCat_attributes: US EPA StreamCat attributes downloaded for the state of Minnesota.

-   "MRB_Water_Chem_2013_2016": original water chemistry data collected in the field for \~200 stream sites in Minnesota

-   "MRB_study_sites_wNHD_attributes.csv": Site information about field sites where water chemistry data was collected, including lat/long coordinates, and NHD COMID.

-   "MRB_study_sites_watershed_area.csv": watershed area data (in km2) for field sites.

-   "streamcat_variable_info_9_24_24.csv": StreamCat attribute information downloaded directly from US EPA.

**For questions** please contact Dr. Christy Dolph at dolph008\@umn.edu.
