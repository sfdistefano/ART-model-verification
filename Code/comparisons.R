library(ggpubr)
library(ggplot2)
library(dplyr)
library(rgdal)
library(sp)
library(raster)
library(tidyr)
library(reshape2)
library(lubridate)

setwd("C:/Users/sfper/Documents/R/WRFO_git")
# reading in all UT Gas Corp reference sites
ref.sites <- as.data.frame(readOGR("C:/Users/sfper/Documents/R/WRFO_git/WRFO_reference_UtahGas_plot_locations.shp"))
# filtering out reference sites that won't be used
similarity.ref <- read.csv("C:/Users/sfper/Documents/R/WRFO_git/bray.csv")
ART.ref.sites <- ref.sites %>% filter(PlotID %in% similarity.ref$Plot1)
# adding the reference site that had blank LPI data so that I can create a complete shapefile # should be 11 reference plots
# ART.ref.sites <- rbind(ART.ref.sites, ref.sites[ref.sites$PlotID == "SDC 7326 Reference",])
# selects the only columns necessary for a shapefile
ART.ref.sites <- ART.ref.sites[, c("PlotID", "Longitd", "Latitud")]
#creates spatial points data frame of reference sites that will be compared to
ART.reference.spdf <- sp::SpatialPointsDataFrame(coords = dplyr::select(ART.ref.sites, Longitd, Latitud),
                                                 data = ART.ref.sites,
                                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
# transforms spdf to shapefile
rgdal::writeOGR(obj = ART.reference.spdf, 
                dsn = "C:/Users/sfper/Documents/R/WRFO_git",
                layer = "ART.reference",
                driver = "ESRI Shapefile",
                overwrite_layer = TRUE)
# read in calculated similarity values from ART model
ART.values <- read.csv("ART.plots_info.csv")
# standardizing names so that merge can go smoothly
ART.values$PlotID <- gsub(pattern = "Road", x = ART.values$PlotID, replacement = "Rd")
ART.values$PlotID <- gsub(pattern = "CATH FED 30 01", replacement = "Cath Fed 30 01", x = ART.values$PlotID)
ART.values$PlotID <- gsub(pattern = "DTU 1058 Well Pad FR", replacement = "DTU 1058", x = ART.values$PlotID)
ART.values$PlotID <- gsub(pattern = "Cities Fed 07 31 FR Well Pad", replacement = "Cities Fed 07 31 FR", x = ART.values$PlotID)
ART.values$PlotID <- gsub(pattern = "PK MTN 9016 FR", replacement = "Pk Mtn 9016", x = ART.values$PlotID)

colnames(ART.values)[6] <- "ART.value"
# sets up for comparison of a plot's ART value to it's bray-curtis similarity value
similarity_ref.ART <- merge(x = similarity.ref, y = ART.values, 
                            by.x = "Plot2", by.y = "PlotID")[, c("Plot1", "Plot2", "bray.sim", "ART.value")]

# removes rows that aren't valid comparisons (not correct ref site to compare to)
similarity_ref.ART <- similarity_ref.ART[!(similarity_ref.ART$Plot1 == "Cath Fed P 35 3 101Road Ref" & 
                                             similarity_ref.ART$Plot2 == "Cath Fed P 35 3 101 FR Plot 1"),]
similarity_ref.ART <- similarity_ref.ART[!(similarity_ref.ART$Plot1 == "Cath Fed P 35 3 101Road Ref" & 
                                             similarity_ref.ART$Plot2 == "Cath Fed P 35 3 101 FR Plot 2"),]
similarity_ref.ART <- similarity_ref.ART[!(similarity_ref.ART$Plot1 == "Cath Fed P 35 3 101 Reference" & 
                                             similarity_ref.ART$Plot2 == "Cath Fed P 35 3 101 Rd Plot 1"),]
similarity_ref.ART <- similarity_ref.ART[!(similarity_ref.ART$Plot1 == "Cath Fed P 35 3 101 Reference" & 
                                             similarity_ref.ART$Plot2 == "Cath Fed P 35 3 101 Rd Plot 2"),]

### DIVERSITY COMPARISONS ####
# creates master comparison df of a plot's ART value to its bray-curtis, simpson, and shannon values
diversity <- read.csv("diversity.results.csv")[,c("SiteName", "PlotID", "simpson", "shannon")]

# extract reference site rows
ref.diversity <- diversity[grep("Reference|Road Ref", diversity$PlotID),]
colnames(ref.diversity) <- c("SiteName", "RefPlot", "ref.simp", "ref.shann")
# extract ART plot rows
plot.diversity <- diversity[!grepl("Reference|Road Ref", diversity$PlotID),]
colnames(plot.diversity) <- c("SiteName", "ARTPlot", "ART.simp", "ART.shann")
# sets up for comparison of a plot's diversity to its reference
diversity.wide <- merge(ref.diversity, plot.diversity, by = "SiteName", all.x = T)

#### MASTER ANALYSIS DATAFRAME ####
ART.info <- read.csv("UT_GasCorp_reclamation_sites_info.csv")

## adding SiteName column for later merge
ART.analysis <- merge(x = similarity_ref.ART, y = diversity, 
                      by.x = "Plot2", by.y = "PlotID", all.x = T)[,c("SiteName", "Plot1", "Plot2", "bray.sim", "ART.value")]

# character is easier to work with in following code
ART.analysis$SiteName <- as.character(ART.analysis$SiteName) 


### CLEANING UP NAMES FOR CORRECT MATCHES ###
# standardizing names so it can be successfully merged later
ART.info$SiteName <- gsub(pattern = "CATH FED 30 01", replacement = "Cath Fed 30 01", x = ART.info$SiteName)

ART.info <- ART.info %>% mutate(SiteName = ifelse(PlotID == "Cath Fed P 35 3 101 Road" & SiteName == "Cath Fed P 35 3 101 Well Pad", "Cath Fed P 35 3 101 Road", SiteName))
ART.analysis <- ART.analysis %>% mutate(SiteName = ifelse(Plot1 == "Cath Fed P 35 3 101Road Ref" & SiteName == "Cath Fed P 35 3 101 Well Pad", "Cath Fed P 35 3 101 Road", SiteName))
# renaming site names where road plots had their own reference site
ART.info <- ART.info %>% mutate(SiteName = ifelse(PlotID == "A-30-3-101S Federal Road" & SiteName == "A-30-3-101S Federal Well Pad", "A-30-3-101S Federal Road", SiteName))
ART.analysis <- ART.analysis %>% mutate(SiteName = ifelse(Plot2 == "A-30-3-101S Rd Plot 1" & SiteName == "A-30-3-101S Federal Reference", "A-30-3-101S Federal Road", SiteName))
ART.analysis <- ART.analysis %>% mutate(SiteName = ifelse(Plot2 == "A-30-3-101S Rd Plot 2" & SiteName == "A-30-3-101S Federal Reference", "A-30-3-101S Federal Road", SiteName))

ART.info <- ART.info %>% mutate(SiteName = ifelse(PlotID == "CATH FED 30 01 Road" & SiteName == "Cath Fed 30 01 Well Pad", "Cath Fed 30 01 Road", SiteName))
ART.analysis <- ART.analysis %>% mutate(SiteName = ifelse(Plot2 == "Cath Fed 30 01 Rd Plot 1" & SiteName == "Cath Fed 30 01 Well Pad", "Cath Fed 30 01 Road", SiteName))
ART.analysis <- ART.analysis %>% mutate(SiteName = ifelse(Plot2 == "Cath Fed 30 01 Rd Plot 2" & SiteName == "Cath Fed 30 01 Well Pad", "Cath Fed 30 01 Road", SiteName))

ART.info <- ART.info %>% mutate(SiteName = ifelse(PlotID == "SDC 7326 FR Road" & SiteName == "Well pad SDC 7326", "SDC 7326 FR Road", SiteName))
ART.analysis <- ART.analysis %>% mutate(SiteName = ifelse(Plot2 == "SDC 7326 FR Rd Plot 1" & SiteName == "Well pad SDC 7326", "SDC 7326 FR Road", SiteName))
ART.analysis <- ART.analysis %>% mutate(SiteName = ifelse(Plot2 == "SDC 7326 FR Rd Plot 2" & SiteName == "Well pad SDC 7326", "SDC 7326 FR Road", SiteName))

# adding well pad or road abandonment date (when reclamation began)
comparisons <- merge(x = ART.analysis, y = ART.info, by = "SiteName", all.x = T)[,c("SiteName", "Plot1", "Plot2", "bray.sim", "Abandon_date", "ART.value")]
# converting date column to a date data format 
comparisons$Abandon_date <- lubridate::mdy(comparisons$Abandon_date)
# extracting year from date
comparisons$abandon_year <- lubridate::year(comparisons$Abandon_date)

write.csv(comparisons, "C:/Users/sfper/Documents/R/WRFO_git/comparison.csv")                                                                                                       
