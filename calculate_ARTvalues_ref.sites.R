library(rgdal)
library(rgeos)
library(raster)
library(tidyverse)
library(qdapRegex)

setwd("C:/Users/sfper/Documents/R/WRFO_git/ART.index")

raster.point.stack <- stack("WRFO.UtahGasCorp.30m.2kmres.separated.tif")
sites.plots.reference.spdf <- readOGR(dsn = "C:/Users/sfper/Documents/R/WRFO_git",
                                      layer = "WRFO_reference_UtahGas_plot_locations")
sites.plots.reclamation.spdf <- readOGR(dsn = "C:/Users/sfper/Documents/R/WRFO_git",
                                        layer = "WRFO_reclamation_UtahGas_plot_locations")

# Extracting specific sim values for reference sites to their reclamation sites
UT_GasCorp_refvalues <- raster::extract(raster.point.stack, sites.plots.reference.spdf)
UT_GasCorp_refvalues <- as.data.frame(UT_GasCorp_refvalues)

# renaming columns to reclamation site names
names(UT_GasCorp_refvalues) <- sites.plots.reclamation.spdf$PlotID
# adding the reference sites for each reclamation
UT_GasCorp_refvalues$reference <- sites.plots.reference.spdf$PlotID
# reordering columns so that reference names are first
UT_GasCorp_refvalues <- UT_GasCorp_refvalues[,c(33, 1:32)]

# need to figure out how to find matches
UT.refvalues.tidy <- gather(UT_GasCorp_refvalues, "reclamation", "similarity", 2:33)
# cleaning up names so it's easier to compare reclamation to their reference
UT.refvalues.tidy$ref.clean <- gsub("Reference|Road|Ref", x = UT.refvalues.tidy$reference, replacement = "")
UT.refvalues.tidy$rec.clean <- gsub("Well|Pad|Road|FR", x = UT.refvalues.tidy$reclamation, replacement = "")
# removing extra white space
UT.refvalues.tidy$rec.clean <- rm_white(UT.refvalues.tidy$rec.clean)
UT.refvalues.tidy$ref.clean <- rm_white(UT.refvalues.tidy$ref.clean)
# finding the right reference for each reclamation
# selecting rows where the reference and reclamation have the same name
UT.refvalues.clean <- UT.refvalues.tidy[UT.refvalues.tidy$ref.clean == UT.refvalues.tidy$rec.clean,]
# removing incorrect matches between reclamation and reference
# these reclamation sites are unique because the road and the well-pad have different reference
# usually roads share the reference with their well-pad
UT.refvalues.clean <- UT.refvalues.clean[!UT.refvalues.clean$reference == "Cath Fed P 35 3 101 Reference" |
                                           !UT.refvalues.clean$reclamation == "Cath Fed P 35 3 101 Road",]
UT.refvalues.clean <- UT.refvalues.clean[!UT.refvalues.clean$reference == "Cath Fed P 35 3 101Road Ref" |
                                           !UT.refvalues.clean$reclamation == "Cath Fed P 35 3 101 FR",]

# writing out the ART index values for all reference sites to their reclamation
# of the 24 reclamation, only 14 were used because of time limits for field collection
# reclamation and reference sites were provided by UT Gas Corp
write.csv(UT.refvalues.clean,"C:/Users/sfper/Documents/R/WRFO_git/UT.ref.ART.values.csv")
