library(dima.tools)
library(dplyr)
library(rgdal)
library(sp)

setwd("C:/Users/sfper/Documents/R/WRFO_git")
# Reads in DIMA
# read.dima() only works in 32-bit R because both DIMAs are in 32-bit format
wrfo.dima.tables <- read.dima(data.path = "C:/Users/sfper/Documents/R/WRFO_git",
                              dima.list = "UTGasCorp_reclamation.mdb",
                              all.tables = TRUE,
                              stability.tidy = FALSE)

wrfo.dima.aim.tables <- read.dima(data.path = "C:/Users/sfper/Documents/R/WRFO_git",
                             dima.list = "All_Years_AIM_WRFO_2011_2016.mdb",
                             all.tables = TRUE,
                             stability.tidy = FALSE)
## Make a geometry data frame with the site and plot information (just so we can use it later to bind to calculated values)
## the selects() are just to get only the important fields
sites.plots <- merge(x = dplyr::select(wrfo.dima.tables$tblSites, SiteKey, SiteID, SiteName),
                     y = dplyr::select(wrfo.dima.tables$tblPlots, SiteKey, PlotID, Easting, Northing, GPSCoordSys, Datum, Zone),
                     by = "SiteKey") %>% filter(!(SiteName %in% c("Site", "TESTING")))

sites.aim.plots <- merge(x = dplyr::select(wrfo.dima.aim.tables$tblSites, SiteKey, SiteID, SiteName),
                     y = dplyr::select(wrfo.dima.aim.tables$tblPlots, SiteKey, PlotID, Easting, Northing, GPSCoordSys, Datum, Zone),
                     by = "SiteKey") %>% filter(!(SiteName %in% c("Site", "TESTING")))
# Renames Easting and Northing to the appropriate labels of Longitude and Latitude
names(sites.plots)[names(sites.plots) %in% c("Easting", "Northing")] <- c("Longitude", "Latitude")

names(sites.aim.plots)[names(sites.aim.plots) %in% c("Easting", "Northing")] <- c("Longitude", "Latitude")

# write.csv(sites.plots, "C:/Users/sfper/OneDrive/Documents/Jornada/MASTERS/WRFO.sites.plots.2011-2016.csv")
# Selects plots that are classified as reclamation(roads and wellpads)
sites.plots.reclamation.FR <- sites.plots[grep("FR", sites.plots$PlotID),]
sites.plots.reclamation.road <- sites.plots[grep("Road", sites.plots$PlotID),]
# Selects plots that are classified as reference
sites.plots.reference <- sites.plots[grep("Reference", sites.plots$PlotID),]
# One reference is called a road ref so it doesn't get picked up in the previous line
roadref <- sites.plots[grep("Cath Fed P 35 3 101Road Ref", sites.plots$PlotID),]
# There should be a total of 20 reference sites
sites.plots.reference <- rbind(sites.plots.reference, roadref)

# Combines the 2 types of reclamation plots into one file
total.plots.reclamation <- rbind(sites.plots.reclamation.FR, sites.plots.reclamation.road)
# There are some duplicated rows so this removes them. There should be a total of 33 reclamation plots after this
total.plots.reclamation <- total.plots.reclamation[!duplicated(total.plots.reclamation),]
roadref <- total.plots.reclamation[total.plots.reclamation$PlotID == "Cath Fed P 35 3 101Road Ref",]
# Remove one row that is a road reference. There should be a total of 32 reclamation plots after this
total.plots.reclamation <- total.plots.reclamation[!total.plots.reclamation$PlotID == "Cath Fed P 35 3 101Road Ref",]


write.csv(total.plots.reclamation, 
          "C:/Users/sfper/Documents/R/WRFO_git/UTGas.reclamation.sites.csv")
## Creates spatial dataset
sites.plots.spdf <- sp::SpatialPointsDataFrame(coords = dplyr::select(sites.plots, Longitude, Latitude),
                                               data = sites.plots,
                                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
# reclamation sites from UT Gas Corp
sites.plots.reclamation.spdf <- sp::SpatialPointsDataFrame(coords = dplyr::select(total.plots.reclamation, Longitude, Latitude),
                                               data = total.plots.reclamation,
                                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
# manually chosen reference sites
sites.plots.reference.spdf <- sp::SpatialPointsDataFrame(coords = dplyr::select(sites.plots.reference, Longitude, Latitude),
                                               data = sites.plots.reference,
                                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
# aim plots
sites.aim.plots.spdf <- sp::SpatialPointsDataFrame(coords = dplyr::select(sites.aim.plots, Longitude, Latitude),
                                               data = sites.aim.plots,
                                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
#Writes out the shapefile
rgdal::writeOGR(obj = sites.plots.spdf, 
                dsn = "C:/Users/sfper/Documents/R/WRFO_git/WRFO_2011-2016_UtahGas_plot_locations.shp",
                layer = "WRFO_2011-2016_UtahGas_plot_locations",
                driver = "ESRI Shapefile",
                overwrite_layer = TRUE
                )

rgdal::writeOGR(obj = sites.plots.reclamation.spdf, 
                dsn = "C:/Users/sfper/Documents/R/WRFO_git/WRFO_reclamation_UtahGas_plot_locations.shp",
                layer = "WRFO_reclamation_UtahGas_plot_locations",
                driver = "ESRI Shapefile",
                overwrite_layer = TRUE
)

rgdal::writeOGR(obj = sites.plots.reference.spdf, 
                dsn = "C:/Users/sfper/Documents/R/WRFO_git/WRFO_reference_UtahGas_plot_locations.shp",
                layer = "WRFO_reference_UtahGas_plot_locations",
                driver = "ESRI Shapefile",
                overwrite_layer = TRUE
)

rgdal::writeOGR(obj = sites.aim.plots.spdf, 
                dsn = "C:/Users/sfper/Documents/R/WRFO_git/WRFO_2011-2016_AIM_plot_locations.shp",
                layer = "WRFO_2011-2016_AIM_plot_locations",
                driver = "ESRI Shapefile",
                overwrite_layer = TRUE
)
