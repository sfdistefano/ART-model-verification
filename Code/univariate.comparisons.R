library(plotrix)
library(readxl)
library(tidyverse)
library(qdapRegex)
library(ggplot2)
library(Metrics)
library(raster)
library(rgdal)
library(plyr)
library(qdapRegex)
library(psych)

setwd("C:/Users/sfper/Documents/R/WRFO_git/Soil.data/PSA_data.entry")

#### IMPORTING & TIDYING RAW PSA DATA ####
file.list <-list.files(pattern='*.xlsx')
# specifying range of cells to pull in, the excel sheet is NOT tidy
psa.list <- lapply(file.list, read_excel, range = "F12:AQ36")
# removing unneeded columns (all NA or misc info)
psa.list2 <- lapply(psa.list, "[", c(1, 13:16, 22:25, 35:38))
# removing any columns with dates because not all files have date data
# the date column is the only column right now that isn't character
psa.list3 <- lapply(psa.list2, function(x) x[,sapply(x, is.character)])
# column names
psa.names <- c("grav.moist", "tin.wt", "tin.samp.wt", "tin.samp.oven.wt", "subsamp.wt", "site", "plot", 
               "horizon", "subsamp.oven.wt", "sand", "silt", "clay")
# renaming all columns in the list
psa.list4 <- lapply(psa.list3, setNames, nm = psa.names)
# removing unneeded rows (blank)
psa.clean <- lapply(psa.list4, function(x) x[4:38,])
# collapsing all list dataframes into 1 dataframe
psa.df <- plyr::rbind.fill(psa.clean)
# removing rows that were entirely NA
psa.df.clean <- psa.df[!is.na(psa.df$grav.moist),]
# combining site and plot to create new plot column
# needed to merge with field data
# when plot or site is unknown it will add NA
psa.df.clean$plot <- paste(psa.df.clean$site, psa.df.clean$plot)
# changing all horizon names to lowercase for later merging processes
psa.df.clean$horizon <- tolower(psa.df.clean$horizon)

# cleaning up names for later merge
psa.df.clean <- psa.df.clean %>% mutate(plot = rm_white(gsub(pattern = "NA",
                                                             replacement = "", x = plot))) %>%
                                mutate(plot = gsub(pattern = "Fed 30-16 Plot",
                                                   replacement = "Federal 30-16 Plot", x = plot)) %>%
                                mutate(site = gsub(pattern = "Federal 30-16", replacement = "Fed 30-16", x = site))

#### COMBINING PSA & FIELD DATA ####
setwd("C:/Users/sfper/Documents/R/WRFO_git/Soil.data")

## field data that includes horizon depths and is needed to weight variable percentages
# includes calculation of the bedrock depth and cleaning up of names for later merge
ref.field <- read.csv("ref.soil.field.csv") %>% group_by(plot) %>% mutate(Total.Soil.Pedon.Depth = max(Lower.Depth))
art.field <- read.csv("art.soil.field.csv") %>% mutate(plot = rm_white(gsub(pattern = "FR|Well Pad", 
                                                                   replacement = "", x = plot))) %>% 
  group_by(plot) %>% mutate(Total.Soil.Pedon.Depth = max(Lower.Depth))

# merging reference field data with psa data
soil.ref <- merge(psa.df.clean, ref.field, by = c("plot", "horizon"))
# checking number of plots, should be 11
 unique(soil.ref$plot)

# merging ART plot field data with psa data
soil.art <- merge(psa.df.clean, art.field, by = c("plot", "horizon"))
# checking number of plots, should be 28
# right now it should be 26 bc some PSA data had unknown sites or plots, ADD LATER
 unique(soil.art$plot)

## changing columns from character to numeric
# columns I want to change
colnames.num <- c("grav.moist", "tin.wt", "tin.samp.wt", "tin.samp.oven.wt", "subsamp.wt", "sand", "silt", "clay")
# making those column numbreic
soil.art[, colnames.num] <- sapply(soil.art[, colnames.num], as.numeric)
soil.ref[, colnames.num] <- sapply(soil.ref[, colnames.num], as.numeric)

# cleaning site names so that they will merge correctly later on
soil.art$site <- gsub(soil.art$site, pattern = "Cath Fed 30 01 Rd", 
                           replacement = "Cath Fed 30 01")
soil.art$site <- gsub(soil.art$site, pattern = "SDC 7326 Rd", 
                           replacement = "SDC 7326")
soil.art$site <- gsub(soil.art$site, pattern = "A-30-3-101S Rd", 
                           replacement = "A-30-3-101S")
soil.ref$site <- gsub(soil.ref$site, pattern = "Fed 30 16",
                           replacement = "Fed 30-16")
soil.ref[soil.ref$site == "Cath Fed P 35 3 101" & 
           soil.ref$plot == "Cath Fed P 35 3 101 Road Ref", 
              "site"] <- "Cath Fed P 35 3 101 Rd"

#### WATER HOLDING CAPACITY ####

### ART PLOTS ###
soil.art$hor.depth <- (soil.art$Lower.Depth - soil.art$Upper.Depth)
# weighting gravimetric moisture (scale 0-1) 
# by how much space the horizon took up in the pit
soil.art$hor.grav.wt <- (soil.art$grav.moist * soil.art$hor.depth)/soil.art$Total.Soil.Pedon.Depth
# summing all rock %s from all horizons in each plot
total.whc.art <- aggregate(data = soil.art, hor.grav.wt ~ plot, sum)


### REFERENCE SITES ###
soil.ref$hor.depth <- (soil.ref$Lower.Depth - soil.ref$Upper.Depth)
# weighting gravimetric moisture (scale 0-1) 
# by how much space the horizon took up in the pit
soil.ref$hor.grav.wt <- (soil.ref$grav.moist * soil.ref$hor.depth)/
  soil.ref$Total.Soil.Pedon.Depth
# summing all rock %s from all horizons in each plot
total.whc.ref <- aggregate(data = soil.ref, hor.grav.wt ~ plot, sum)


### COMBINING ART & REF GRAVIMETRIC ###
# adding site column
# some rows are duplicated which is why I added unique()
total.whc.ref <- unique(merge(total.whc.ref, soil.ref[,c("site", "plot")], by = "plot"))
total.whc.art <- unique(merge(total.whc.art, soil.art[,c("site", "plot")], by = "plot"))

# renaming columns to make less confusing
colnames(total.whc.art) <- c("art.plot", "art.grav.moist", "site")
colnames(total.whc.ref) <- c("ref.site", "ref.grav.moist", "site")

# there should only be 11 unique sites
unique(total.whc.art$site)
unique(total.whc.ref$site)

# merging so that art plots can be compared with their reference
# rows should be equal to the number of rows in total.whc.art
whc.wide <- merge(total.whc.art, total.whc.ref, by = "site")

# removing 3 outliers
whc.wide <- whc.wide[!(whc.wide$art.plot == "Fed 30-16 Plot 2"|
                         whc.wide$ref.site == "Cath Fed P 35 3 101 Ref"),]

# write.csv(whc.wide, "C:/Users/sfper/Documents/R/WRFO_git/Soil.data/whc.wide.csv")
#### ROCK FRAGMENT % ####
## reference sites
# weighting each horizon's rock % by it's depth within the soil pedon
soil.ref$hor.rock.wt <- (soil.ref$horizon.rock.frag.vol * soil.ref$hor.depth)/
  soil.ref$Total.Soil.Pedon.Depth
# summing all rock %s from all horizons in each plot
total.rock.ref <- aggregate(data = soil.ref, hor.rock.wt ~ plot, sum)

## art plots
# weighting each horizon's rock % by it's depth within the soil pedon
soil.art$hor.rock.wt <- (soil.art$horizon.rock.frag.vol * soil.art$hor.depth)/
  soil.art$Total.Soil.Pedon.Depth
# summing all rock %s from all horizons in each plot
total.rock.art <- aggregate(data = soil.art, hor.rock.wt ~ plot, sum)

# adding site column
# some rows are duplicated which is why I added unique()
total.rock.ref <- unique(merge(total.rock.ref, soil.ref[,c("site", "plot")], by = "plot"))
total.rock.art <- unique(merge(total.rock.art, soil.art[,c("site", "plot")], by = "plot"))

# renaming columns to make less confusing
colnames(total.rock.art) <- c("art.plot", "art.rock", "site")
colnames(total.rock.ref) <- c("ref.site", "ref.rock", "site")

# merging so that art plots can be compared with their reference
# rows should be equal to the number of rows in total.rock.art
rock.wide <- merge(total.rock.art, total.rock.ref, by = "site")

## difference between art plots and ref
rock.wide$rock.diff <- abs(rock.wide$art.rock - rock.wide$ref.rock)

write.csv(rock.wide, "rock.diff.wide.csv")
## Frequency table for rock fragment % differences 
#between art plots and their reference
rock.diff <- rock.wide$rock.diff

# breaks for table
# range from 0 to 40, with a break every 10
rock.breaks <- seq(0, 40, by = 10)
# categorizing data by breaks
rock.cut = cut(rock.diff, rock.breaks, right = FALSE)
# frequency table
rock.freq = table(rock.cut)

## statistics: root mean square error
Metrics::rmse(rock.wide$art.rock, rock.wide$ref.rock)

#### BEDROCK ####
# creating a separate df of bedrock depth
bedrock.ref <- unique(soil.ref[,c("site", "plot", "Total.Soil.Pedon.Depth")])
colnames(bedrock.ref) <- c("site", "ref.site", "ref.bedrock")
bedrock.art <- unique(soil.art[,c("site", "plot", "Total.Soil.Pedon.Depth")])
colnames(bedrock.art) <- c("site", "art.plot", "art.bedrock")
# wide dataframe to compare art plot bedrock depth to ref site bedrock depth
bedrock.wide <- merge(bedrock.art, bedrock.ref, by = "site")

bedrock.wide$bedrock.diff <- abs(bedrock.wide$art.bedrock - bedrock.wide$ref.bedrock)

# write.csv(bedrock.wide, "bedrock.diff.wide.csv")
## Frequency table for bedrock depth differences 
#between art plots and their reference
bedrock.diff <- bedrock.wide$bedrock.diff
# breaks for table
# range from 0 to 40, with a break every 10
bedrock.breaks <- seq(0, 40, by = 10)
# categorizing data by breaks
bedrock.cut = cut(bedrock.diff, bedrock.breaks, right = FALSE)
# frequency table
bedrock.freq = table(bedrock.cut)

## statistics: root mean square error
Metrics::rmse(bedrock.wide$art.bedrock, bedrock.wide$ref.bedrock)

#### SLOPE ####

setwd("C:/Users/sfper/Documents/R/WRFO_git")

# pulling raster that has the slope values calculated from DEM in QGIS
slope <- raster("Topographic.data/Slope08142017.tif")
# reading in the plot locations
ref.sites <- readOGR("WRFO_reference_UtahGas_plot_locations.shp")
rec.sites <- readOGR("rec.sites.visit.shp")
ART.plots <- readOGR("final_ART.plots.shp")

# extracting slope values
rec.slope <- as.data.frame(extract(slope, rec.sites, method = "simple"))
rec.slope$PlotID <- rec.sites$PlotID
rec.slope$SiteID <- rec.sites$SiteID
colnames(rec.slope)[1] <- "slope"
# cleaning up SiteID column in preparation for merge()
rec.slope$SiteID <- gsub(pattern = "Road|FR|Well Pad|Federal|L19 2101|[()]|D25 1103", 
                         replacement = "",
                         x = rec.slope$SiteID)
rec.slope$SiteID <- rm_white(rec.slope$SiteID)
# cleaning up SiteID column in preparation for merge()
ref.slope <- as.data.frame(extract(slope, ref.sites, method = "simple"))
ref.slope$PlotID <- ref.sites$PlotID
ref.slope$SiteID <- ref.sites$SiteID
colnames(ref.slope)[1] <- "slope"
ref.slope$SiteID <- gsub(pattern = "Road|FR|Well Pad|Federal|L19 2101|[()]|D25 1103", 
                         replacement = "",
                         x = ref.slope$SiteID)
ref.slope$SiteID <- rm_white(ref.slope$SiteID)
# cleaning up SiteID column in preparation for merge()
ART.slope <- as.data.frame(extract(slope, ART.plots, method = "simple"))
ART.slope$PlotID <- ART.plots$PlotID
colnames(ART.slope)[1] <- "slope"
ART.slope$SiteID <- gsub(pattern = "Plot 1|Plot 2|Road|FR|Well Pad|Federal", 
                         replacement = "",
                         x =ART.slope$PlotID)
ART.slope$SiteID <- rm_white(ART.slope$SiteID)
# creates long dataframe
total.slope <- rbind(ref.slope, rec.slope, ART.slope)

# creates wide dataframe
slope.wide <- merge(rec.slope, ref.slope, by = "SiteID")
# resulting df has the slopes of each ART plot with their corresponding rec and ref site
# there are duplicates of rec and ref sites since ART plots share them
total.slope.wide <- merge(slope.wide, ART.slope, by = "SiteID")
colnames(total.slope.wide) <- c("SiteID", "slope.rec", "reclamation", 
                                "slope.ref", "reference", "slope.ART", "ART.plot")

## resulting df has the slopes of each ART plot and their REFERENCE
slope.wide.ART.ref <- merge(ART.slope,ref.slope, by = "SiteID")
# getting rid of incorrect matches (have same site but road has a separate reference)
slope.wide.ART.ref <- slope.wide.ART.ref %>% 
  filter(!(PlotID.x == "Cath Fed P 35 3 101 Road Plot 1" & 
             PlotID.y == "Cath Fed P 35 3 101 Reference")) %>% 
  filter(!(PlotID.x == "Cath Fed P 35 3 101 Road Plot 2" & 
             PlotID.y == "Cath Fed P 35 3 101 Reference")) %>% 
  filter(!(PlotID.x == "Cath Fed P 35 3 101 FR Plot 1" & 
             PlotID.y == "Cath Fed P 35 3 101Road Ref")) %>%
  filter(!(PlotID.x == "Cath Fed P 35 3 101 FR Plot 2" & 
             PlotID.y == "Cath Fed P 35 3 101Road Ref"))
# renaming columns for clarity
colnames(slope.wide.ART.ref) <- c("SiteID", "ART.slope", "ART.plot", "ref.slope", "reference")
# calculating slope differences between ART plots and their reference
slope.wide.ART.ref$slope.diff <- abs(slope.wide.ART.ref$ART.slope - slope.wide.ART.ref$ref.slope) * 100

write.csv(slope.wide.ART.ref, "C:/Users/sfper/Documents/R/WRFO_git/Topographic.data/slope.wide.csv")
