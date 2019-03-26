library(plyr)
library(dplyr)
library(vegan)
library(gtools)
library(reshape2)
library(tibble)
library(rlang)

## TO DO: ADD COVER BY FUNCTIONAL GROUP (BASED ON CODE??) ##

# make sure you have the most up to date version of dima.tools
# devtools::install_github("nstauffer/dima.tools", force = T)
library(dima.tools)

setwd("C:/Users/sfper/Documents/R/WRFO_git")

# vector of non-foliar codes, will be used to filter later
non.foliar <- c(NA,"", "None", "L", "S", "R", "M", "NL", "WL", "LC")

# reading in DIMA data from ART plots (vegetation monitoring data)
ART.dima.tables <- read.dima(data.path = "C:/Users/sfper/Documents/R/WRFO_git",
                             dima.list = "ARTplots_WRFO.mdb",
                             all.tables = T
                             )
## gathering the line-point-intercept (LPI) data from the DIMA
# normally species.characteristics = T but it throws an error 
# since data collectors didn't populate the growth habit table in DIMA
lpi.tall <- gather.lpi(ART.dima.tables, species.characteristics = F) 

lpi.tall.layers <- lpi.tall$layers

# filtering out rows with non-foliar cover
lpi.veg <- lpi.tall.layers[!(lpi.tall.layers$code %in% non.foliar),]
# adding foliar column # this way you can look at total foliar later
lpi.veg$foliar <- "foliar"
# summary of vegetation cover by plant species
summ.ART.cover <- pct.cover(lpi.tall = lpi.veg,
                                 tall = FALSE,
                                 hit = "any",
                                 by.year = FALSE,
                                 by.line = FALSE,
                                 code
                                )
# same process as above 
# but for data collected for Utah Gas Corp reference and reclamation sites
UTGas.dima.tables <- read.dima(data.path = "C:/Users/sfper/Documents/R/WRFO_git",
                               dima.list = "UTGasCorp_reclamation.mdb",
                               all.tables = T
                               )

UT.lpi.tall <- gather.lpi(UTGas.dima.tables, species.characteristics = F)

UT.lpi.tall.layers <- UT.lpi.tall$layers

# filtering out rows with non-foliar cover
UT.lpi.veg <- UT.lpi.tall.layers[!(UT.lpi.tall.layers$code %in% non.foliar),]
# adding foliar column # this way you can look at total foliar later
UT.lpi.veg$foliar <- "foliar"

summ.UTGas.cover <- pct.cover(lpi.tall = UT.lpi.veg,
                            tall = FALSE,
                            hit = "any",
                            by.year = FALSE,
                            by.line = FALSE,
                            code
)
# for some reason the data for this site isn't pulled in so I'm manually adding it here
# estimates come from the reporting available within DIMA
SDC.cover <- read.csv("SDC7326_cover.csv")

# adding it to the bigger dataset
summ.UTGas.cover <- rbind.fill(summ.UTGas.cover, SDC.cover)
summ.UTGas.cover[is.na(summ.UTGas.cover)] <- 0

## cleaning up data for vegetative similarity calculations
# filtering data to only include the sites of interest
summ.UTGas.cover.match <- summ.UTGas.cover[summ.UTGas.cover$SiteName %in% summ.ART.cover$SiteName,]
# subsetting only the cover for reference sites
summ.UTGas.ref.cover <- summ.UTGas.cover.match[grep("Reference|Road Ref", summ.UTGas.cover.match$PlotID),]

# all cover data for reference and ART plots in one df
summ.cover <- rbind.fill(summ.ART.cover, summ.UTGas.ref.cover)

# setting all NAs to 0 in preparation for similarity calculations
summ.cover[is.na(summ.cover)] <- 0
# cleaning up names so that they'll match
summ.cover$SiteName <- gsub(pattern = "CATH FED 30 01 Well Pad",
                            replacement = "Cath Fed 30 01 Well Pad",
                            x = summ.cover$SiteName)
#### LPI DATA BY FUNCTIONAL GROUP ####

### Still working on this, right NOW imported DIMA data doesn't have species characteristics ###

# ## summarizing lpi data based on functional group 
# # we're interested in shrubs, perennial grasses, and invasives
# summ.ART.fg.cover <- pct.cover(lpi.tall = lpi.tall.layers,
#                                tall = FALSE,
#                                hit = "any",
#                                by.year = FALSE,
#                                by.line = FALSE,
#                                duration, growth.habit, invasive
# )
# summ.UT.fg.cover <- pct.cover(lpi.tall = UT.lpi.tall.layers,
#                               tall = FALSE,
#                               hit = "any",
#                               by.year = FALSE,
#                               by.line = FALSE,
#                               duration, growth.habit, invasive
# )
# 
# ## summing cover values for: shrub, perennial grass, annual grass, invasive
# # foliar cover for ART plots
# summ.ART.fg.cover$shrub <- rowSums(summ.ART.fg.cover[,c("perennial.shrub.no", "perennial.shrub.yes")])
# summ.ART.fg.cover$PG <- rowSums(summ.ART.fg.cover[,c("perennial.graminoid.no", "perennial.graminoid.yes")])
# summ.ART.fg.cover$invasive <- rowSums(summ.ART.fg.cover[,grepl("yes", names(summ.ART.fg.cover))])
# 
# # foliar cover for reclamation and reference sites
# summ.UT.fg.cover$shrub <- rowSums(summ.UT.fg.cover[,c("perennial.shrub.no", "perennial.shrub.yes")])
# summ.UT.fg.cover$PG <- rowSums(summ.UT.fg.cover[,c("perennial.graminoid.no", "perennial.graminoid.yes")])
# summ.UT.fg.cover$invasive <- rowSums(summ.UT.fg.cover[,grepl("yes", names(summ.UT.fg.cover))])
# 
# ## merging the reclamation, reference, and ART plots based on Site Name so they can be compared
# # cleaning up names so they'll match for merge
# summ.ART.fg.cover$SiteName <- gsub(replacement = "CATH FED 30 01 Well Pad",
#                                    pattern = "Cath Fed 30 01 Well Pad",
#                                    x = summ.ART.fg.cover$SiteName) 
# # merging so that ART plot can be compared to their reference and reclamation
# # each comparison has its own row
# func.group <- merge(summ.ART.fg.cover,summ.UT.fg.cover, by = "SiteName", all.x = T)[,c("SiteName", "PlotID.x","PlotID.y", "shrub.x", "PG.x", "AG.x", "invasive.x",
#                                                                                        "shrub.y", "PG.y", "AG.y", "invasive.y")]
# # selecting comparisons between ART plot and its reference    
# func.group <- func.group[grep("Reference|Ref",func.group$PlotID.y),]

#### BRAY-CURTIS ####
# each list component, is all the plots related to one reclamation area # used for bray-curtis
summ.cover.split <- split(x = summ.cover, f = summ.cover$SiteName)


# removing non-numeric columns that don't have numeric information otherwise you'll get an error message when trying to calculate vegdist()
sim.cover.filter <- lapply(summ.cover.split, FUN = function(x){x[, c(5:130)]})
# converting first column into rownames, this makes it possible to run the sim index which requires all numeric values in matrix
sim.cover.filter <- lapply(sim.cover.filter, function(x){ row.names(x)<-as.character(x$PlotID); x})
# removed the first column of PlotID (they are now rownames)
sim.cover.filter <- lapply(sim.cover.filter, FUN = function(x){x[, c(2:126)]})

# calculating the difference between plots of the same reclamation using bray-curtis # 0 = completely similar, 1 = completely dissimilar
sim.analysis <- lapply(sim.cover.filter, function(x){vegdist(x, method =  "bray")})
# converts sim index output ("dist")into usable dataframe output
sim.df <- lapply(sim.analysis, function(x){as.data.frame(as.matrix(x))})
# converting rownames into first column
sim.df <- lapply(sim.df, function(x){tibble::rownames_to_column(x, "Plot1")})
# transforming to a long format, where there's a distance value between 2 plots in each row
sim.gathered <- lapply(sim.df, function(x){melt(x, id.vars = "Plot1", variable.name = "Plot2", value.name = "distance")})
# selecting only rows that pertain to reference similarity
sim.ref.values <- lapply(sim.gathered, function(x){x[grep("Reference|Road Ref", x$Plot1),]})

# combines all elements of list into one dataframe
similarity <- ldply(sim.gathered, data.frame)
similarity$bray.sim <- 1 - similarity$distance
similarity.ref <- similarity[!grepl("Reference|Road Ref", similarity$Plot2),]  
similarity.ref <- similarity.ref[grepl("Reference|Road Ref", similarity.ref$Plot1),]
write.csv(similarity.ref, "bray.csv")

#### SIMPSON ####
# removing non-numeric columns that don't have numeric information otherwise you'll get an error message when trying to calculate vegdist()
simpson.filter <- summ.cover[, c(5:130)]
# converting first column into rownames, this makes it possible to run the sim index which requires all numeric values in matrix
row.names(simpson.filter)<-as.character(simpson.filter$PlotID)
# removed the first column of PlotID (they are now rownames)
simpson.filter <- simpson.filter[, c(2:126)]
# calculate simpson diversity indice for each plot
simpson.results <- diversity(simpson.filter, index = "simpson")
# converts results into usable dataframe
simpson.results.df <- as.data.frame(as.matrix(simpson.results))
# converts rownames to column "PlotID"
simpson.results.df <- tibble::rownames_to_column(simpson.results.df, "PlotID")
# rename second column
colnames(simpson.results.df)[2] <- "simpson"
# adding SiteName so that they can be split later for comparison
simpson <- merge(x = simpson.results.df, y = summ.cover, by = "PlotID", all.x = T)[,c("SiteName","PlotID", "simpson")]
# splitting plots based on shared "SiteName" aka they share the same reclamation 
simpson.split <- split(x = simpson, f = simpson$SiteName)

#### SHANNON-WIENER ####
# removing non-numeric columns that don't have numeric information otherwise you'll get an error message when trying to calculate vegdist()
shannon.filter <- summ.cover[, c(5:130)]
# converting first column into rownames, this makes it possible to run the sim index which requires all numeric values in matrix
row.names(shannon.filter)<-as.character(shannon.filter$PlotID)
# removed the first column of PlotID (they are now rownames)
shannon.filter <- shannon.filter[, c(2:126)]

# calculate simpson diversity indice for each plot
shannon.results <- diversity(shannon.filter, index = "shannon")
# converts results into usable dataframe
shannon.results.df <- as.data.frame(as.matrix(shannon.results))
# converts rownames to column "PlotID"
shannon.results.df <- tibble::rownames_to_column(shannon.results.df, "PlotID")
# rename second column
colnames(shannon.results.df)[2] <- "shannon"
# adding SiteName so that they can be split later for comparison
shannon <- merge(x = shannon.results.df, y = summ.cover, by = "PlotID", all.x = T)[,c("SiteName","PlotID", "shannon")]

#### FINAL DIVERSITY RESULTS ####
diversity <- merge(x = shannon, y = simpson, by = "PlotID")[,c("SiteName.x","PlotID", "simpson", "shannon")]
# rename column so that it doesn't have th extra ".x"
colnames(diversity)[1] <- "SiteName"

write.csv(diversity, "diversity.results.csv")

