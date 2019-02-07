library(plotrix)
library(readxl)
library(tidyverse)
library(qdapRegex)
library(ggplot2)


setwd("C:/Users/sfper/Documents/R/WRFO_git/Soil/PSA_data.entry")

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

write.csv(psa.df.clean, "psa.combined.csv")

#### COMBINING PSA & FIELD DATA ####
setwd("C:/Users/sfper/Documents/R/WRFO_git/Soil")

# field data that includes horizon depths
# needed to weight variable percentages
ref.field <- read.csv("ref.soil.field.csv")
art.field <- read.csv("art.soil.field.csv")
art.depth <- read.csv("art.plot.depth.csv")
# adding total pedon depth
art.field <- merge(art.field, art.depth[,c("plot", "Total.Soil.Pedon.Depth")], by = "plot")
# cleaning names so that all rows will merge later
art.field$plot <- gsub(art.field$plot, pattern = "FR|Well Pad", replacement = "")
art.field$plot <- gsub(art.field$plot, pattern = "Federal", replacement = "Fed")
art.field$plot <- rm_white(art.field$plot)

# getting rid of site column
# FIGURE OUT HOW TO REMOVE 1 COLUMN BY NAME
art.field <- art.field[,-2]

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

## graphing relationship between art plots and their reference 
## for water holding capacity (gravimetric moisture)

# graphing regression function
ggplotRegression <- function(fit){
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red", se = F) +
    labs(title = paste("R2 = ",signif(summary(fit)$adj.r.squared, 2),
                       "Intercept =",signif(fit$coef[[1]],2 ),
                       " Regression Coefficient =",signif(fit$coef[[2]], 2),
                       " p-value =",signif(summary(fit)$coef[2,4], 2)))
}

# removing 3 outliers
whc.wide <- whc.wide[!(whc.wide$art.plot == "Fed 30-16 Plot 2"|
                         whc.wide$ref.site == "Cath Fed P 35 3 101 Ref"),]

# graphing the relationship between ART plots and there reference
# comparing water holding capacity aka gravimetric moisture
# these values were obtained by air drying and then oven drying samples 
# for 48 hrs at 105 degrees celsius
ggplotRegression(lm(ref.grav.moist ~ art.grav.moist, data = whc.wide)) +
  xlab("ART Plot Water Holding Capacity (%) ") +
  ylab("Reference Water Holding Capacity (%)") +
  # geom_abline(linetype = "dashed")
  # scale_y_continuous(breaks = c(0.0, 0.02, 0.04, 0.06, 0.08)) +
  # scale_x_continuous(breaks = c(0.0, 0.02, 0.04, 0.06, 0.08)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


#### ROCK FRAGMENT % ####
## reference sites
# weighting each horizon's rock % by it's depth within the soil pedon
soil.ref$hor.rock.wt <- (soil.ref$Total.Rock.Fragments..vol * soil.ref$hor.depth)/
  soil.ref$Total.Soil.Pedon.Depth
# summing all rock %s from all horizons in each plot
total.rock.ref <- aggregate(data = soil.ref, hor.rock.wt ~ plot, sum)

## art plots
# weighting each horizon's rock % by it's depth within the soil pedon
soil.art$hor.rock.wt <- (soil.art$Total.Rock.Fragments..vol * soil.art$hor.depth)/
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

#### BEDROCK ####
# creating a separate df of bedrock depth
bedrock.ref <- unique(soil.ref[,c("site", "plot", "Total.Soil.Pedon.Depth")])
colnames(bedrock.ref) <- c("site", "ref.site", "ref.bedrock")
bedrock.art <- unique(soil.art[,c("site", "plot", "Total.Soil.Pedon.Depth")])
colnames(bedrock.art) <- c("site", "art.plot", "art.bedrock")
# wide dataframe to compare art plot bedrock depth to ref site bedrock depth
bedrock.wide <- merge(bedrock.art, bedrock.ref, by = "site")

bedrock.wide$bedrock.diff <- abs(bedrock.wide$art.bedrock - bedrock.wide$ref.bedrock)

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
