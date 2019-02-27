# This code written by Colby Brungard to produce conditioned latin hypercube sampling points and similarity indecies across Black Mesa in San Juan County, UT, USA. 
# 10/15/2015

# This code was altered by Sean Di Stefano to produce similarity indices for reclamation sites within the BLM White River FO in Colorado
# 10/03/2017

library(rgdal)
library(raster)
library(sp)

setwd("C:/Users/sfper/Documents/R/WRFO_git/ART.index")
# ---------------------------------------------------------------------------------------------------------------------    
# Run the CLHS algorithm

# When needed, scale up 30m resolution to 60m by a factor of 2, this makes the data a quarter of the size
# WRFO.composite.60m <- aggregate(WRFO_composite, factor = 2)


# --------------------------------------------------------------------------------------------------------
# Gowers similarity Index

# To run this similairty index on your own data, put in the appropriate object names in the following code and uncomment:
cstak.bm <- stack("WRFO_composite_30m.tif") #Raster stack of covariates input to cLHS. 
csamp <- readOGR(dsn = "C:/Users/sfper/Documents/R/WRFO_git",
                 layer = "WRFO_reclamation_UtahGas_plot_locations") # SpatialPointsDataframe of the cLHS points


# ****************************************************************************************************
# Calculate Gower's similarity index for every pixel within a 500 m radius buffer of each sampling point.
# 1. First define the raster that will be used to hold the raster values
r = cstak.bm[[2]] #Get a raster that is the same size as the covariates used. The easiest way is to just select the first covariate from the raster stack. 
r[] <- NA #Since I took this from the first layer from raster stack make all the cell values NA. 
names(r) <- 'Similarity' #Change the name.

# 2. extract all pixels within 2000 m of the sampling points. This returns a list, where each element in the list is a matrix. In this case buffer = 2000 m (2km), so final buffer will be 500 m in diameter. 
# Currently at 5 km, was previously 2 km
buffs.bm <- extract(x = cstak.bm, y = csamp, method = 'simple', buffer = 2000, factors = TRUE, cellnumbers = TRUE)
# x = the rasterstack to extract values from
# y = spatialPointsDataFrame of the cLHS locations
# buffer = size (in m) of the buffer 
# factors = keeps the right values associated with the factor covariates
# cellnumbers = this returns the cell numbers which eventually allows you to make a raster. It is very important. 


# 3.Apply gowers similarity index to each element of list of extracted raster values.
# Get the cell numbers from each cLHS point. This will be used to identity the appropriate column in the similarity matrix. 
cLHS_cellNums <- extract(x = cstak.bm, y = csamp, method = 'simple', cellnumbers = TRUE, small = TRUE)
cCellnum <- cLHS_cellNums[,1] #just need the cell numbers. 


# 3.b Function to calculate Gower's similarity index around each point. I used Gower's because it can handle categorical covariates, but there are other options. 
library(cluster)

# Define a list to hold the data.   
similarL <- list()

# The actual function.

for(i in seq(buffs.bm)){
  
  testL <- data.frame(buffs.bm[[i]]) # convert each list element in buffs to a data frame
  testL <- testL[complete.cases(testL),] # remove NA values. If values are on edges this makes the elements of similarL not the same so they can't be rbind-ed together
  if (length(testL) > 0) {
    testL[,2] <- as.factor(testL[,2]) # Force any factors to be a factor
    
    GtestL <- daisy(x = as.data.frame(testL[,-1]), metric = 'gower') # Calculate gowers similarity index for each list element.
    gmatL <- cbind(testL$cell, as.matrix(GtestL)) # Convert dissimilarity objects to matrix.
    # message("gmatl <-  cbind(testL$cell, as.matrix(GtestL)) produces:")
    # print(gmatL)

    finalgL <- 1-gmatL[gmatL[,1] == cCellnum[i],] # select the row of similarity indices with cell numbers equal to the cell number of the cLHS point, then convert from dissimilarity to similarity by subtracting from 1.
    
    similarL[[i]] <- cbind(testL$cell, finalgL[-1]) # combine the cellnumbers of the raster to the similarity index. 
  } else {
    message("It's empty!")
  }
  
  
  # Print out the iteration number that it is currently working on. 
  cat("Iteration: ", i, "  | ",date(), "\n")
  flush.console()
  
  # Note this returns several warning messages from the daisy package. See https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/daisy.html for more information on these warnings.
}
## Error in daisy(x = as.data.frame(testL[, -1]), metric = "gower") : 
# long vectors (argument 11) are not supported in .Fortran

# Converts list similarL into a dataframe and renames the two columns
s.df <- do.call(rbind.data.frame, similarL) #Convert list of cell numbers and similarity values to a dataframe
names(s.df) <- c('CellNum', 'Similar') #Give better names

# Index the original raster by the cell numbers and replace the NA values with the similarity values. This results in a raster with similarity values in the buffers around each cLHS point and NA everywhere else. 
r[s.df$CellNum] <- s.df$Similar 

plot(r) #you can't really see much with this plot and need to write the raster for viewing in a GIS.  
# writeRaster(r, filename = "WRFO.UtahGasCorp.30m.2kmres.combined.tif")


# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Since points are closer than buffer size, the similarity values overlap making it impossible to assess the similarity of some points. To create a similarity raster for each point, use the list....
 
 
# Create an empty raster with the same number of bands as input points
raster.empty = r # r is the similarity raster for all points together 
raster.empty[] <- NA 
raster.point.stack <- stack(mget(rep("raster.empty", length(csamp)))) #csamp is the input point layer

# For() loop that creates a raster stack where each point buffer has it's own layer so that the similarity buffers don't overlap eachother
for(i in 1:length(similarL)){
similarity.point <- as.data.frame(similarL[[i]]) #Convert list of cell numbers and similarity values to a dataframe
names(similarity.point) <- c('CellNum', 'Similar') #Give better names
raster.point.stack[[i]][similarity.point$CellNum] <- similarity.point$Similar #Index each raster layer and insert corresponding similarity values
}

writeRaster(raster.point.stack, 
            filename = "C:/Users/sfper/Documents/R/WRFO_git/ART.index/WRFO.UtahGasCorp.30m.2kmres.separated.tif",
            overwrite = TRUE)
