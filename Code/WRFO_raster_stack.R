library(raster)
library(rgdal)

#### CREATING THE INPUT RASTERSTACK FOR GOWER'S SIMILARITY ####

# Setting the workspace where you're pulling files from
setwd("C:/Users/sfper/Documents/R/WRFO_git/ART.index")

# Importing rasters you will be using to create a RasterStack
PSCS <- raster("PSCS_30m_UTM13N_Clip1.tif")
wetness.index <- raster("Topographic_Wetness_Index081720171.tif")

## Importing and rasterizing the shapefile with the soil map units
WRFO.soilmu <- readOGR(dsn = "C:/Users/sfper/Documents/R/WRFO_git/ART.index", 
                       layer = 'soilmu_a_co_clip')
# reprojecting the .shp  to UTM 13N
WRFO.soilmu.UTM13N <- spTransform(WRFO.soilmu, CRS("+proj=utm +zone=13 +ellps=GRS80 +towgs84=0,0,0 +units=m +no_defs")) #Reproject to match rasters
  
    # Convert soil data from vector to raster format
    # First, create a raster that will hold the rasterized soils data. This creates an "empty" raster
  y <- raster(wetness.index)
      # Rasterize the spatialpolygondataframe and make sure that it is a factor
      # takes several minutes
      soil.survey <- rasterize(WRFO.soilmu.UTM13N, field = "MUKEY", y)

## Cropping rasters so they're the same extent otherwise stack() will throw an error
# setting the base extent
extent <- extent(PSCS)
# cropping
PSCS <- crop(PSCS, extent)
soil.survey <- crop(soil.survey, extent)
wetness.index <- crop(wetness.index, extent)

# Since they're originally a different resolution, this aligns the grids so that they can be stacked later
soil.survey.new = resample(x = soil.survey, y = PSCS, method = "ngb")
wetness.index.new = resample(x = wetness.index, y = PSCS, method = "ngb")

# Creating RasterStack for similarity index 
WRFO.composite.30m <- stack(x = c(PSCS, soil.survey.new, wetness.index.new))

# Writing out Raster Stack
writeRaster(WRFO.composite.30m, 
            filename = "C:/Users/sfper/Documents/R/WRFO_git/ART.index/WRFO_composite_30m.tif",
            overwrite = TRUE)
# writing out all raster stack components
writeRaster(PSCS, 
            filename = "C:/Users/sfper/Documents/R/WRFO_git/ART.index/PSC.resample.tif",
            overwrite = TRUE)
writeRaster(soil.survey.new, 
            filename = "C:/Users/sfper/Documents/R/WRFO_git/ART.index/soil.survey.resample.tif",
            overwrite = TRUE)
writeRaster(wetness.index.new, 
            filename = "C:/Users/sfper/Documents/R/WRFO_git/ART.index/wetness.index.resample.tif",
            overwrite = TRUE)