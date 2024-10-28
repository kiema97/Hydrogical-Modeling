library(NASAaccess)
library(terra)
library(tidyterra)
vignette("NEXGDDP-CMIP6",package="NASAaccess")

#Reading input data
dem_path <- system.file("extdata",
                        "DEM_TX.tif", 
                        package = "NASAaccess")

shape_path <- system.file("extdata", 
                          "basin.shp", 
                          package = "NASAaccess")


library(NASAaccess)

NEX_GDDP_CMIP5_DATA <- NEX_GDDP_CMIP5(Dir = "./NEX_GDDP_CMIP5/", 
               watershed = shape_path,
               DEM = dem_path, 
               start = "2006-12-1", 
               end = "2006-12-3",
               model = 'IPSL-CM5A-MR', 
               type = 'tas', 
               slice = 'rcp85')



#Reading input data

dem_path <- system.file("extdata",
                        "DEM_TX.tif",
                        package = "NASAaccess")

shape_path <- system.file("extdata",
                          "basin.shp", 
                          package = "NASAaccess")

#CMIP6  example for air temperature

library(NASAaccess)

NEX_GDDP_CMIP6(
  Dir = "./NEX_GDDP_CMIP6/", 
  watershed = shape_path,
  DEM = dem_path,  
  start = "2060-12-1", 
  end = "2060-12-3",
  model = 'ACCESS-CM2', 
  type = 'tas', 
  slice = 'ssp245')
