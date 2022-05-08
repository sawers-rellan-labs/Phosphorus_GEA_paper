# Get phenotypes for georeferenced points
# I'll  add the hapmap identifier  (genotype table) later

library("raster")

# process tif rasters

sol_file <- "/Volumes/GoogleDrive/My\ Drive/repos/soilP/inst/extdata/ISRIC2011/soilP_raster/sol.tif"

# FAO74_file <- 
sol <- raster::raster(sol_file) 

map_points$sol <- raster::extract(sol,map_points[,c("lon","lat")])


# process NetCDF data

# convert to raster

ncdf2brick <- function(nc_file){
  phospho_nc <- ncdf4::nc_open(nc_file)
  vars <- names(phospho_nc$var)
  lst_by_var <- lapply(
    1:length(vars),
    function(i) {
      raster::brick(nc_file, varname = vars[i])
    }
  )
 brick <- raster::brick(lst_by_var)
 names(brick) <- vars
 brick
}


