# Get phenotypes for georeferenced points
# I'll  add the hapmap identifier  (genotype table) later

library("raster")

# get points
load("../data/grassGEA_geo_loc.Rdata")
map_points
# process tif rasters

data_dir <- "/Volumes/GoogleDrive/My\ Drive/repos/soilP/inst/extdata/ISRIC2011/soilP_raster"

for(file in list.files(data_dir,pattern="sol.*.tif$")){
  trait <- tools::file_path_sans_ext(basename(file))
  print(trait)
  raster_file <- file.path(data_dir,file)
  env_raster <- raster::raster(raster_file) 
  map_points[,trait] <- raster::extract(env_raster,map_points[,c("lon","lat")])
}

cor(map_points[4:8], use = "pairwise.complete.obs")


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


extract_brick <- function(brick, geo_loc, lon ="lon", lat = "lat") {
  for (var_name in names(brick)) {
    geo_loc[var_name] <- raster::extract(x = brick[[var_name]],
                                        y = geo_loc[, c(lon, lat)])
  }
  geo_loc
}




data_dir <- "/Volumes/GoogleDrive/My\ Drive/repos/soilP/inst/extdata/GLOBAL_PHOSPHORUS_DIST_MAP_1223/data"

for(file in list.files(data_dir,pattern=".*.nc$")){
  nc_file <- file.path(data_dir,file)
  map_points <- extract_brick(ncdf2brick(nc_file),
                map_points)
}

map_points
ncol(map_points)

M <- cor(map_points[4:ncol(map_points)], 
    use = "pairwise.complete.obs")

library(corrplot)
quartz()
corrplot(M, type="upper")

