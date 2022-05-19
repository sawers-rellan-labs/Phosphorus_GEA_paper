# Get phenotypes for georeferenced points
# I'll  add the hapmap identifier  (genotype table) later

library("raster")

# get points
# load("../data/grassGEA_geo_loc.Rdata")
# load("../Phosphorus_GEA_paper/Figures/data/grassGEA_geo_loc.Rdata")
head(map_points)
# process tif rasters

data_dir <- "/Volumes/GoogleDrive/My\ Drive/repos/soilP/inst/extdata/ISRIC2011/soilP_raster"
map_points <- traits
colnames(map_points)
# data_dir <- "/rsstu/users/r/rrellan/sara/gisdata/tif"
#for(file in list.files(data_dir,pattern="sol.*.tif$")){
for(file in list.files(data_dir,pattern=".*.tif$")){
  #file <- "NPlim.tif"
  trait <- tools::file_path_sans_ext(basename(file))
  print(trait)
  raster_file <- file.path(data_dir,file)
  env_raster <- raster::raster(raster_file) 
  map_points[,trait] <- raster::extract(env_raster,map_points[,c("lon","lat")])
}

colnames(map_points)
traits <- map_points

# cor(map_points[4:8], use = "pairwise.complete.obs")


write.csv(map_points, file = "grassGEA_phenotypes.csv")

# process NetCDF data

# convert to raster

get_ORNL_brick <- function(nc_file){
  phospho_nc <- ncdf4::nc_open(nc_file)
  vars <- names(phospho_nc$var)
#  "PBR1.nc" and "PBR2.nc" have the same varriable name: "PBR" nightmare!
# omit *2.nc frrom analysis for  now
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



get_GSDE_brick <- function(nc_file){
  var_name <- tools::file_path_sans_ext(basename(nc_file))
  #  "PBR1.nc" and "PBR2.nc" have the same varriable name: "PBR" nightmare!
  #  omit *2.nc from analysis for  now
  #  getting just the first depth
 brick <- raster::brick(
    raster::raster(nc_file, lvar=4, level=1)
    )
 names(brick) <- var_name
 brick
}


extract_brick <- function(brick, geo_loc, lon ="lon", lat = "lat") {
  for (var_name in names(brick)) {
    geo_loc[var_name] <- raster::extract(x = brick[[var_name]],
                                        y = geo_loc[, c(lon, lat)])
  }
  geo_loc
}


# data_dir <- "/Volumes/GoogleDrive/My\ Drive/repos/soilP/inst/extdata/GLOBAL_PHOSPHORUS_DIST_MAP_1223/data"
data_dir <- "/rsstu/users/r/rrellan/sara/gisdata/Shangguan2014/"
for(file in list.files(data_dir,pattern=".*.nc$")){
  print(file)
  nc_file <- file.path(data_dir,file)
  map_points <- extract_brick(get_GSDE_brick(nc_file),
                map_points)
}

write.csv(map_points, file = "grassGEA_phenotypes.csv")


# data_dir <- "/Volumes/GoogleDrive/My\ Drive/repos/soilP/inst/extdata/GLOBAL_PHOSPHORUS_DIST_MAP_1223/data"
data_dir <- "/rsstu/users/r/rrellan/sara/gisdata//Yang2014/Global_Phosphorus_Dist_Map_1223/data"
file <- "pforms_den.nc"
print(file)
nc_file <- file.path(data_dir,file)
map_points <- extract_brick(get_ORNL_brick(nc_file),
                            map_points)

write.csv(map_points, file = "grassGEA_phenotypes.csv")

library(dplyr)
map_points %>% filter(sp =="Zea mays") %>% summary()
map_points %>% filter(sp =="Sorghum bicolor") %>% summary()


# M <- cor(map_points[4:ncol(map_points)], 
#     use = "pairwise.complete.obs")
# 
# library(corrplot)
# quartz()
# corrplot(M, type="upper")



