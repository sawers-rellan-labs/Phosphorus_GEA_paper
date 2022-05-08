library(dplyr)
library(ggplot2)

data_dir<- file.path("..","data")

# These files come from my previous work on soilP extdata folder
# I have yet to document sources and preprocessing

# there is a datavarse link to this file
# the germinate database is currently down.
#  accn_file <- file.path(data_dir, "germinate_SeeD_GWAS_GBS_4022.tab") 

# this comes from an excel file from the CYMMIT dataverse
# "Mapping file - Sample ID to Germplasm ID.xlsx" ?
# "Original Mapping file Sample ID to Germplasm ID_original.xlsx" ?
# id_map_file <-file.path(data_dir, "SEED_GWAS_GID.tab") 

# this is from the Chr01 inputed genotypes hapmap file
# AllZeaGBSv2.7_SEED_Beagle4_chr1.hmp.zip
# GWAS_id_file <- file.path(data_dir,"GWAS_ID.tab")


# accn_info <- read.table(file = accn_file, header = TRUE, quote = "",sep ="\t")
# id_map <- read.table(file = id_map_file, header = TRUE, quote = "", sep = "\t")
# GWAS_id <- read.table(file = GWAS_id_file, header = FALSE, quote = "", sep ="\t") 

# colnames(id_map)

accn_csv <- file.path(data_dir, "germinate_SeeD_GWAS_GBS_4022.csv")
id_map_csv <-file.path(data_dir, "SEED_GWAS_GID.csv")
GWAS_id_csv <- file.path(data_dir,"GWAS_ID.csv")


# writing to CSV format for easier previews with quicklook in Mac 

# write.csv(accn_info, file= accn_csv, row.names = FALSE)
# write.csv(id_map, file= id_map_csv, row.names = FALSE)
# write.csv(GWAS_id, file= GWAS_id_csv, row.names = FALSE)

accn_info <- read.csv(file = accn_csv, header = TRUE, sep =",")
id_map <- read.table(file = id_map_csv,  header = TRUE, sep =",")
GWAS_id <- read.table(file = GWAS_id_csv, header = TRUE, sep =",") 


#adding back colons to GWAS IDs as they are used in the hapmap sample column
id_map <- id_map %>% 
  dplyr::left_join(GWAS_id,by = c("sample_id" = "V1")) %>%
  dplyr::mutate(taxa = paste(sample_id,V2,V3,V4, sep =":"))


geo_loc<- id_map %>% 
  dplyr::left_join(accn_info,by = c("accn_GID" = "general_identifier")) %>%
  dplyr::arrange(sample_id) %>%
  dplyr::select(bank_number,taxa,locations_region,countries_country_code3,starts_with("locations"))


# library(geonames)
# 
# options(geonamesUsername="faustovrz")
# 
# get_country_geonames <- function(x,y){ 
#   lapply(1:(length(x)), FUN = function(idx){
#     lon = x[idx]
#     lat = y[idx]
#     if(!is.na(lon) & !is.na(lat)){
#       geonames::GNcountryCode( lng = lon , lat = lat, radius = 10) %>%
#         as.data.frame()
#     } else{
#       data.frame(languages = NA, distance = NA, 
#            countryCode = NA, countryName = NA)
#     }
#   }) %>% dplyr::bind_rows()
# }


library(sp)
library(rworldmap)
library(ISOcodes)

get_country_df <- function(x){ 
  
  points <- x %>% 
    dplyr::filter(!is.na(LAT) & !is.na(LON)) %>% 
    dplyr::arrange(LON,LAT) %>%
    dplyr::distinct()
  nrow(x)
  countriesSP <- getMap(resolution = "high")

  pointsSP <- SpatialPoints(points, proj4string=CRS(proj4string(countriesSP))) 

  country <- sp::over(pointsSP, countriesSP)
  country$LON <- points$LON
  country$LAT <- points$LAT
  
  x %>% dplyr::left_join(
    country
  )
}


maize_country <- get_country_df(
 data.frame(
  LON = geo_loc$locations_longitude,
  LAT = geo_loc$locations_latitude)
)

summary(maize_country)

# UNM49 Central america
# Standard country or area codes for statistical use (M49)
# https://unstats.un.org/unsd/methodology/m49/

region <- subset(UN_M.49_Regions, Name == "Central America")
codes <- unlist(strsplit(region$Children, ", "))
CA <- subset(UN_M.49_Countries, Code %in% codes)

CA_maize <- subset(maize_country, ISO3.1 %in% CA$ISO_Alpha_3)

# UNEP GEO-3 "Meso-America"
# Global Environment Outlook 3 Report
# https://www.unep.org/resources/global-environment-outlook-3

MESO_maize <- subset(maize_country, GEO3 == "Meso-America")

summary(CA_maize$ISO3.1)

summary(MESO_maize$ISO3.1)

# The terms seem to behave identically in this set


sb_latlon_file <- file.path(data_dir, "hapmap_geo_loc.tassel")

sb_geo_loc <- read.table(sb_latlon_file, header = TRUE)

colnames(sb_geo_loc)[1] <- "Taxa"


sb_geo_loc


sb_country <- get_country_df(
  data.frame(
    LON = sb_geo_loc$lon,
    LAT = sb_geo_loc$lat)
)

summary(sb_country)

map_points <- rbind(
    data.frame(
      sp = "Zea mays",
      lon = maize_country$LON,
      lat = maize_country$LAT
  ),
  data.frame(
    sp = "Sorghum bicolor",
    lon = sb_country$LON,
    lat = sb_country$LAT
  )
) %>%
  dplyr::filter(!is.na(lon) & !is.na(lat))
  

map_points
table(map_points$sp)

####################################################################

library("raster")


sol_file <- "/Volumes/GoogleDrive/My\ Drive/repos/soilP/inst/extdata/ISRIC2011/soilP_raster/sol.tif"

# FAO74_file <- 
sol <- raster::raster(sol_file) 

map_points$sol <- raster::extract(sol,map_points[,c("lon","lat")])



zm_sol <- map_points %>% 
  dplyr::filter(sp == "Zea mays") %>% 
  dplyr::pull(sol)

sb_sol <- map_points %>% 
  dplyr::filter(sp == "Sorghum bicolor") %>% 
  dplyr::pull(sol)

zmh <- map_points %>% 
  dplyr::filter(sp == "Zea mays") %>% 
  ggplot2::ggplot(aes(x=sol)) + 
  ggplot2::xlab("Solubility") +
  ggplot2::ylab("Frequency") +
  ggplot2::geom_histogram( binwidth =1) + 
  ggplot2::scale_x_continuous(breaks= 1:12) +
  ggpubr::theme_classic2()

zmh_grob <-  ggplot2::ggplotGrob(zmh)


sbh <- map_points %>% 
  dplyr::filter(sp == "Sorghum bicolor") %>% 
  ggplot2::ggplot(aes(x=sol)) + 
  ggplot2::xlab("Solubility") +
  ggplot2::ylab("Frequency") +
  ggplot2::geom_histogram( binwidth =1) + 
  ggplot2::scale_x_continuous(breaks= 1:12) +
  ggpubr::theme_classic2()

sbh_grob <-  ggplot2::ggplotGrob(sbh)


world <- ggplot2::map_data("world")


fig1 <- ggplot2::ggplot(data = world) +
  ggplot2::ggtitle("Soil Phosphorus Map") +
  ggplot2::coord_sf(ylim = c(-35, 35), 
                    xlim = c(-105, 85)) + 
  ggplot2::annotation_custom(grob = zmh_grob, xmin = -115, xmax = -75, 
                    ymin = -40, ymax = 0) + 
  ggplot2::annotation_custom(grob = sbh_grob, xmin = -25, xmax = 15, 
                    ymin = -40, ymax = 0) +
  ggplot2::geom_map(map = world, 
                    ggplot2::aes(x =long, y = lat, map_id = region),
                    fill = NA, lwd = 0.2, color = "black") +
  ggplot2::geom_point(data = map_points,  ggplot2::aes( lon, lat, color = sol), size = 0.5) +
  ggplot2::scale_color_viridis_c(breaks = 0:12) +
  ggplot2::labs(color='Solubility')+
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white"),
                 panel.border = element_blank(),
                 panel.grid.major = ggplot2::element_line(colour = 'transparent'),
                 legend.key = ggplot2::element_rect(fill = NA),
                 legend.key.size = ggplot2::unit(2, 'lines'),
                 axis.line=element_blank(),
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 panel.grid.minor=element_blank())



ggsave("../Fig1_SoilMap.png",dpi = 300, height = 7, width =15)

save(map_points, file = "../data/grassGEA_geo_loc.Rdata")
load("../data/grassGEA_geo_loc.Rdata")

