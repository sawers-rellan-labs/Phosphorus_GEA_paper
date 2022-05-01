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

colnames(id_map)
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

# Use packages to make this.
# library(geonames)
# library(ISOcodes)
install.packages("geonames")
install.packages("ISOcodes")

library(geonames)

options(geonamesUsername="faustovrz")

get_country_df <- function(x,y){ 
  lapply(1:(length(x)), FUN = function(idx){
    lon = x[idx]
    lat = y[idx]
    if(!is.na(lon) & !is.na(lat)){
      geonames::GNcountryCode( lng = lon , lat = lat, radius = 10) %>%
        as.data.frame()
    } else{
      data.frame(languages = NA, distance = NA, 
           countryCode = NA, countryName = NA)
    }
  }) %>% dplyr::bind_rows()
}


library(sp)
library(rworldmap)
library(ISOcodes)

get_country_df <- function(x,y){ 
  
  countriesSP <- getMap(resolution='low')
  
  points <- data.frame(lon = 4, lat = 72)
  
  NApoint <- SpatialPoints(points,
                           proj4string=CRS(proj4string(countriesSP))) 
  forNA <- over(pointsSP, countriesSP)
  forNA[1,] <- NA

  
  lapply(1:(length(x)), FUN = function(idx){
    
    if(!is.na(x) & !is.na(y)){
      points <-data.frame( lon = x[idx], lat = y[idx])
      pointsSP <- SpatialPoints(points,
                               proj4string=CRS(proj4string(countriesSP))) 
      over(pointsSP, countriesSP)
    }
    else{
      forNA
     }
  }) %>% dplyr::bind_rows()
}




ISO_3166_1



maize_country <- get_country_df(x = geo_loc$locations_longitude,
               y = geo_loc$locations_latitude)

UNM49 <- read.csv(url("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")) 
colnames(UNM49)
region <- UNM49[,c("alpha.3","region","intermediate.region")]

geo_loc<- geo_loc %>%
  dplyr::left_join(region, by = c("countries_country_code3" = "alpha.3" ))
colnames(geo_loc)
table(geo_loc$countries_country_code3)

