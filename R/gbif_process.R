library(data.table)
library(curl)
library(zip)
library(tidyverse)
library(CoordinateCleaner)

dir.create("raw_data/gbif",showWarnings = TRUE)

curl_download("https://api.gbif.org/v1/occurrence/download/request/0001832-210819072339941.zip",
              "raw_data/gbif/gbif.zip",
              quiet=FALSE)
unzip("raw_data/gbif/gbif.zip",exdir="raw_data/gbif")

#aus_gbif<-fread("raw_data/gbif/0344358-200613084148143.csv",nrows = 1000)

aus_gbif<-fread("raw_data/gbif/0001832-210819072339941.csv",quote="")

# modified from: https://data-blog.gbif.org/post/gbif-filtering-guide/

data.frame(aus_gbif) %>%
    setNames(tolower(names(.))) %>% # set lowercase column names to work with CoordinateCleaner
    filter(countrycode  == "AU") %>%
    filter(occurrencestatus  == "PRESENT")  %>%
    filter(basisofrecord %in% c("HUMAN_OBSERVATION","LIVING_SPECIMEN","PRESERVED_SPECIMEN")) %>%
    filter(!is.na(decimallongitude)) %>% 
    filter(!is.na(decimallatitude)) %>% 
    filter(establishmentmeans %in% c("INTRODUCED", "INVASIVE", "NATURALISED","")) %>% #exclude "MANAGED"
    filter(year >= 1900) %>%
    filter(coordinateprecision < 0.05 | is.na(coordinateprecision)) %>% 
    filter(coordinateuncertaintyinmeters < 10000 | is.na(coordinateuncertaintyinmeters)) %>%
    filter(!decimallatitude == 0 | !decimallongitude == 0) ->intermediate_check

#adding some additional filters on the issue column
intermediate_check %>%
filter(!grepl("COUNTRY_COORDINATE_MISMATCH",intermediate_check$issue) & 
           !grepl("RECORDED_DATE_UNLIKELY",intermediate_check$issue)) ->i2

i2 %>%
    cc_sea(ref=buffland) %>% # remove from ocean # doesn't work?
    cc_val() %>% 
    cc_equ() %>%
    cc_gbif() %>%
    cc_cen(buffer = 2000) %>% # remove country centroids within 2km 
    cc_cap(buffer = 2000) %>% # remove capitals centroids within 2km
    cc_inst(buffer = 2000) %>% # remove zoo and herbaria within 2km 
    #cc_coun() %>% #country mismatch with coords #not working
    distinct(decimallongitude,decimallatitude,specieskey,datasetkey, .keep_all = TRUE) -> aus_filt


ggplot(aus_filt,aes(x=decimallongitude,y=decimallatitude,col=genus))+geom_point()

write_csv(aus_filt,"processed_data/filtered_aus_obs.csv")



select(aus_filt,species,decimalLongitude=decimallongitude,decimalLatitude=decimallatitude,scientificname,
       verbatimscientificname,day,month,year) %>%
    write_csv("processed_data/filt_aus_limited_columns.csv")


aus_filt %>%
    filter(genus=="Petrophila") ->protea

