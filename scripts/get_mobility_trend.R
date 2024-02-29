setwd('/zine/HPC02S1/ex-dveloza/AGORA/apps/fred_colombia_implementation/process_inputs/scripts')
##===============================================================#
## Get mobility data from Grandata
## Author: Guido España
## Date: 2020/09/08
##===============================================================#
## Setup-------------
##===============================================================#
library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(tidyverse)
library(rjson)

library(sf)
library(raster)
library(rgdal)
library(maptools)
library(osmdata)
library(RColorBrewer)
library(osmplotr)
library(RCurl)
library(data.table)
options(digits = 22,scipen = 999)

##===============================================================#
## Read shp data-------------
##===============================================================#
metadata_file <- "/zine/HPC02S1/ex-dveloza/AGORA/apps/synthetic_populations/data/param_files/colombia_municipios_metadata.json"
municipios_shp <- '/zine/HPC02S1/ex-dveloza/AGORA/apps/synthetic_populations/data/raw_data/geodata/Colombia_shp/Municipios.shp'
population_data_file <- '/zine/HPC02S1/ex-dveloza/AGORA/apps/synthetic_populations/data/processed_data/popdata/colombia_population_data_municp.csv'
gadm_file <- '/zine/HPC02S1/ex-dveloza/AGORA/apps/fred_colombia_implementation/input_files/gadm_data/gadm41_COL_2.shp'

##===============================================================#
## Get municipalities metadata-------------
##===============================================================#
json_data = rjson::fromJSON( file = metadata_file,
                             simplify = F)

# Initialize an empty data frame to store the results
colombia_municipalities_info <- data.frame(
  department_name = character(),
  department_code = numeric(),
  mun_name = character(),
  divipola_code = numeric(),
  stringsAsFactors = FALSE
)

# Iterate through the departments
for (department_name in names(json_data$colombia)) {
  
  department_data <- json_data$colombia[[department_name]]
  
  # Iterate through the municipalities within each department
  for (mun_name in names(department_data)) {
    
    mun_data <- department_data[[mun_name]]
    
    department_code <- mun_data$department_code[[1]]
    divipola_code <- mun_data$divipola_code[[1]]
    
    # Create a new row with the extracted data
    new_row <- data.frame(
      department_name = department_name,
      department_code = department_code,
      mun_name = mun_name,
      divipola_code = divipola_code,
      stringsAsFactors = FALSE
    )
    
    # Append the new row to the result data frame
    colombia_municipalities_info <- rbind(colombia_municipalities_info, new_row)
  }
}

##===============================================================#
## Combine shape files-------------
##===============================================================#
dept_list <- unique(colombia_municipalities_info$department_code)
for(dept_code in c(15)){
    cat(crayon::red(sprintf("\nCurrent department : %s\n", dept_code)))

    if(dept_code == 11){
      synth_household_file <- sprintf('../../../synthetic_populations/output/formatted_populations_/colombia_11001/colombia_11001_synth_households.txt', dept_code, dept_code)
    }else{
      synth_household_file <- sprintf('../../../synthetic_populations/output/formatted_populations/colombia_%d/colombia_%d_synth_households.txt', dept_code, dept_code)
    }
    mobility_file <- sprintf('/zine/HPC02S1/ex-dveloza/AGORA/apps/fred_colombia_implementation/fred_input_files/facebook_mobility_trends/%d_facebook_mobility_trends.csv', dept_code)

    house_esc = read_csv(synth_household_file) %>%
        mutate(ESC = as.integer(paste0(dept_code, substr(stcotrbg, nchar(stcotrbg) - 2, nchar(stcotrbg))))) %>%
        group_by(ESC) %>%
        summarize(N = sum(hh_size)) %>%
        mutate(Mun_prop = N / sum(N)) %>%
        ungroup()

    facebook_data <- read.csv(mobility_file) %>%
      #mutate(shelter_trend = ifelse(shelter_trend > 0, 0, shelter_trend)) %>%
      #mutate(shelter_trend = -1*shelter_trend) %>%
      #mutate(shelter_trend = ifelse(shelter_trend > 1, 1, shelter_trend)) %>%
      mutate(Mun_prop = Pop / Pop_dpto) #%>%
        #left_join(house_esc, by = c("divipola_code" = "ESC")) 

    tmp_mov = facebook_data  %>%
              mutate(shelter_trend = shelter_trend * Mun_prop) %>%
              dplyr::select(date, shelter_trend) %>%
              group_by(date) %>%
              summarize(shelter_trend = sum(shelter_trend))

    tmp_mov$date <- as.Date(tmp_mov$date)

    df_mobility <- tmp_mov %>%
      mutate(date = floor_date(date, "day")) %>%
      group_by(date) %>%
      summarise(shelter_trend = mean(shelter_trend, na.rm = TRUE)) %>%
      arrange(date)

    write_csv(df_mobility, sprintf('../../fred_input_files/mobility_trends/%d_mobility_trends.csv', dept_code))
}
