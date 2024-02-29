setwd('/zine/HPC02S1/ex-dveloza/AGORA/apps/fred_colombia_implementation/scripts')
##===============================================================#
## Get mobility data from Grandata
## Author: Diego Veloza
## Date: 2023/10/26
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
library(stringi)
library(stringr)
library(scales)
options(digits = 22,scipen = 999)

##===============================================================#
## files paths -------------
##===============================================================#
metadata_file <- "/zine/HPC02S1/ex-dveloza/AGORA/apps/synthetic_populations/data/param_files/colombia_municipios_metadata.json"
municipios_shp <- '/zine/HPC02S1/ex-dveloza/AGORA/apps/synthetic_populations/data/raw_data/geodata/Colombia_shp/Municipios.shp'
population_data_file <- '/zine/HPC02S1/ex-dveloza/AGORA/apps/synthetic_populations/data/processed_data/popdata/colombia_population_data_municp.csv'
facebook_data_file_0 <- '/zine/HPC02S1/ex-dveloza/AGORA/apps/fred_colombia_implementation/fred_input_files/facebook_open_data/movement-range-data-2020-03-01--2020-12-31.txt'
facebook_data_file <- '/zine/HPC02S1/ex-dveloza/AGORA/apps/fred_colombia_implementation/fred_input_files/facebook_open_data/movement-range-2022-05-22.txt'
gadm_file <- '/zine/HPC02S1/ex-dveloza/AGORA/apps/fred_colombia_implementation/fred_input_files/gadm_data/gadm41_COL_2.shp'

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

colombia_municipalities_info <- colombia_municipalities_info %>%
                                mutate(department_name = if_else(department_name == "ARCHIPIÉLAGO DE SAN ANDRÉS PROVIDENCIA Y",
                                                                            "SAN ANDRÉS Y PROVIDENCIA", department_name)) %>%
                                mutate(mun_name = stri_trans_general(mun_name, "Latin-ASCII"), department_name = stri_trans_general(department_name, "Latin-ASCII"))

##===============================================================#
## Read shp data-------------
##===============================================================#
## Maybe only download shp data if it's not downloaded yet')
mun_shp = rgdal::readOGR(municipios_shp)
gadm_shp = rgdal::readOGR(gadm_file)
population_mun = read_csv(population_data_file)

##===============================================================#
## Read data-------------
##===============================================================#
facebook_data_0 <- read.csv(facebook_data_file_0, sep = '\t') %>% 
                                dplyr::filter(country == 'COL') %>% 
                                mutate(polygon_name = toupper(polygon_name))

facebook_data <- read.csv(facebook_data_file, sep = '\t') %>% 
                                dplyr::filter(country == 'COL') %>% 
                                mutate(polygon_name = toupper(polygon_name))

facebook_data <- bind_rows(facebook_data_0, facebook_data)

facebook_data$num_dpto <- as.numeric(str_extract(facebook_data$polygon_id, "(?<=\\.)\\d+"))
facebook_data$num_mun  <- as.numeric(str_extract(facebook_data$polygon_id, "(?<=\\.)\\d+(?=_1)"))

facebook_data <- facebook_data %>%
  mutate(
    num_dpto = case_when(
      num_dpto >= 5 ~ num_dpto + 1,
      TRUE ~ num_dpto
    ),
    num_dpto = case_when(
      polygon_name == "SANTAFÉ DE BOGOTÁ" ~ 5,
      TRUE ~ num_dpto
    ),
    num_mun = case_when(
      polygon_name == "SANTAFÉ DE BOGOTÁ" ~ 1,
      TRUE ~ num_mun
    ),
    polygon_name = case_when(
      polygon_name == "SANTAFÉ DE BOGOTÁ" ~ "BOGOTÁ D.C.",
      TRUE ~ polygon_name
    )
  )

gadm_dpto_data <- gadm_shp@data %>% dplyr::select(GID_2, NAME_1, NAME_2) %>%
                mutate(num_dpto_gadm = as.numeric(str_extract(GID_2, "(?<=\\.)\\d+"))) %>%
                #mutate(num_mun = as.numeric(str_extract(GID_2, "(?<=\\.)\\d+(?=_2)"))) %>%
                rename("true_polygon_id" = "GID_2", "name_dpto" = "NAME_1", "name_mun" = "NAME_2") %>%
                dplyr::select(num_dpto_gadm, name_dpto) %>%
                distinct(num_dpto_gadm, name_dpto) %>%
                mutate(name_dpto = toupper(name_dpto), name_dpto = stri_trans_general(name_dpto, "Latin-ASCII"))


colombia_municipalities_info <- left_join(colombia_municipalities_info, gadm_dpto_data, 
                                            by = c("department_name" = "name_dpto")) 

replacement_values <- c(
  "PURISIMA"                    = "PURISIMA DE LA CONCEPCION",
  "SAN BERNARDINO DE SAHAGUN"   = "SAHAGUN",
  "SANTA CRUZ DE LORICA"        = "LORICA",
  "PUERTO INIRIDA"              = "INIRIDA",
  "SANTA MARTA (DIST. ESP.)"    = "SANTA MARTA",
  "DON MATIAS"                  = "DONMATIAS",
  "LA UNION DE SUCRE"           = "LA UNION",
  "SAN VICENTE"                 = "SAN VICENTE FERRER",
  "SAN LUIS DE CUBARRAL"        = "CUBARRAL",
  "VISTA HERMOSA"               = "VISTAHERMOSA",
  "SAN JOSE DE CUCUTA"          = "CUCUTA",
  "SAN MIGUEL DE MOCOA"         = "MOCOA",
  "SANTIAGO DE CALI"            = "CALI",
  "SAN JUAN DE PASTO"           = "PASTO",
  "SINCE"                       = "SINCELEJO",
  "TOLU"                        = "SANTIAGO DE TOLU",
  "ARMERO"                      = "ARMERO GUAYABAL",
  "TUMACO"                      = "SAN ANDRES DE TUMACO"
)

facebook_data <- facebook_data %>%
                mutate(polygon_name = stri_trans_general(polygon_name, "Latin-ASCII")) %>%
                mutate(polygon_name = case_when(
                    num_dpto == 2 & polygon_name == "BOLIVAR" ~ "CIUDAD BOLIVAR",
                    TRUE ~ recode(polygon_name, !!!replacement_values)
                ))


facebook_data_divipola_code <- left_join(facebook_data, colombia_municipalities_info, by = c("polygon_name" = "mun_name", "num_dpto" = "num_dpto_gadm"))
write_csv(facebook_data_divipola_code, '../input_files/COL_mobility_trends_facebook.csv')

##===============================================================#
## Combine shape files-------------
##===============================================================#
population_mun <- population_mun %>% dplyr::filter(Code != "00", Code != Zone, Year == '2018', Gender == 'Total')

agg_population_mun <- population_mun %>%
    dplyr::filter(Year == '2018', Gender == 'Total') %>%
    group_by(Code, Zone) %>%
    summarize(mun_population = sum(Pop), .groups = 'drop') %>% mutate(Zone = as.numeric(Zone), Code = as.numeric(Code))

agg_population_dept <- agg_population_mun %>%
    group_by(Code) %>%
    summarize(dept_population = sum(mun_population), .groups = 'drop')

dept_list <- unique(population_mun$Code)
for(dept_code in c(15)){
    cat(crayon::red(sprintf("\nCurrent department : %s\n", dept_code)))

    dept_population <- population_mun %>% dplyr::filter(Code == dept_code) %>% mutate(Zone = as.numeric(Zone))
    total_population <- sum(dept_population$Pop)
    mun_list <- unique(dept_population$Zone)

    dept_all_days_list <- list()

    # Loop through each unique date in the facebook_data_divipola_code dataframe
    unique_dates <- unique(facebook_data_divipola_code$ds)
    for(date in unique_dates){
        dept_mobility <- facebook_data_divipola_code %>% 
                    dplyr::filter(department_code == as.numeric(dept_code) & ds == date)
        
        mun_with_data <- unique(dept_mobility$divipola_code)
        mun_without_data <- setdiff(as.numeric(mun_list), as.numeric(mun_with_data))
        
        if(length(mun_without_data) > 0){
            weighted_sum <- sum(dept_mobility$all_day_ratio_single_tile_users * 
                                    dept_population %>% dplyr::filter(as.numeric(Zone) %in% mun_with_data) %>% pull(Pop))
            
            total_pop_with_data <- sum(dept_population %>% dplyr::filter(as.numeric(Zone) %in% mun_with_data) %>% pull(Pop))
            avg_trend <- weighted_sum / total_pop_with_data
            
            mun_without_data_df <- dept_population %>% 
                dplyr::filter(as.numeric(Zone) %in% mun_without_data) %>%
                mutate(
                department_code = as.numeric(dept_code),
                divipola_code = as.numeric(Zone),
                ds = date,
                all_day_ratio_single_tile_users = avg_trend * (Pop / total_pop_with_data)
                ) %>%
                dplyr::select(ds, divipola_code, department_code, all_day_ratio_single_tile_users)
        }
        dept_all_days_list[[length(dept_all_days_list) + 1]] = bind_rows(dept_mobility %>% 
                                                                        dplyr::select(ds, 
                                                                                      divipola_code, 
                                                                                      department_code,
                                                                                      all_day_ratio_single_tile_users), mun_without_data_df)
    }

    dept_mobility <- bind_rows(dept_all_days_list)

    # Joining with the population_mun to add the population per "municipio" and department
    dept_mobility <- dept_mobility %>%
                      left_join(agg_population_mun, by = c("department_code" = "Code", "divipola_code" = "Zone")) %>%
                      left_join(agg_population_dept, by = c("department_code" = "Code")) %>%
                      rename("date" = "ds",
                             #"shelter_trend" = "all_day_ratio_single_tile_users",
                             "shelter_trend" = "all_day_ratio_single_tile_users",
                             "Pop" = "mun_population",
                             "Pop_dpto" = "dept_population")


    write_csv(dept_mobility, sprintf('/zine/HPC02S1/ex-dveloza/AGORA/apps/fred_colombia_implementation/fred_input_files/facebook_mobility_trends/%s_facebook_mobility_trends.csv', dept_code))
}

##===============================================================#
## Plot trends by locality-------------
##===============================================================#
dept_mobility$date <- as.Date(dept_mobility$date)

# Normalizing the values
max_values <- dept_mobility %>%
  group_by(divipola_code) %>%
  summarize(max_value = max(shelter_trend, na.rm = TRUE))

dept_mobility <- dept_mobility %>%
  left_join(max_values, by = "divipola_code") %>%
  mutate(normalized_values = shelter_trend / max_value)

my_colors = brewer.pal(6,"RdYlBu")
my_colors = colorRampPalette(my_colors)(13)
mov_brk <- cut(as.numeric(dept_mobility$shelter_trend), breaks = c(-1,-0.5,-0.2,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,2.0), include.lowest = T, right = F)
my_colors = my_colors[as.numeric(mov_brk)]


# Using ggplot2 for the visualization
p <- ggplot(dept_mobility, aes(x = date, y = normalized_values, color = as.factor(divipola_code))) +
  geom_line() +
  scale_color_manual(values = my_colors) +
  labs(title = "Normalized Mobility Trend by Region", 
       x = "Date", 
       y = "Normalized Mobility Trend", 
       color = "Region Code") +
  theme_minimal()# +
  # theme(legend.position = "bottom")

ggsave(filename = "mobility_plot.png", plot = p, width = 10, height = 6, dpi = 300)


##===============================================================#
## Plot mobility in shape files-------------
##===============================================================#

# For simplicity, let's take the latest mobility data for each region
latest_mobility <- dept_mobility %>%
  group_by(divipola_code) %>%
  arrange(desc(date)) %>%
  slice(1)

mun_shp_sf <- st_as_sf(mun_shp)
mun_shp_sf$ID_ESPACIA <- as.numeric(mun_shp_sf$ID_ESPACIA)

# Merge shapefile with the mobility data
merged_data <- left_join(mun_shp_sf[as.numeric(mun_shp@data$ID_ESPACIA) %in% latest_mobility$divipola_code, ], latest_mobility, by = c("ID_ESPACIA" = "divipola_code"))

# Plotting the map
q = ggplot(data = merged_data) +
  geom_sf(aes(fill = normalized_values), color = "white") + # Assuming normalized_values is what you want to plot
  scale_fill_viridis_c() + # You can use any other color scale
  labs(title = "Latest Normalized Mobility Trends by Region",
       fill = "Mobility") +
  theme_minimal()

ggsave(filename = "mobility_plot.png", plot = q, width = 10, height = 6, dpi = 300)

##===============================================================#
## Just for fun, make a video-------------
##===============================================================#
# Directory to save temporary images
img.dir <- "tmpimg"

# Clean up old images
if(file.exists(img.dir)){
    system(paste('rm -rf ', img.dir,sep = ''))
}
system(paste('mkdir ', img.dir,sep = ''))

# Assuming that 'date_seq' is the sorted unique set of dates in your dataset
date_seq <- sort(unique(merged_data_all_dates$ds))

# Calculate the overall min and max values for normalized_values
overall_min <- min(merged_data_all_dates$normalized_values, na.rm = TRUE)
overall_max <- max(merged_data_all_dates$normalized_values, na.rm = TRUE)

for(dd in 1:length(date_seq)){
    cat(crayon::red(sprintf("\r\b%s", date_seq[dd])))
    filename <- file.path(img.dir, sprintf("mobility_trends_%06d.png", dd - 1))
    png(filename, width = 1024, height = 800)
    
    tmp_df <- merged_data_all_dates %>% dplyr::filter(ds == date_seq[dd])
    
    # Adapted ggplot code with fixed color scale
    q <- ggplot(data = tmp_df) +
        geom_sf(aes(fill = normalized_values), color = "white") + 
        scale_fill_viridis_c(limits = c(overall_min, overall_max)) +  # Fixed scale limits
        labs(title = sprintf("Mobility Trends: %s", date_seq[dd]),
             fill = "Mobility") +
        theme_minimal()
    
    print(q)
    dev.off()
}


# Generate the video
video_name <- "Mobility_trends_animation"

output_movie <- sprintf('%s.mp4', video_name)

ffmpeg_str <- sprintf(
    "ffmpeg -r 3 -i %s/%s -c:v libx264 -r 30 -pix_fmt yuv420p %s -y ", 
    img.dir,  'mobility_trends_%06d.png',   output_movie )

system(ffmpeg_str)
