##===============================#
## Process google mobility data
## Author: Guido España
## 2020
##===============================#
## Setup-------------
##===============================#
library(dplyr)
library(tidyverse)
library(mgcv)
library(MASS)
library(signal)

forecast_date = "2022-06-30"
reps_in = 500
redownload_in = 0
args = (commandArgs(TRUE))

if(length(args) >= 1){
    forecast_date = args[1]
    if(length(args) >= 2){
        reps_in = as.numeric(reps_in)
    }
}
forecast_date = as.Date(forecast_date)

##===============================#
## Read data-------------
##===============================#
interventions_df = read_csv('./input_files/interventions_Colombia.csv')
if(redownload_in == 1){
    ## Update file
    download.file('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=911a386b6c9c230f',
                  './input_files/USA_google_movement_data.csv')
}

if(!file.exists('./input_files/USA_google_movement_data.csv')){
    stop("Incidence data not found")
}

google_data = read_csv('./input_files/USA_google_movement_data.csv') %>%
    dplyr::filter(sub_region_1 %in% interventions_df$state_name) %>%
    mutate(day = as.numeric(date - min(date))) %>%
    dplyr::select(ends_with('baseline'), date, sub_region_1) %>%
    tidyr::gather(key = category, value = trend_mobility, -c('sub_region_1', 'date'))

categories_google = unique(google_data$category)
layout(matrix(1:length(categories_google), nrow = floor(length(categories_google)/2)))
for(ct in 1:length(categories_google)){
    tmp_data_base = dplyr::filter(google_data, sub_region_1 == 'Bogota', category == categories_google[ct]) %>%
        group_by(date) %>%
        summarize(trend_mobility = mean(trend_mobility, na.rm = T)) 
   ##plot(tmp_data_base$date, tmp_data_base$trend_mobility, type = "l", ylab = categories_google[ct], xlim = c(as.Date('2021-03-01'), as.Date('2021-05-11')))
   plot(tmp_data_base$date, tmp_data_base$trend_mobility, type = "l", ylab = categories_google[ct])
   abline(h = 1)
   abline(v = c(as.Date('2021-04-29')), col = 'red')
}

##===============================#
## Fit to data-------------
##===============================#
##ct = "residential_percent_change_from_baseline"
ct = "transit_stations_percent_change_from_baseline"
##ct = "grocery_and_pharmacy_percent_change_from_baseline"
shelter_df = tibble()
shelter_lockdown_df = tibble()
for(ss in 1:nrow(interventions_df)){
    tmp_data_base = dplyr::filter(google_data, category == ct, sub_region_1 == interventions_df$state_name[ss]) %>%
        group_by(date) %>% summarize(trend_mobility = -mean(trend_mobility, na.rm = T)) %>%
        ungroup() %>%
        mutate(trend_mobility = (trend_mobility - min(trend_mobility))) %>%
        mutate(trend_mobility = trend_mobility/max(trend_mobility) * 100)
    
    tmp_data_base = tmp_data_base[1:(nrow(tmp_data_base) - 0),]    
    ##min_mobility = min(tmp_data_base$trend_mobility[tmp_data_base$date < as.Date('2020-03-30')])
    ##    tmp_data_base$trend_mobility = (tmp_data_base$trend_mobility - min_mobility)/100
    
    tmp_data_base$trend_mobility = tmp_data_base$trend_mobility / 100    
    tmp_data_base$day = 1:nrow(tmp_data_base)

    ## Seasonal components
    bf <- butter(2, 1/15,type = "low")
    bfh <- butter(2, 1/15,type = "high")
    bfq <- butter(2, 1/90,type = "low")
    bfhq <- butter(2, 1/90,type = "high")
    
    b <- signal::filter(bf, tmp_data_base$trend_mobility)
    bh <- signal::filter(bfh, tmp_data_base$trend_mobility)
    bl_q <- signal::filter(bfq, b)
    bh_q <- signal::filter(bfhq, b)

    seasonal_mobility_signal = bh_q
    
    plot(tmp_data_base$date, tmp_data_base$trend_mobility, type = 'l')

    lines(tmp_data_base$date, b, col = 'red')
    lines(tmp_data_base$date, bh, col = 'green')
    lines(tmp_data_base$date, bl_q, col = 'blue')
    lines(tmp_data_base$date, seasonal_mobility_signal, col = 'magenta')

    ## Xmas season    
    tmp_data_base_xmas = tmp_data_base[tmp_data_base$date > as.Date('2020-12-19'),]
    tmp_data_base_pre = tmp_data_base[tmp_data_base$date <= as.Date('2020-03-19'),]    
    tmp_data_base = tmp_data_base[tmp_data_base$date <= as.Date('2020-12-24') & tmp_data_base$date > as.Date('2020-03-19'),]
    
    ## Fit two models, one before xmas, one after xmas
    mod_gam0 = gam(trend_mobility ~ s(day), data=tmp_data_base_pre, family=gaussian(link='identity'))
    mod_gam1 = gam(trend_mobility ~ s(day), data=tmp_data_base, family=gaussian(link='identity'))
    mod_gam2 = gam(trend_mobility ~ s(day), data=tmp_data_base_xmas, family=gaussian(link='identity'))


    ## Predict only based on last months trends
    tmp_shelter_pre = tibble(date = tmp_data_base_pre$date)    
    tmp_shelter_pre$day = (1:nrow(tmp_shelter_pre))     
    tmp_shelter_pre$shelter_trend = predict(mod_gam0, newdata = tmp_shelter_pre)
    
    tmp_shelter = tibble(date = tmp_data_base$date)    
    tmp_shelter$day = 1:nrow(tmp_shelter) + max(tmp_data_base_pre$day)
    tmp_shelter$shelter_trend = predict(mod_gam1, newdata = tmp_shelter)
    
    tmp_shelter_xmas = tibble(date = tmp_data_base_xmas$date)    
    ##tmp_shelter_xmas$day = (1:nrow(tmp_shelter_xmas))  + max(tmp_shelter_pre$day)
    tmp_shelter_xmas$day = as.numeric(tmp_shelter_xmas$date - min(tmp_data_base_pre$date)) + 1
    tmp_shelter_xmas$shelter_trend = predict(mod_gam2, newdata = tmp_shelter_xmas)

    tmp_shelter_xmas = dplyr::filter(tmp_shelter_xmas, date > max(tmp_data_base$date))
    tmp_data_base_xmas = dplyr::filter(tmp_data_base_xmas, date > max(tmp_data_base$date))
    ## Combine again
    tmp_shelter = bind_rows(tmp_shelter_pre,tmp_shelter, tmp_shelter_xmas)
    tmp_data_base = bind_rows(tmp_data_base_pre, tmp_data_base, tmp_data_base_xmas)
        
    ##tmp_shelter$shelter_trend = tmp_shelter$shelter_trend - min(tmp_shelter$shelter_trend)
    ##tmp_shelter$shelter_trend = tmp_shelter$shelter_trend / max(tmp_shelter$shelter_trend)

    current_trends = dplyr::filter(tmp_shelter, date >= max(tmp_data_base$date) - 30)
    current_trends$day = 1:nrow(current_trends)
    mod_lm = lm(shelter_trend ~ day, data = current_trends)

    pred_data = tibble(date = seq(from=min(current_trends$date), to = forecast_date, by = 1)) 
    pred_data$day = 1:nrow(pred_data)    
    pred_data$shelter_trend = predict(mod_lm, newdata = pred_data)

    pred_data = pred_data %>% dplyr::filter(date > max(tmp_shelter$date))
    
    shelter_reps = bind_rows(tmp_shelter, pred_data) %>%
        mutate(replicate = 1,
               State = interventions_df$State[ss],
               state_name = interventions_df$state_name[ss]) %>%   
        dplyr::select(date, replicate, shelter_trend, State, state_name)

              
    shelter_reps$shelter_trend[shelter_reps$date > as.Date('2021-05-07') & shelter_reps$date <= as.Date('2021-05-21')] = min(shelter_reps$shelter_trend[shelter_reps$date > as.Date('2020-12-01') & shelter_reps$date <= as.Date('2021-01-01')])

    forecast_indx = which(shelter_reps$date > max(tmp_data_base$date) & shelter_reps$date < max(tmp_data_base$date) + 365)
    shelter_reps$shelter_trend[forecast_indx] = shelter_reps$shelter_trend[shelter_reps$date == max(tmp_data_base$date)] + seasonal_mobility_signal[forecast_indx - 365+7] - seasonal_mobility_signal[forecast_indx-365+7][1]
    
    shelter_reps$shelter_trend[shelter_reps$shelter_trend < 0] = 0.0


    
    ## Adjust for lockdowns
    lockdown_mobility_21 = max(shelter_reps$shelter_trend[shelter_reps$date > as.Date('2021-03-30') & shelter_reps$date < as.Date('2021-04-16')])

    lockdown_strategy = rep(c(0,0,0,1,1,1,1), 3)
    lockdown_dates = seq(from = as.Date('2021-07-05'), by = 'day', length.out = length(lockdown_strategy))
    shelter_lockdown_reps = shelter_reps
    
    shelter_lockdown_reps$shelter_trend[shelter_lockdown_reps$date %in% lockdown_dates[lockdown_strategy == 0]] = lockdown_mobility_21        
    
    shelter_df = bind_rows(shelter_df, shelter_reps)
    ##shelter_df$shelter_trend = shelter_df$shelter_trend - min(shelter_df$shelter_trend)

    shelter_lockdown_df = bind_rows(shelter_lockdown_df, shelter_lockdown_reps)
    shelter_lockdown_df$shelter_trend = shelter_lockdown_df$shelter_trend - min(shelter_lockdown_df$shelter_trend)
}

write_csv(shelter_df,'./input_files/interventions_covid_timevarying_shelter.csv')
write_csv(shelter_lockdown_df,'./input_files/interventions_covid_timevarying_shelter_lockdown.csv')

plot(shelter_df$date, shelter_df$shelter_trend, type = 'l', ylim = c(-1,2))
lines(tmp_data_base$date, tmp_data_base$trend_mobility, col = 'red')
lines(shelter_df$date, shelter_df$shelter_trend, type = 'l')
##lines(shelter_lockdown_df$date, shelter_lockdown_df$shelter_trend, type = 'l', col = 'green')

##===============================#
## Process ESC weights -----------
##===============================#
## This script depends on the mobility trends file being created from grandata
## Not updated here because the grandata website sometimes freezes
grandata_mov_df = read_csv('./input_files/11001_grandata_mobility_trends.csv')

city_pop = grandata_mov_df %>%
    group_by(SCACODIGO) %>%
    summarize(pop = mean(pop)) %>%
    ungroup() %>%
    summarize(pop = sum(pop))

## Define each ESC contribution
esc_weight_df = grandata_mov_df %>%
    group_by(date) %>%
    mutate(shelter_weight = shelter_trend * pop / sum(shelter_trend * pop)) %>%
    ungroup() %>%
    mutate(shelter_weight = ifelse(is.na(shelter_weight), 0, shelter_weight))

last_weight = dplyr::filter(esc_weight_df, date == max(esc_weight_df$date)) %>%
    dplyr::select(-date)

missing_dates = expand.grid(date = seq(from = max(esc_weight_df$date) + 1, to = max(shelter_df$date), by = 1), ind = 1:nrow(last_weight))

esc_weight_df = bind_rows(esc_weight_df, mutate(last_weight[missing_dates$ind,], date = missing_dates$date))

esc_shelter_df = esc_weight_df %>% dplyr::select(date, SCACODIGO, pop, shelter_weight) %>%
    left_join(shelter_df, by = 'date') %>%
    mutate(shelter_pop = shelter_trend * city_pop$pop) %>%
    mutate(shelter_esc_trend = shelter_weight * shelter_trend * city_pop$pop / pop) %>%
    mutate(shelter_esc_trend = ifelse(shelter_esc_trend > 1, 1, shelter_esc_trend)) %>%
    mutate(shelter_trend = shelter_esc_trend) %>%
    dplyr::select(-shelter_esc_trend)

write_csv(esc_shelter_df,'./input_files/11001_mobility_trends.csv')


##===============================#
## Process ESC lockdown  -----------
##===============================#
## This script depends on the mobility trends file being created from grandata
## Not updated here because the grandata website sometimes freezes
grandata_mov_df = read_csv('./input_files/11001_grandata_mobility_trends.csv')

city_pop = grandata_mov_df %>%
    group_by(SCACODIGO) %>%
    summarize(pop = mean(pop)) %>%
    ungroup() %>%
    summarize(pop = sum(pop))

## Define each ESC contribution
esc_weight_df = grandata_mov_df %>%
    group_by(date) %>%
    mutate(shelter_weight = shelter_trend * pop / sum(shelter_trend * pop)) %>%
    ungroup() %>%
    mutate(shelter_weight = ifelse(is.na(shelter_weight), 0, shelter_weight))

last_weight = dplyr::filter(esc_weight_df, date == max(esc_weight_df$date)) %>%
    dplyr::select(-date)

missing_dates = expand.grid(date = seq(from = max(esc_weight_df$date) + 1, to = max(shelter_lockdown_df$date), by = 1), ind = 1:nrow(last_weight))

esc_weight_df = bind_rows(esc_weight_df, mutate(last_weight[missing_dates$ind,], date = missing_dates$date))

esc_shelter_df = esc_weight_df %>% dplyr::select(date, SCACODIGO, pop, shelter_weight) %>%
    left_join(shelter_lockdown_df, by = 'date') %>%
    mutate(shelter_pop = shelter_trend * city_pop$pop) %>%
    mutate(shelter_esc_trend = shelter_weight * shelter_trend * city_pop$pop / pop) %>%
    mutate(shelter_esc_trend = ifelse(shelter_esc_trend > 1, 1, shelter_esc_trend)) %>%
    mutate(shelter_trend = shelter_esc_trend) %>%
    dplyr::select(-shelter_esc_trend)

write_csv(esc_shelter_df,'./input_files/11001_mobility_trends_lockdown.csv')


##===============================#
## Baseline community-------------
##===============================#
file.copy('../input_files/community_survey_11001.csv', './input_files/community_survey_11001.csv', overwrite = T)
## Cuarentenas:
## 10 - 13 cuarentena hasta el tres de mayo
survey_df = read_csv('./input_files/community_survey_11001.csv') %>%
    mutate(Day = as.numeric(Date - as.Date('2020-01-01')))
survey_df = survey_df[-1,]
ct = "grocery_and_pharmacy_percent_change_from_baseline"
community_df = tibble()
community_baseline_df = tibble()

for(ss in 1:nrow(interventions_df)){
    tmp_data_base = dplyr::filter(google_data, category == ct, sub_region_1 == interventions_df$state_name[ss]) %>%
        group_by(date) %>% summarize(trend_mobility = mean(trend_mobility, na.rm = T)) %>%
        ungroup() 
    tmp_data_base = tmp_data_base[1:(nrow(tmp_data_base)),]

    ## tmp_data_base$trend_mobility[tmp_data_base$date < as.Date('2020-05-15')] = tmp_data_base$trend_mobility[tmp_data_base$date == as.Date('2020-05-15')]    
    ## tmp_data_base$trend_mobility = tmp_data_base$trend_mobility + abs(tmp_data_base$trend_mobility[tmp_data_base$date == as.Date('2020-05-15')])
        
    tmp_data_base$trend_mobility = tmp_data_base$trend_mobility / 100   
    tmp_data_base$day = 1:nrow(tmp_data_base)

    tmp_data_base_pre = tmp_data_base[tmp_data_base$date <= as.Date('2020-03-19'),]    
    tmp_data_base_xmas = tmp_data_base[tmp_data_base$date > as.Date('2020-12-10'),]
    
    ## min_trend = min(survey_df$Contact_unknown[survey_df$Date > as.Date('2021-01-01')]) * 1.0
           
    tmp_data_base = tmp_data_base[tmp_data_base$date <= as.Date('2020-12-24') & tmp_data_base$date > as.Date('2020-03-19'),]

    ## Fit two models, one before xmas, one after xmas
    mod_gam1 = gam(trend_mobility ~ s(day), data=tmp_data_base, family=gaussian(link='identity'))
    mod_gam2 = gam(trend_mobility ~ s(day), data=tmp_data_base_xmas, family=gaussian(link='identity'))
    mod_gam0 = gam(trend_mobility ~ s(day), data=tmp_data_base_pre, family=gaussian(link='identity'))

    ## Predict only based on last months trends
    tmp_community_pre = tibble(date = tmp_data_base_pre$date)    
    tmp_community_pre$day = (1:nrow(tmp_community_pre))     
    tmp_community_pre$community_trend = predict(mod_gam0, newdata = tmp_community_pre)

    tmp_community = tibble(date = tmp_data_base$date)    
    tmp_community$day = 1:nrow(tmp_community)  + max(tmp_data_base_pre$day)          
    tmp_community$community_trend = predict(mod_gam1, newdata = tmp_community)
    
    tmp_community_xmas = tibble(date = tmp_data_base_xmas$date)    
    ##tmp_community_xmas$day = (1:nrow(tmp_community_xmas)) + max(tmp_community$day)
    tmp_community_xmas$day = as.numeric(tmp_community_xmas$date - min(tmp_community_pre$date)) + 1
    tmp_community_xmas$community_trend = predict(mod_gam2, newdata = tmp_community_xmas)

    tmp_community_xmas = dplyr::filter(tmp_community_xmas, date > max(tmp_data_base$date))
    tmp_data_base_xmas = dplyr::filter(tmp_data_base_xmas, date > max(tmp_data_base$date))
    
    ## Combine again
    tmp_community = bind_rows(tmp_community_pre,tmp_community, tmp_community_xmas)
    tmp_data_base = bind_rows(tmp_data_base_pre, tmp_data_base, tmp_data_base_xmas)
        
    current_trends = dplyr::filter(tmp_community, date >= max(tmp_data_base$date) - 14)
    current_trends$day = 1:nrow(current_trends)
    mod_lm = lm(community_trend ~ day, data = current_trends)

    pred_data = tibble(date = seq(from=min(current_trends$date), to = forecast_date, by = 1)) 
    pred_data$day = 1:nrow(pred_data)    
    pred_data$community_trend = predict(mod_lm, newdata = pred_data)
    pred_data$community_trend[pred_data$community_trend > max(tmp_community$community_trend)] = max(tmp_community$community_trend)
    pred_data = pred_data %>% dplyr::filter(date > max(tmp_community$date))
    
    community_reps = bind_rows(tmp_community, pred_data) %>%
        mutate(replicate = 1,
               State = interventions_df$State[ss],
               state_name = interventions_df$state_name[ss]) %>%   
        dplyr::select(date, replicate, community_trend, State, state_name)
        
    nov_data = mean(tmp_data_base$trend_mobility[tmp_data_base$date >=as.Date('2020-11-01') & tmp_data_base$date <= as.Date('2020-12-01')])
    
    mar_data = max(community_reps$community_trend[community_reps$date >=as.Date('2021-03-01') & community_reps$date <= as.Date('2021-04-01')])

    community_reps$community_trend[community_reps$date > as.Date('2020-12-24') & community_reps$date < as.Date('2021-03-01') & community_reps$community_trend < nov_data] = nov_data
    
##    community_reps$community_trend[community_reps$date > as.Date('2021-05-01')  & community_reps$community_trend > mar_data] = mar_data

    
    community_reps$community_trend[community_reps$date > as.Date('2021-01-01') & community_reps$date < as.Date('2021-05-14')] = nov_data

    community_reps$community_trend[community_reps$date >= min(pred_data$date)] = pred_data$community_trend[1]

    
    community_reps$community_trend[community_reps$date > as.Date('2021-03-27') & community_reps$date <= as.Date('2021-04-01')] = tmp_data_base$trend_mobility[tmp_data_base$date > as.Date('2021-03-27') & tmp_data_base$date <= as.Date('2021-04-01')]

    ## Reopen everything in june 8
    ##community_reps$community_trend[community_reps$date > as.Date('2021-06-21')]  = max(community_reps$community_trend[community_reps$date >= as.Date('2021-06-07')] * 1.0)
    
    
    tmp_data_base$trend_mobility = tmp_data_base$trend_mobility - max(tmp_data_base$trend_mobility) + 1
    community_reps$community_trend = community_reps$community_trend - max(community_reps$community_trend) + 1


    community_reps$community_trend[community_reps$date > as.Date('2020-12-24')] = community_reps$community_trend[community_reps$date > as.Date('2020-12-24')]
    
    community_reps$community_trend[community_reps$community_trend < 0] = 0
    community_reps$community_trend[community_reps$community_trend > 1] = 1

    ## Adjust for high variation in protests
    ## community_reps$community_trend[community_reps$date > as.Date('2021-05-03') & community_reps$date <= max(tmp_data_base$date)] = tmp_data_base$trend_mobility[tmp_data_base$date > as.Date('2021-05-03')]

    community_reps_high = community_reps
    community_reps_high$community_trend[community_reps_high$date > as.Date('2020-12-24')] = community_reps_high$community_trend[community_reps_high$date > as.Date('2020-12-24')] + 0.2    
    community_reps_high$community_trend[community_reps_high$community_trend > 1] = 1
    
    
    community_baseline_df = bind_rows(community_baseline_df, community_reps)
    community_df = bind_rows(community_df, community_reps_high)
}

plot(community_baseline_df$date, community_baseline_df$community_trend, type = 'l')
lines(tmp_data_base$date, tmp_data_base$trend_mobility, col = 'red')
lines(community_baseline_df$date, community_baseline_df$community_trend, type = 'l')

abline(v = as.Date('2021-05-14'))
write_csv(community_baseline_df,'./input_files/interventions_covid_timevarying_community_baseline.csv')
write_csv(community_df,'./input_files/interventions_covid_timevarying_community.csv')

