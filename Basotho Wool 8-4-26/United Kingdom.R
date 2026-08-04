library(comtradr)
library(tidyverse)
library(skimr)

#Get API Key Here: https://comtradedeveloper.un.org/profile
#set_primary_comtrade_key("primary key")


united_kingdom_wool_old <- map_df(2010:2020, function(yr) {
  message(paste("Fetching global monthly rows for:", yr))
  
  ct_get_data(
    reporter = "all_countries",
    partner = "GBR",
    commodity_code = c("5101", "5103"),
    flow_direction = "import",
    frequency = "M",                  
    start_date = paste0(yr, "-01"),   
    end_date = paste0(yr, "-12")      
  )
})
united_kingdom_wool_new <- map_df(2021:2025, function(yr) {
  message(paste("Fetching global monthly rows for:", yr))
  
  ct_get_data(
    reporter = "all_countries",
    partner = "GBR",
    commodity_code = c("5101", "5103"),
    flow_direction = "import",
    frequency = "M",                  
    start_date = paste0(yr, "-01"),   
    end_date = paste0(yr, "-12")      
  )
})

uk_wool<-bind_rows(united_kingdom_wool_old,united_kingdom_wool_new) %>% 
  mutate(date = ymd(ref_period_id))
skim_without_charts((uk_wool))
