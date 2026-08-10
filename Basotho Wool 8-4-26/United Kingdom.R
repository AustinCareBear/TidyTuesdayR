library(comtradr)
library(tidyverse)
library(skimr)

#Get API Key Here: https://comtradedeveloper.un.org/profile
#set_primary_comtrade_key("primary key")

#comtrade API data import ----
#Grab up to 12 months at a time so have to split
#Same code used in tidytuesdayR for Lesotho just changed to GBR
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
#One data frame
uk_wool<-bind_rows(united_kingdom_wool_old,united_kingdom_wool_new) %>% 
  mutate(date = ymd(ref_period_id))

#Alternative method for data:
#uk_wool<-read_csv("Basotho Wool 8-4-26/UK Wool Data.csv")

#Overview of data ----
skim_without_charts((uk_wool))

#Monthly visualization ----
#Data frame for winter months in northern hemisphere used to make blue boxes
winter <- tibble(
  xmin = ymd(paste0(2009:2024, "-12-01")),
  xmax = ymd(paste0(2010:2025, "-02-28")),
  ymin = -Inf,
  ymax = Inf
)

#Similar graph as Lesotho and South Africa just with all GBR exports
uk_wool %>% 
  #20130201 in South Africa was either miss reported or miss entered as it was an extreme outlier
  filter(!(reporter_code==710 & ref_period_id==20130201)) %>% 
  group_by(date) %>% 
  summarise(qty=sum(qty)) %>% 
  ggplot(aes(x=date, y=qty/1000))+
  geom_point(shape = 19)+
  geom_line()+
  geom_rect(
    data = winter,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "lightblue2",
    alpha = 0.4
  )+
  scale_x_date(
    limits = c(ymd("2010-01-01"), NA),
    breaks = seq(
      from = ymd("2010-01-01"),
      to = ymd("2025-01-01"),
      by = "6 months"
    ),
    date_labels = "%b %Y"
  )+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  labs(x="Date",y="Tonnes of Wool", caption = 
         "Wool exported from the Great Britain by month in tonnes.
          The blue boxes indicate winter periods in the northern hemisphere.")

#Broken Out by Continent ----
to_be_joined<-read_csv("Basotho Wool 8-4-26/Country And Region.csv") 

# RUN ONCE
uk_wool<-uk_wool  %>% 
  left_join(to_be_joined, by =c("reporter_iso"="alpha-3") )

rm(to_be_joined)

uk_wool %>% 
  filter(!(reporter_code == 710 & ref_period_id == 20130201)) %>% 
  group_by(date, region) %>% 
  summarise(qty = sum(qty, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = date, y = qty / 1000, color = region)) +
  geom_line() +
  geom_point(shape = 19) +
  scale_x_date(
    limits = c(ymd("2010-01-01"), NA),
    breaks = seq(
      from = ymd("2010-01-01"),
      to = ymd("2025-01-01"),
      by = "6 months"
    ),
    date_labels = "%b %Y"
  )+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date", y = "Tonnes of Wool",color = "Continent", 
       caption = "Great Britain wool exports broken out by continent")+
  scale_color_viridis_d(option = "C")


uk_wool %>% 
  filter(!(reporter_code == 710 & ref_period_id == 20130201)) %>% 
  group_by(ref_year, region) %>% 
  summarise(qty = sum(qty, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = ref_year, y = qty / 1000, color = region)) +
  geom_line() +
  geom_point(shape = 19) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Year", y = "Tonnes of Wool",color = "Continent", 
       caption = "Great Britain wool exports broken out by continent")+
  scale_color_viridis_d(option = "C")


#Net Weight instead of quantity ----
#It seems like net weight might be a more forgiving metric as it does not always have a 
#unit tied to it. Most likely kg but do not want to assume. Data source says kg
uk_wool %>% 
  filter(!(reporter_code == 710 & ref_period_id == 20130201)) %>% 
  group_by(date) %>% 
  summarise(net_wgt = sum(net_wgt, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = date, y = net_wgt)) +
  geom_line() +
  geom_point(shape = 19) +
  geom_rect(
    data = winter,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "lightblue2",
    alpha = 0.4
  )+
  scale_x_date(
    limits = c(ymd("2010-01-01"), NA),
    breaks = seq(
      from = ymd("2010-01-01"),
      to = ymd("2025-01-01"),
      by = "6 months"
    ),
    date_labels = "%b %Y"
  )+
  theme_classic() +
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date", y = "Net Weight of Wool (Unknown Unit)", 
       caption = "Great Britain wool exports as measure by net weight")



