#Libraries and Data ----
library(tidytuesdayR)
library(tidyverse)
library(skimr)

urban_green<-tt_load(2026, week=38)[[1]] %>%
  mutate(averageShareOfGreenAreaInCityUrbanAreaPct = ifelse(
    year == 2025 & countryOrTerritoryName == "Global Average",
    mean(averageShareOfGreenAreaInCityUrbanAreaPct[year == 2025 & countryOrTerritoryName != "Global Average"], na.rm = TRUE),
    averageShareOfGreenAreaInCityUrbanAreaPct
  )) %>% 
  mutate(year_ch=as.character(year)) 

skim_without_charts(urban_green)

#Box plot ----
urban_green %>% 
  drop_na() %>% #Note this drops the built in regional averages which is desired behavior
  ggplot(aes(x=year_ch,y=averageShareOfGreenAreaInCityUrbanAreaPct))+
  geom_boxplot()+
  theme_minimal()+
  labs(x="Region",y="Green Area Percentage")+
  facet_wrap(~sdgRegion)->box_plot

box_plot+patchwork::inset_element(
  urban_green %>% 
    filter(countryOrTerritoryName=="Global Average") %>% 
    drop_na(year_ch) %>% 
    ggplot(aes(x=year_ch,y=averageShareOfGreenAreaInCityUrbanAreaPct))+
    geom_point()+
    theme_minimal()+
    labs(x="",y="",subtitle = "Global Average")+
    scale_y_continuous(limits=c(0,80))+
    theme(plot.subtitle = element_text(size=8,hjust=0.5)),
  left = (2/3),
  bottom=0,
  right=1,
  top=1/3,
  align_to = "plot"
)

#Basic Model ----
model<-lm(averageShareOfGreenAreaInCityUrbanAreaPct~year,data=urban_green)
summary(model)

urban_green %>% 
  ggplot(aes(y=averageShareOfGreenAreaInCityUrbanAreaPct,x=year))+
  geom_point(position="jitter")+
  geom_smooth(method = "lm")+
  theme_minimal()+
  scale_x_continuous(limits=c(1985,2030),breaks = c(1990,2000,2010,2020,2025))


#Top Cities and Changes ----
top_cities_by_year<-urban_green %>% 
  group_by(year) %>% 
  slice_max(averageShareOfGreenAreaInCityUrbanAreaPct, n=1) %>% 
  ungroup()

#Bottom Cities with at least some green space
bottom_cities_by_year<-urban_green %>% 
  filter(averageShareOfGreenAreaInCityUrbanAreaPct>0) %>% 
  group_by(year) %>% 
  slice_min(averageShareOfGreenAreaInCityUrbanAreaPct, n=1) %>% 
  ungroup()
