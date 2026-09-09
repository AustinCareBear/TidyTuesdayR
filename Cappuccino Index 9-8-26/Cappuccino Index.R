library(tidyverse)
library(tidytuesdayR)
library(skimr)

coffee_data<-tt_load(2026,week=36) 

cafe<-coffee_data %>% pluck(1) %>% 
  mutate(hours_to_earn = (price_gbp/hourly_wage_gbp))
cappuccino_index<-coffee_data%>% pluck(2)
rm(coffee_data)

skim_without_charts(cafe)
skim_without_charts(cappuccino_index)

cappuccino_index %>% 
  slice_max(order_by = index, n=15) %>% 
  ggplot(aes(x=reorder(country,index),y=index))+
  geom_col()+
  coord_flip()+
  theme_classic()+
  labs(x="Country",y="Time (min)",title="Time for a Barista to Earn a Small Cappuccino")


