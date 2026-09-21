#Libraries ----
library(tidyverse)
library(tidytuesdayR)
library(skimr)
library(patchwork)

#Data load and mutate ----
#added hours to earn and reformatted the setting 
coffee_data<-tt_load(2026,week=36) 
cafe<-coffee_data %>% pluck(1) %>% 
  mutate(hours_to_earn = (price_gbp/hourly_wage_gbp),
         minutes_to_earn = hours_to_earn*60,
         setting=case_when(
           urban ~ "Urban",
           suburban ~ "Suburban",
           TRUE ~ "Rural"
         ),
         city = str_to_title(str_to_lower(str_split_i(city,",",1)))
         ) %>% 
  select(-c(urban,suburban,rural)) %>% 
  drop_na()

cappuccino_index<-coffee_data%>% pluck(2)
rm(coffee_data)

#Exploratory ----
skim_without_charts(cafe)
skim_without_charts(cappuccino_index)

#Show NA values
View(cafe %>% filter(is.na(city)))

#Basic Viz ----
#Longest time to earn a coffee
cappuccino_index %>% 
  slice_max(order_by = index, n=15) %>% 
  ggplot(aes(x=reorder(country,index),y=index))+
  geom_col()+
  coord_flip()+
  theme_classic()+
  labs(x="Country",y="Time (min)",title="Time for a Barista to Earn a Small Cappuccino")

#Price over time plots
cafe %>% 
  ggplot(aes(x=hourly_wage_gbp,y=price_gbp))+
  geom_point(alpha=0.6)+
  theme_classic()+
  labs(x="Hourly Wage (GBP)", y="Cappuncion Price (GBP)")

#Grouped by setting
cafe %>% 
  ggplot(aes(x=hourly_wage_gbp,y=price_gbp))+
  geom_density_2d_filled()+
  theme_classic()+
  labs(x="Hourly Wage (GBP)", y="Cappuncion Price (GBP)")+
  facet_wrap(~setting)

cafe %>% 
  ggplot(aes(x=hourly_wage_gbp,y=price_gbp))+
  geom_hex()+
  theme_classic()+
  labs(x="Hourly Wage (GBP)", y="Cappuncion Price (GBP)")+
  facet_wrap(~setting)

plots <- cafe %>%
  group_split(setting) %>%
  map(~ ggplot(.x, aes(x = hourly_wage_gbp, y = price_gbp)) +
        geom_hex() +
        theme_classic() +
        labs(
          x = "Hourly Wage (GBP)",
          y = "Cappuccino Price (GBP)",
          title = unique(.x$setting),
          fill="Count"
        )
  )

wrap_plots(plots, ncol = length(plots))


#Log Scale box plot because some of the values are very extreme
#Outliers should be examined
cafe %>% 
  ggplot(aes(x = setting, y = hours_to_earn)) +
  geom_boxplot() +
  theme_classic() +
  scale_y_continuous(
    trans = "log",
    breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2, 5, 10, 20),  
    limits = c(exp(-3), exp(3))
  ) +
  labs(x = "Setting", y = "Hours to Earn a Small Cappuccino")

#Show all outliers by Setting
cafe_outliers <- cafe %>%
  group_by(setting) %>%
  mutate(
    q1 = quantile(hours_to_earn, 0.25, na.rm = TRUE),
    q3 = quantile(hours_to_earn, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower = q1 - 1.5 * iqr,
    upper = q3 + 1.5 * iqr,
    is_outlier = hours_to_earn < lower | hours_to_earn > upper
  ) %>%
  ungroup() %>% 
  filter(is_outlier) %>% 
  mutate(lower_or_upper_outlier = ifelse(hours_to_earn < lower, "lower","upper"))

skim_without_charts(cafe_outliers)

#Country, City level
cafe_city<-cafe %>% 
  group_by(country,city) %>% 
  summarize(count=n(),
            average_hours_to_earn = mean(hours_to_earn, na.rm=TRUE),
            sd_hours_to_earn = sd(hours_to_earn, na.rm = TRUE)
            ) %>% 
  ungroup()

#City level index
cafe_city %>% 
  filter(count>3) %>% 
  slice_max(order_by=average_hours_to_earn,n=20,with_ties = FALSE) %>% 
  ggplot(aes(x=reorder(city,average_hours_to_earn),y=average_hours_to_earn))+
  geom_col()+
  theme_classic()+
  coord_flip()+
  labs(x="City",y="Average Hours to Earn a Small Cappucino", 
       caption = "Cities with 2 or less observations excluded")
