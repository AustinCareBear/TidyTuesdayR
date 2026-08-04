#Libraries
library(tidyverse)
library(tidytuesdayR)
library(skimr)

#This data is queried from the UN Comtrade database with the condition that these are exports from Lesotho
# Data load
basotho_wool<-tt_load(2026,week =31) %>% pluck(1)

#Data Explortation
skim_without_charts(basotho_wool)

#Looking at only South Africa ----
basotho_wool %>% 
  filter(reporter_code==710) %>% 
  group_by(ref_year) %>% 
  ggplot(aes(x= ref_year, y=qty/1000))+
  geom_boxplot(aes(group=ref_year))+
  geom_point(aes(group=ref_year),alpha = 0.3)+
  theme_classic()+
  labs(x="Year", y = "Quantity of Wool (Tonnes)",caption = 
      "Distribution of monthly Basotho wool exports from Lesotho to South Africa by year. 
       Each box summarizes the spread of monthly export quantities within a given year, 
       illustrating changes in typical shipment size and variability over time")

basotho_wool %>% 
  filter(reporter_code==710) %>% 
  group_by(ref_year) %>%
  summarize(qty=sum(qty)) %>% 
  ggplot(aes(x=ref_year,y=qty/1000))+
  geom_point()+
  geom_line()+
  theme_classic()+
  labs(x="Year", y= "Basotho Wool Exports (Tonnes)", caption = 
         "Sum of Basotho Wool exports from Lesotho to South Africa by year.")
#Dataframe used to create a winter box to show how winter impacts production
winter <- tibble(
  xmin = ymd(paste0(2010:2024, "-06-01")),
  xmax = ymd(paste0(2010:2024, "-08-31")),
  ymin = -Inf,
  ymax = Inf
)

basotho_wool %>% 
  filter(reporter_code==710) %>% 
  mutate(date = ymd(ref_period_id)) %>% 
  ggplot(aes(x=date, y=qty/1000))+
  geom_point()+
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
    )
  )+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1) )+
  labs(x="Date", y="Basotho Wool Exports (Tonnes)", caption = 
         "Basotho Wool exports from Lesotho to South Africa by month.
          Blue rectangles indicate the southern hemisphere's winter months")

basotho_wool %>% 
  filter(reporter_code==710) %>% 
  mutate(date = ymd(ref_period_id)) %>% 
  ggplot(aes(x=date, y=primary_value))+
  geom_point()+
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
    )
  )+
  scale_y_continuous(labels = scales::dollar)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1) )+
  labs(x="Date", y="Value of Besotho Exports (USD)", caption = 
         "Value of Basotho Wool Exports from Lesotho to South Africa in USD.")

