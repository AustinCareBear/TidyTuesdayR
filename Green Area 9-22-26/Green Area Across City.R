#Libraries and Data ----
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(car)

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

#change over time
change <- urban_green %>% 
  select(-year_ch) %>% 
  pivot_wider(
    names_from = year,
    values_from = c(averageShareOfGreenAreaInCityUrbanAreaPct, greenAreaPerCapitaM2)
  ) %>% 
  mutate(
    "1990-2000" = averageShareOfGreenAreaInCityUrbanAreaPct_2000 - averageShareOfGreenAreaInCityUrbanAreaPct_1990,
    "2000-2010" = averageShareOfGreenAreaInCityUrbanAreaPct_2010 - averageShareOfGreenAreaInCityUrbanAreaPct_2000,
    "2010-2020" = averageShareOfGreenAreaInCityUrbanAreaPct_2020 - averageShareOfGreenAreaInCityUrbanAreaPct_2010,
    "2020-2025" = averageShareOfGreenAreaInCityUrbanAreaPct_2025 - averageShareOfGreenAreaInCityUrbanAreaPct_2020,
    "Total Change" = ifelse(
      is.na(averageShareOfGreenAreaInCityUrbanAreaPct_2025),
      averageShareOfGreenAreaInCityUrbanAreaPct_2020 - averageShareOfGreenAreaInCityUrbanAreaPct_1990,
      averageShareOfGreenAreaInCityUrbanAreaPct_2025 - averageShareOfGreenAreaInCityUrbanAreaPct_1990
    )
  )

most_growth<-change %>% 
  select(-starts_with("averageShareOfGreenAreaInCityUrbanAreaPct_"),
         -starts_with("greenAreaPerCapitaM2_")) %>% 
  pivot_longer(
    cols = c("1990-2000", "2000-2010", "2010-2020", "2020-2025", "Total Change"),
    names_to = "years",
    values_to = "pct_point_change"
  ) %>% 
  group_by(years) %>% 
  slice_max(pct_point_change, n=1) %>% 
  ungroup()

worst_growth<-change %>% 
  select(-starts_with("averageShareOfGreenAreaInCityUrbanAreaPct_"),
         -starts_with("greenAreaPerCapitaM2_")) %>% 
  pivot_longer(
    cols = c("1990-2000", "2000-2010", "2010-2020", "2020-2025", "Total Change"),
    names_to = "years",
    values_to = "pct_point_change"
  ) %>% 
  group_by(years) %>% 
  slice_min(pct_point_change, n=1) %>% 
  ungroup()

bind_rows(worst_growth,most_growth) %>% 
  ggplot(aes(x=years,y=pct_point_change, fill=paste0(cityName, ", ",countryOrTerritoryName)))+
  geom_col(position="dodge")+
  theme_minimal()+
  labs(y="Change in Green Space (%)", x="Time Span", fill="City")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle=45,hjust=1))

#Examining US ----
us<-urban_green %>% filter(countryOrTerritoryName=="United States of America")

us %>% filter(year!=2025) %>% #Not a lot of data for 2025
  ggplot(aes(x=year_ch,y=averageShareOfGreenAreaInCityUrbanAreaPct))+
  geom_violin(aes(fill=year_ch))+
  geom_jitter(alpha=0.4, width=0.1)+
  theme_minimal()+
  labs(x="Year",y="Green Space (%)")+
  theme(legend.position = "none")+
  stat_summary(fun.data = mean_se,geom="errorbar", width=0.15,linewidth=0.6)+
  stat_summary(fun=mean, geom="point",shape=23,size=3,fill="white")

us %>% filter(year!=2025) %>% #Not a lot of data for 2025
  ggplot(aes(x=year_ch,y=greenAreaPerCapitaM2))+
  geom_violin(aes(fill=year_ch))+
  geom_jitter(alpha=0.4, width=0.1)+
  theme_minimal()+
  labs(x="Year",y="Green Area per Capita (m^2)")+
  theme(legend.position = "none")+
  stat_summary(fun.data = mean_se,geom="errorbar", width=0.15,linewidth=0.6)+
  stat_summary(fun=mean, geom="point",shape=23,size=3,fill="white")

result <- us %>% 
  select(
    cityName,
    year_ch,
    averageShareOfGreenAreaInCityUrbanAreaPct
  ) %>% 
  mutate(year_ch = as.character(year_ch)) %>% 
  filter(year_ch %in% c("1990", "2000","2010","2020")) %>% 
  pivot_wider(
    names_from = year_ch,
    values_from = averageShareOfGreenAreaInCityUrbanAreaPct
  ) %>% drop_na()


check_pair_assumptions <- function(x, y) {
  d <- x - y
  diff_df <- data.frame(diff = d)

  cat("n pairs:", length(d), "\n")
  cat("Shapiro-Wilk p-value (normality of differences):", 
      round(shapiro.test(d)$p.value, 4), "\n")
  cat("Mean diff:", round(mean(d), 3), " SD diff:", round(sd(d), 3), "\n")
  
  # Histogram
  p_hist <- ggplot(diff_df, aes(x = diff)) +
    geom_density() +
    theme_minimal() +
    labs(x = "Difference", y = "Count")
  print(p_hist)
  
  # Boxplot
  p_box <- ggplot(diff_df, aes(y = diff, x = "")) +
    geom_boxplot(fill = "skyblue") +
    theme_minimal() +
    labs(x = NULL, y = "Difference")
  print(p_box)
  
  # Q-Q plot (car package - includes confidence envelope)
  qqPlot(d, ylab = "Difference")
}
check_pair_assumptions(result$`1990`, result$`2020`)
t.test(result$`1990`, result$`2020`, paired = TRUE)

check_pair_assumptions(result$`1990`, result$`2000`)
t.test(result$`1990`, result$`2000`, paired = TRUE)

check_pair_assumptions(result$`2010`, result$`2020`)
t.test(result$`2010`, result$`2020`, paired = TRUE)
