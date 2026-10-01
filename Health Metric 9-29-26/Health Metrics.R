library(tidytuesdayR)
library(tidyverse)


#Data Clean and Splitting ----
#Load and rename data for better understanding
health_df<-tt_load(2026, week=39)[[1]] %>% 
  rename(
    # Core Identification
    id = ID_UC_G0,
    city_name = GC_UCN_MAI_2025,
    country_name = GC_CNT_GAD_2025,
    
    # Geography & Demographics
    area_km2_2025 = GC_UCA_KM2_2025,
    population_2025 = GC_POP_TOT_2025,
    wb_income_group = GC_DEV_WIG_2025,
    un_sdg_region = GC_DEV_USR_2025,
    
    # Raw Facility Counts (2024)
    hospitals_count_2024 = HL_FCL_HOS_2024,
    pharmacies_count_2024 = HL_FCL_PHA_2024,
    
    # Facility Density (per km2)
    hospitals_per_km2_2024 = HL_FDE_HOS_2024,
    pharmacies_per_km2_2024 = HL_FDE_PHA_2024,
    
    # Facility Per Capita (2025)
    hospitals_per_capita_2025 = HL_FPC_HOS_2025,
    pharmacies_per_capita_2025 = HL_FPC_PHA_2025,
    
    # Population Access (1km Buffer)
    pop_near_hospital_2025 = HL_POP_HOS_2025,
    pop_near_pharmacy_2025 = HL_POP_PHA_2025,
    
    # Share of Population Access (1km Buffer)
    pct_near_hospital_2025 = HL_SHP_HOS_2025,
    pct_near_pharmacy_2025 = HL_SHP_PHA_2025
  )
#Exploratory
skimr::skim_without_charts(health_df)

#Split into Pharama and Hospital
pharma_df<-health_df %>% select(id,city_name,country_name,area_km2_2025,population_2025,
                                wb_income_group,un_sdg_region,contains("pharma")) %>% 
  drop_na(pharmacies_count_2024)

hospital_df<-health_df %>% select(id,city_name,country_name,area_km2_2025,population_2025,
                                wb_income_group,un_sdg_region,contains("hospital")) %>% 
  drop_na(hospitals_count_2024)

#Full data for both
both_df <- inner_join(
  pharma_df,
  hospital_df %>%
    select(-any_of(setdiff(intersect(names(pharma_df), names(hospital_df)), "id"))),
  by = "id"
)

skimr::skim_without_charts(pharma_df)
skimr::skim_without_charts(hospital_df)
skimr::skim_without_charts(both_df)

pharma_df %>% 
  drop_na(wb_income_group) %>% 
  mutate(wb_income_group = factor(wb_income_group, 
                                  levels = c("High income", "Upper Middle", "Lower Middle", "Low income"))) %>%
  group_by(wb_income_group) %>% 
  slice_max(pharmacies_per_km2_2024,n=10) %>% 
  ungroup() %>% 
  ggplot(aes(x=pharmacies_per_km2_2024,y=reorder(city_name,pharmacies_per_km2_2024)))+
  geom_col()+
  facet_wrap(~wb_income_group, scale="free")+
  scale_x_continuous(labels=scales::comma_format())+
  theme_minimal()+
  labs(x="Pharmacies per Square Kilometer", y="Top Ten Cities")


pharma_df %>% 
  drop_na(wb_income_group) %>% 
  mutate(wb_income_group = factor(wb_income_group, 
                                  levels = c("High income", "Upper Middle", "Lower Middle", "Low income"))) %>%
  group_by(wb_income_group) %>% 
  slice_max(pct_near_pharmacy_2025,n=10) %>% 
  ungroup() %>% 
  ggplot(aes(x=pct_near_pharmacy_2025/100,y=reorder(city_name,pct_near_pharmacy_2025)))+
  geom_col()+
  facet_wrap(~wb_income_group, scale="free")+
  scale_x_continuous(labels=scales::percent_format())+
  theme_minimal()+
  labs(x="Population within 1km of Pharmacy (%)",y="Top Ten Cities")


#Examine pharma to hospital ratio ----
both_df<-both_df %>% 
  mutate(pharmacy_to_hospital_ratio=pharmacies_count_2024/hospitals_count_2024)

both_df %>% 
  ggplot(aes(x=pharmacies_count_2024,y=hospitals_count_2024))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_minimal()+
  labs(x="Pharmacies",y="Hospitals")

ratio_model<-lm(hospitals_count_2024~pharmacies_count_2024,data=both_df)
summary(ratio_model)
plot(ratio_model)
#Normality Issue 

#Log fixes normality
log_ratio_model<-lm(log10(hospitals_count_2024)~log10(pharmacies_count_2024),data=both_df)
summary(log_ratio_model)
plot(log_ratio_model)

both_df %>% 
  ggplot(aes(x=pharmacies_count_2024,y=hospitals_count_2024))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_minimal()+
  scale_x_log10()+
  scale_y_log10()+
  labs(x="Pharmacies",y="Hospitals")

#Look at a sub region level
both_df %>% 
  ggplot(aes(x=pharmacies_count_2024,y=hospitals_count_2024))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_minimal()+
  scale_x_log10()+
  scale_y_log10()+
  labs(x="Pharmacies",y="Hospitals")+
  facet_wrap(~un_sdg_region)


log_ratio_filtered_model<-function(x){
  data_filtered<-filter(both_df,un_sdg_region==x)
  if (nrow(data_filtered) < 4) {
    message(x, ": only ", nrow(data_filtered), " observations — skipping")
    return(NULL)
  }
  model<-lm(log10(hospitals_count_2024)~log10(pharmacies_count_2024),data=data_filtered)
  print(summary(model))
  par(mfrow=c(2,2))
  plot(model,main=paste(x))
  return(model)
}

region_list<-tibble(
  region=unique(both_df$un_sdg_region)
)
models <- setNames(
  lapply(region_list$region, log_ratio_filtered_model),
  region_list$region
)
# Everywhere has a relationship between hospitals and pharmacies
