library(tidytuesdayR)
library(tidyverse)
library(skimr)

tt_load(2026,week=37) %>% pluck(1) %>% mutate(period=str_remove(period,"[(]"),
                                              period=str_remove(period,"[)]"),
                                              period=str_remove(period,"period"),
                                              period=str_squish(period)
                                              
                                              )->dead_sea_df
skim_without_charts(dead_sea_df)


group_by_multiple <- function(data, ...) {
  group_vars <- c(...)
  data %>%
    drop_na(all_of(group_vars)) %>% 
    group_by(across(all_of(group_vars))) %>%
    summarise(count = n(), .groups = "drop") %>% 
    return()
}

plot_grouped_bar <- function(data, ...) {
  group_vars <- c(...)
  x_var <- group_vars[1]
  summarised <- group_by_multiple(data, ...) 
  
  
  color_var <- if (length(group_vars) >= 2) group_vars[2] else NULL
  
  p <- ggplot(summarised, aes(x = reorder(.data[[x_var]],count), y = count))
  
  if (!is.null(color_var)) {
    p <- p + geom_col(aes(fill = .data[[color_var]]), position = "stack")
  } else {
    p <- p + geom_col()
  }
  
  if (length(group_vars) > 2) {
    facet_vars <- group_vars[3:length(group_vars)]
    p <- p + facet_wrap(as.formula(paste("~", paste(facet_vars, collapse = " + "))))
  }
  
  p<-p + labs(x = str_to_title(str_replace(x_var,"_"," ")), y = "Count", fill = str_to_title(color_var)) +
    theme_minimal()+
    coord_flip()+
    theme(axis.text.y = element_text(angle=45))
  
  p
}
#Basic Plot
plot_grouped_bar(dead_sea_df,"biblical_book")

#Show color Aspect
plot_grouped_bar(dead_sea_df,"script_type","site")

#Show Faceting Aspect
plot_grouped_bar(dead_sea_df,"site_parent","language","period") 
