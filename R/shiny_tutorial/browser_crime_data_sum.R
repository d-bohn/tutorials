pacman::p_load(tidyverse)

df1 <- read.csv('~/Downloads/estimated_crimes.csv') %>%
  mutate(., year = as.character(year)) %>% 
  group_by(., year) %>% 
  summarise_if(., is.numeric, mean)
  #separate(., Month, into = c("year","month"), sep = "-")

df2 <- read.csv("~/Downloads/browser_stats.csv") %>% 
  separate(., Date, into = c("year","month"), sep = "-") %>% 
  group_by(., year) %>% 
  summarise_if(., is.numeric, mean)

data <- left_join(df1, df2)

ggplot(data, aes(x=IE, y=robbery)) +
  geom_point() +
  stat_smooth(method='lm')

write.csv(data, 'data/shiny_tutorial/browser_crime.csv', row.names=FALSE)

PerformanceAnalytics::chart.Correlation(data[c(2:6,41:51)])
