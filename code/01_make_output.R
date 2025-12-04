here::i_am(
  "code/01_make_output.R"
)

#options(repos = c(CRAN = "https://cloud.r-project.org"))
library(readr)
library(tidyverse)
library(lubridate)
library(gt)
library(gtExtras)
library(ggplot2)
library(webshot2)


daily_data <- read.csv("./Data/ad_viz_plotval_data.csv")  
saveRDS(
  daily_data,
  file = here::here("output", "daily_data.rds"))


daily_data_ga <- daily_data %>%
  mutate(Month= as.numeric(substr(Date, 1, 2)))
saveRDS(
  daily_data_ga,
  file = here::here("output", "daily_data_ga.rds"))


Forest_Park <- daily_data_ga %>% 
  filter (Local.Site.Name == "Forest Park") %>% 
  group_by(Month) %>% 
  summarize(Month_Average = mean(Daily.Mean.PM2.5.Concentration))
saveRDS(
  Forest_Park,
  file = here::here("output", "Forest_Park.rds"))


Kennesaw <- daily_data_ga %>% 
  filter (Local.Site.Name == "Kennesaw") %>% 
  group_by(Month) %>% 
  summarize(Month_Average = mean(Daily.Mean.PM2.5.Concentration))
saveRDS(
  Kennesaw,
  file = here::here("output", "Kennesaw.rds"))


South_DeKalb <- daily_data_ga %>% 
  filter (Local.Site.Name == "South DeKalb") %>% 
  group_by(Month) %>% 
  summarize(Month_Average = mean(Daily.Mean.PM2.5.Concentration))
saveRDS(
  South_DeKalb,
  file = here::here("output", "South_DeKalb.rds"))


NR_285 <- daily_data_ga %>% 
  filter (Local.Site.Name == "NR-285")%>% 
  group_by(Month) %>% 
  summarize(Month_Average = mean(Daily.Mean.PM2.5.Concentration))
saveRDS(
  NR_285,
  file = here::here("output", "NR_285.rds"))


Fire_Station_8 <- daily_data_ga %>% 
  filter (Local.Site.Name == "Fire Station #8")%>% 
  group_by(Month) %>% 
  summarize(Month_Average = mean(Daily.Mean.PM2.5.Concentration))
saveRDS(
  Fire_Station_8,
  file = here::here("output", "Fire_Station_8.rds"))


United_Avenue <- daily_data_ga %>% 
  filter (Local.Site.Name == "United Avenue")%>% 
  group_by(Month) %>% 
  summarize(Month_Average = mean(Daily.Mean.PM2.5.Concentration))
saveRDS(
  United_Avenue,
  file = here::here("output", "United_Avenue.rds"))


NR_GA_Tech <- daily_data_ga %>% 
  filter (Local.Site.Name == "NR-GA Tech")%>% 
  group_by(Month) %>% 
  summarize(Month_Average = mean(Daily.Mean.PM2.5.Concentration))
saveRDS(
  NR_GA_Tech,
  file = here::here("output", "NR_GA_Tech.rds"))


Gwinnett <- daily_data_ga %>% 
  filter (Local.Site.Name == "Gwinnett")%>% 
  group_by(Month) %>% 
  summarize(Month_Average = mean(Daily.Mean.PM2.5.Concentration))
saveRDS(
  Gwinnett,
  file = here::here("output", "Gwinnett.rds"))


McDonough <- daily_data_ga %>% 
  filter (Local.Site.Name == "McDonough")%>% 
  group_by(Month) %>% 
  summarize(Month_Average = mean(Daily.Mean.PM2.5.Concentration))
saveRDS(
  McDonough,
  file = here::here("output", "McDonough.rds"))




all_sites <- bind_rows(
  Forest_Park    %>% mutate(Site = "Forest Park"),
  Kennesaw     %>% mutate(Site = "Kennesaw"),
  South_DeKalb    %>% mutate(Site = "South Dekalb"),
  NR_285  %>% mutate(Site = "NR-285"),
  Fire_Station_8    %>% mutate(Site = "Fire State #8"),
  United_Avenue    %>% mutate(Site = "United Avenue"),
  NR_GA_Tech %>% mutate(Site = "NR-GA Tech"),
  Gwinnett     %>% mutate(Site = "Gwinnett"),
  McDonough    %>% mutate(Site = "McDonough")
) %>%
  mutate(Month = as.integer(Month))
saveRDS(
  all_sites,
  file = here::here("output", "all_sites.rds"))


all_sites_12_months <- all_sites %>%
  group_by(Site) %>%
  complete(Month = 1:12) %>%
  ungroup()
saveRDS(
  all_sites_12_months,
  file = here::here("output", "all_sites_12_months.rds"))


Average_PM2.5 <- all_sites_12_months %>%
  pivot_wider(names_from = Site, values_from = Month_Average) %>%
  arrange(Month)
saveRDS(
  Average_PM2.5,
  file = here::here("output", "Average_PM2.5.rds"))


Fulton_data <- daily_data_ga %>%
  filter(Local.Site.Name %in% c("NR-GA Tech", "United Avenue")) 
saveRDS(
  Fulton_data,
  file = here::here("output", "Fulton_data.rds"))

Average_PM2.5 <- readRDS(
  here::here("output/Average_PM2.5.rds")
)

Average_table <- Average_PM2.5 %>% 
  gt() %>% 
  fmt_number(
    columns = -Month,  
    decimals = 2) %>% 
  gt_theme_pff()

gt::gtsave(
  data = Average_table,
  filename = here::here("output", "Average_table.png")
)

#ggsave(here::here("output/Average_table.png"), plot = Average_table)

Fulton_data <- readRDS(
  here::here("output/Fulton_data.rds"))


fulton_boxplot <- ggplot(Fulton_data, aes(x = Local.Site.Name, y = Daily.Mean.PM2.5.Concentration, fill = Local.Site.Name)) +
  geom_boxplot() + 
  facet_wrap(~Month) +
  labs(
    title = "Comparing Daily PM2.5 Range each Month Between Two Fulton County Air Monitoring Locations",
    x = "Month", 
    y = "Daily PM2.5 (µg/m³)",
    fill = "Local.Site.Name"
  ) +
  theme(
    axis.text.x = element_blank())

ggsave(here::here("output/fulton_boxplot.png"), plot = fulton_boxplot)
