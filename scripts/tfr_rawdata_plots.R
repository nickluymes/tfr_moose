
tidy_tags %>%
  group_by(Tag_WMU, Tag_FA, Tag_Res, Tag_Moose) %>%
  summarise(tags = log(mean(Tag_Issued))) %>%
  left_join(WMU_shp, by = c("Tag_WMU" = "WMU")) %>%
  sf::st_as_sf() %>%
  ggplot(aes(fill = tags)) +
  geom_sf() +
  facet_wrap(~Tag_Res + Tag_FA + Tag_Moose)


tidy_tags %>%
  left_join(wmu_cez_region) %>%
  group_by(Region, Tag_Moose, Year) %>%
  summarise(tags = sum(Tag_Allocated)) %>%
  ggplot(aes(x = Year, y = tags, colour = Tag_Moose)) +
  geom_line() +
  facet_wrap(~Region)

tidy_tags %>%
  ggplot(aes(x = Year, weight = Tag_Issued, fill = Tag_Res)) +
    geom_bar(position = "dodge") + 
    ylab("Tags Issued") +
    facet_wrap(~Tag_Moose, ncol = 1, scales =  "free_y") +
    theme_prj

tidy_tags %>%
  ggplot(aes(x = Tag_WMU, y = Tag_Issued, colour = Tag_Res)) +
  stat_summary(fun = mean, 
               fun.max = max,
               fun.min = min) +
  facet_grid(Tag_Moose + Tag_FA ~ Tag_Res) +
  scale_y_log10() +
  theme_prj

ggplot(tidy_mai,aes(x = Year, y = cpop_dens, colour = Tag_Moose)) +
  stat_summary(fun = mean) +
  geom_smooth(se = FALSE) +
  facet_wrap(~Tag_Moose, scales = "free") +
  theme_prj


ggplot(filter(tidy_season, Tag_Moose == "Bull"), 
       aes(x = Year, y = Season_Length, colour = Tag_WMU)) +
  geom_line() +
  facet_grid(Tag_FA ~ Tag_Res) +
  theme_prj
ggplot(filter(tidy_season, Tag_Moose == "Bull"), 
       aes(x = Year, y = Season_Start, colour = Tag_WMU)) +
  geom_line() +
  facet_grid(Tag_FA ~ Tag_Res) +
  theme_prj


# Kill date ----
harvdates <- readRDS("data/processed/tidy_long.rds") %>%
  filter(!is.na(KillDate)) %>%
  mutate(yday = yday(KillDate),
         Tag_FA = ifelse(is.na(Tag_FA),"G",as.character(Tag_FA))) %>%
  left_join(readRDS("data/processed/tidy_season.rds")) %>%
  mutate(diff = yday-Season_Start,
         seas_bins = cut(Season_Length,breaks = c(0,30,60,100))) %>%
  filter(diff >= 0,
         diff <= Season_Length) 

harvdates %>%
  mutate(
    group = case_when(
      seas_bins == "(0,30]" ~ "<30 day season",
      seas_bins == "(30,60]" ~ "30-60 day season",
      TRUE ~ ">60 day season"
    ),
    group = factor(group,
                   levels = c("<30 day season",
                              "30-60 day season",
                              ">60 day season"))
  ) %>%
  ggplot(aes(x = diff)) +
  geom_histogram(aes(y=after_stat(density))) +
  facet_wrap(~group) +
  xlab("Day of moose harvest") +
  ylab("Frequency")+
  theme_prj

ggsave("harvest_dates.png", height = 4, width = 5, bg = "white")

# Create graph for moose killed by tag ----

xtidy_moose <- readRDS("data/processed/tidy_short.rds") %>%
  filter(Tag_Lottery) %>%
  group_by(
    Year, Tag_WMU, Tag_Moose, Moose_Hunted
  ) %>%
  summarise(
    N = n()
  ) %>%
  drop_na(Tag_Moose, Moose_Hunted)

ggplot(xtidy_moose, aes(weight = N, x = Tag_Moose, fill = Moose_Hunted)) +
  geom_bar(position = "dodge") +
  scale_fill_viridis_d()+
  xlab("Tag type") +
  labs(fill = "Moose killed") +
  theme_prj

ggplot(xtidy_moose, aes(weight = N, x = Year, fill = Moose_Hunted)) +
  geom_bar(position = "stack") +
  facet_wrap(~Tag_Moose, nrow = 3, scale = "free_y") +
  scale_fill_viridis_d()+
  xlab("Year") +
  ylab("% moose killed by tag type") +
  labs(fill = "Moose killed") +
  theme_prj





# Create graph for percent of moose killed in correct WMU ----

xtidy_hunt <- readRDS("data/processed/tidy_short.rds") %>%
  filter(
    Tag_Lottery == TRUE,
    Hunted == TRUE,
    UsedTag == TRUE
  ) %>%
  group_by(
    Year,Tag_Moose
  ) %>%
  summarise(
    N = sum(Tag_WMU == WMUKilled, na.rm = TRUE),
    Tot = sum(!is.na(Tag_WMU == WMUKilled)),
    P = N/Tot
  ) %>%
  drop_na(Tag_Moose)

ggplot(xtidy_hunt, aes(y = P, x = Year, colour = Tag_Moose)) +
  geom_point(size = 2) +
  geom_line() +
  scale_colour_viridis_d()+
  scale_y_continuous(limits = c(0.8,1)) +
  xlab("Year") +
  ylab("Proportion of tags filled in correct WMU") +
  labs(fill = "Tag type") +
  theme_prj

### Time to event plots ----
tidy_tte <- readRDS("data/processed/tidy_tte.rds")

tidy_tte %>%
  filter(Season_Length<=90) %>%
  mutate(lengthbin = cut(Season_Length,breaks = c(0,10,20,30,40,50,60,70,80,90))) %>%
  group_by(lengthbin, Tag_Res, Tag_Moose) %>%
  summarise(days = mean(Days)) %>%
  mutate(tag = str_c(Tag_Moose, Tag_Res)) %>%
  ggplot(aes(x = lengthbin, y = days, group = tag, linetype = tag)) +
  geom_point() +
  geom_line() +
  theme_prj

tidy_tte%>%
  ggplot(aes(x = Days)) +
  geom_histogram(binwidth = 2, boundary = 0.5) + 
  facet_wrap(~Tag_Res) +
  theme_prj

ggplot(count(tidy_tte, Tag_WMU), aes(x = fct_reorder(Tag_WMU, n), y = n)) +
  geom_bar(stat = "identity") +
  theme_prj

ggplot(tidy_tte, aes(x = Year, y = Days)) +
  stat_summary(fun = mean) +
  theme_prj

(tidy_tte %>%
    group_by(Season_Start, Tag_Res) %>%
    summarize(y = sum(Tag_Success, na.rm = TRUE),
              n = n(),
              p = y / n) %>%
    ggplot(aes(x = Season_Start, y = log(-log(1 - p)), 
               colour = Tag_Res)) +
    stat_summary(fun = mean) +
    facet_wrap(~Tag_Res) +
    geom_smooth() +
    xlab("Season Length Difference from Mean") +
    theme_prj)

ggplot(filter(tidy_tte, !is.na(Tag_Success)),
       aes(x = Days, fill = Tag_Moose)) +
  geom_bar() +
  scale_fill_viridis_d() +
  ylab("Number of hunters") +
  labs(fill = "Tag type") + 
  theme_prj

ggplot(filter(tidy_tte, Tag_Success==TRUE),
       aes(x = Days, fill = Tag_Moose)) +
  geom_bar() +
  scale_fill_viridis_d() +
  ylab("Number of successful hunters") +
  labs(fill = "Tag type") + 
  theme_prj

tidy_tte %>%
    filter(!is.na(Tag_Success), Tag_Lottery) %>%
    rename(x = Tag_Moose) %>%
    group_by(Year, x) %>%
    summarise(
      event = sum(Tag_Success),
      total = n()
    ) %>%
    mutate(
      success = event/total
    ) %>%
    ggplot(aes(x = Year, 
               y = success,
               colour = x)) +
    geom_smooth(se = FALSE, method = "loess", aes(weight = total)) +
    stat_summary(fun = mean) +
    theme_bw()

sf_tte <- tidy_tte %>%
  group_by(Tag_WMU) %>%
  summarise(
    y = sum(Tag_Success, na.rm = TRUE),
    n = n(),
    Hunter_Success = y / n,
    Start_of_season = mean(mean_start),
    Length_of_season = mean(mean_length)
  ) %>%
  right_join(readRDS("data/processed/WMU_shp.rds"), by = c("Tag_WMU" = "WMU")) %>%
  sf::st_as_sf()

ggplot(sf_tte) +
  geom_sf(aes(fill = Length_of_season)) +
  scale_fill_viridis_c(option = "plasma",
                       na.value = "white") +
  theme_prj
