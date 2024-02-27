######################################################
############ Moose Tag Fill Rates Project ############ 
######################################################
### Understanding what factors impact hunter success rates and developing
### a model to predict success rates for upcoming hunting seasons

######################################################
### Script to clean up data for use in Bayesian hierarchical survival models

### NOTE: Raw moose hunter reporting data contains sensitive information and is not
### included with data files on Dryad. 

library(tidyverse)
source("set_up.R")

# midpoints <- function(x, dp=2){
#   lower <- as.integer(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
#   upper <- as.integer(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
#   return(round(lower+(upper-lower)/2, dp))
# }

# Import shapefiles ----

WMU_shp <- sf::st_read("data/raw/WMU_shapefile/WILDLIFE_MGMT_UNIT.shp") %>%
  rename(WMU = NAME) %>%
  mutate(
    ### merge 53A and B  and 63A and B b/s they are considered the same in the 
    ### moose data sets
    WMU = parse_factor(
      str_replace_all(
        WMU,
        c("^53.$" = "53",
          "^63.$" = "63")),
      levels = WMU_level,
      include_na = FALSE
    )
  ) %>%
  ### combine geometry of merged shapes
  group_by(WMU) %>%
  summarise(geometry = sf::st_union(geometry))
saveRDS(WMU_shp, "data/processed/wmu_shp.rds")

WMU_shp_simp <- WMU_shp %>%
  sf::st_simplify(dTolerance = 1000) %>%
  sf::st_transform(3161) 
saveRDS(WMU_shp, "data/processed/wmu_shp_simp.rds")

wlz_shp = sf::st_read("data/raw/WLZ_shapefile/WLZ_version_D_14June2016.shp") %>%
  rename(WLZ = ZoneID) %>%
  sf::st_transform(32617) %>%
  mutate(WLZ = parse_factor(WLZ))

temp <- wlz_shp %>%
  sf::st_transform(32617) %>%
  sf::st_buffer(10) %>%
  sf::st_union(by_feature = TRUE) %>%
  sf::st_buffer(-1000) %>%
  sf::st_buffer(1000) %>%
  sf::st_simplify()

WLZ <- sf::st_join(sf::st_transform(WMU_shp, 32617),
                           temp,
                   largest = TRUE) %>%
  sf::st_drop_geometry()
saveRDS(WLZ, "data/processed/wlz.rds")

### import WMU-CEZ-Region Look Up Table
wmu_cez_region <- read_csv("data/raw/wmu_lut.csv") %>%
  ### convert WMU names to be consistent across datasets
  mutate(
    WMU = str_remove(WMU,"^0"),
    WMU = factor(WMU, levels = WMU_level)) %>%
  rename(Tag_WMU = WMU,
         Region = REGION,
         CEZ = CervidZone)
saveRDS(wmu_cez_region, "data/processed/wmu_cez_region.rds")

# Import and tidy tag allocation data ----

### clean tag allocation data 
tidy_huntsummary <- read_csv(
  "data/raw/FinalMooseCombinedPCResultsResTIJan262022v1.csv",
  col_types = cols(.default = col_character())) %>%
  rename(
    quests = `Quest Sent`,
    Tag_WMU = WMU,
    Year = YEAR,
    Tag_Issued = "Tags Issued (N)",
    Tag_Allocated = "Tags Allocated",
    n_bull = "Estimated Bull Kill",
    n_cow = "Estimated Cow Kill",
    n_calf = "Estimated Calf Kill"
  ) %>%
  mutate(
    quests = parse_number(quests),
    Replies = parse_number(Replies),
    ### fix WMU entries to match those used in the wmu_level list
    Tag_WMU = parse_factor(
      str_replace_all(
        str_to_upper(Tag_WMU),
        c("PROV" = NA_character_,
          "99Z" = NA_character_,
          "^0" = "",
          "^[KLMNP]$" = NA_character_)
      ),
      levels = WMU_level,
      include_na = FALSE
    ),
    ### determine res/non-res tag types
    Tag_Res = parse_factor(
      str_replace_all(
        SampleCat,
        c("^[RS].+" = "Res",
          "^[T].+" = "Tour",
          "^[zNP].+" = NA_character_)
      ),
      levels = residence_level,
      include_na = FALSE
    ),
    ### determine firearm type
    Tag_FA = parse_factor(
      str_replace_all(
        SampleCat,
        c("^.[G].+" = "G",
          "^.[A].+" = "A",
          "^.[CN].+" = NA_character_,
          "^[zNP].+" = NA_character_)
      ),
      levels = firearm_level,
      include_na = FALSE
    ),
    ### determine moose age/sex class
    Tag_Moose = parse_factor(
      str_replace_all(
        SampleCat,
        c("^..B$" = "Bull",
          "^..C$" = "Cow",
          "^.CA$" = "Calf",
          ".+Calf.+" = "Calf",
          "Non" = NA_character_,
          "PH" = NA_character_,
          "^z" = NA_character_)
      ),
      levels = moose_level,
      include_na = FALSE
    ),
    ### determine if tag was acquired via the lottery draw (as opposed to most
    ### calf tags)
    Tag_Lottery = !is.na(Tag_Moose) & 
      str_detect(
        SampleCat,
        "^.CA$"
      ) == FALSE,
    Tag_Issued = as.numeric(Tag_Issued),
    Tag_Allocated = as.numeric(Tag_Allocated)
  )

### create data set for tags allocated/issued
tidy_tags <- tidy_huntsummary %>%
  ### determine total number of hunter replies
  rename(n_tag = "Valid Replies Kill") %>%
  ### filter non-lottery tags and any calf tags - not enough years of calf 
  ### lottery for full analysis
  filter(
    !is.na(Tag_WMU),
    Year >= 1995,
    Tag_Lottery == TRUE,
    Tag_Moose != "Calf"
  ) %>%
  ### calculate total harvest for each tag type
  mutate(
    y_tag = as.numeric(case_when(
      Tag_Moose == "Bull" ~ `Raw Bull Kill`,
      Tag_Moose == "Cow" ~ `Raw Cow Kill`
    )),
    Year = as.numeric(Year)
  ) %>%
  ### sum total harvest, hunter replies,
  ### tags issued/allocated, and reply rate for each WMU tag type
  group_by(Year, Tag_WMU, Tag_Res, Tag_FA, Tag_Moose) %>%
  summarise(
    Tag_Issued = sum(Tag_Issued, na.rm = TRUE),
    y_tag = sum(y_tag, na.rm = TRUE),
    n_tag = sum(as.numeric(n_tag), na.rm = TRUE),
    replyrate = sum(Replies, na.rm = TRUE)/sum(quests, na.rm = TRUE),
    replyrate = ifelse(is.na(replyrate),0,replyrate),
    Tag_Allocated = sum(Tag_Allocated, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  ### remove entries where there weren't any tags issued
  filter(Tag_Issued > 0) 
  ### calculate WMU-specific average tags issued and year-specific deviations 
  ### from averages
  # group_by(Tag_WMU, Tag_Res, Tag_FA, Tag_Moose) %>%
  # mutate(Tag_Issued_Avg = mean(Tag_Issued),
  #        Tag_Issued_change = Tag_Issued - Tag_Issued_Avg) %>%
  # ungroup() 
saveRDS(tidy_tags, "data/processed/tidy_tags.rds")

### calculate harvest numbers of moose to calculate pre-hunt population estimates
tidy_killed <- tidy_huntsummary %>%
  select(Year, Tag_WMU, n_bull, n_cow, n_calf) %>%
  type_convert() %>%
  ### sum total moose harvested by WMU, year, and moose age/sex class
  group_by(Year, Tag_WMU) %>%
  summarise(
    Bull = sum(n_bull, na.rm = TRUE),
    Cow = sum(n_cow, na.rm = TRUE),
    Calf = sum(n_calf, na.rm = TRUE)
  ) %>%
  pivot_longer(
    Bull:Calf,
    names_to = "Tag_Moose",
    values_to = "kill_count"
  ) %>%
  mutate(
    Tag_Moose = parse_factor(
      Tag_Moose,
      levels = moose_level
    )
  )
saveRDS(tidy_killed, "data/processed/tidy_killed.rds")


# Import and tidy MAI data ----

### import data on area of moose habitat for each WMU
tidy_mooserange <- read_csv(
  "data/raw/Copy of 2014HuntableMoosePop&amp_2013HunterEffort.csv"
  ) %>%
  rename(Tag_WMU = WMU...1,
         Region = "MNR Region",
         Range = "Moose Range (km2)\nUpdated for NER, and for NWR & SR based on 2014 MAI"
  ) %>%
  select(Tag_WMU, Range) %>%
  drop_na() %>%
  ### update WMU entries for consistency
  mutate(
    Tag_WMU = parse_factor(
      str_replace_all(
        str_to_upper(Tag_WMU),
        c("PROV" = NA_character_,
          "99Z" = NA_character_,
          "^0" = "",
          "60A" = "60")
      ),
      levels = WMU_level
    )
  )
saveRDS(tidy_mooserange, "data/processed/tidy_mooserange.rds")
  

### import MAI data up to and including 2018
tidy_mai_2018 <- read_csv("data/raw/moose_1975-2020.csv") %>%
  rename(Tag_WMU = WMU,
         Year = SYR,
         proj_moose = "ProjMoose",
         proj_bull = "Proj Bulls",
         proj_cow = "Proj Cows",
         proj_calf = "Proj Calves") %>%
  mutate(
    ### update WMU entries for consistency
    ### combination surveys are dropped from analysis
    Tag_WMU = parse_factor(
      str_replace_all(
        str_to_upper(Tag_WMU),
        c("PROV" = NA_character_,
          "99Z" = NA_character_,
          "^0" = "",
          "^11Q$" = "11C",
          "P" = NA_character_,
          "32C" = NA_character_,
          "52 / 48" = "48",
          "^52$" = NA_character_,
          "^76$" = NA_character_)
      ),
      levels = WMU_level,
      include_na = FALSE
    ),
    ### calculate projected moose based on bull, cow, and calf projections
    ### compare both estimates of projected moose for consistency
    proj_moose2 = proj_bull + proj_cow + proj_calf,
    proj_moose_diff = proj_moose2/proj_moose
  ) %>%
  ### remove entries with highly inconsistent projected moose estimates
  filter(!is.na(Tag_WMU), proj_moose_diff > 0.85) %>%
  pivot_longer(proj_bull:proj_calf,
               names_to = "Tag_Moose",
               values_to = "pop_size") %>%
  mutate(
    Tag_Moose = parse_factor(
      str_replace_all(
        Tag_Moose,
        c("proj_bull" = "Bull",
          "proj_cow" = "Cow",
          "proj_calf" = "Calf")
      ),
      levels = moose_level,
    ),
    ### calculate corrected pop. size estimates for each age/sex class
    pop_prop = pop_size / proj_moose2,
    cpop_size = pop_prop * proj_moose
  ) %>%
  ### since MAI population estimates will be corrected to the previous year 
  ### using the previous year's harvest data, set year equal to one less
  mutate(
    Survey_year = Year,
    Year = Year - 1
  ) %>%
  left_join(tidy_mooserange) %>%
  select(Year, Survey_year, Tag_WMU, Tag_Moose, cpop_size)
  
### import MAI data up to and including 2020
tidy_mai_2020 <- read_csv("data/raw/MAI_2020.csv") %>%
  rename(
    Tag_WMU = "WMU",
    Year = "SurveyYear",
    p_bull = "ObsPercentBulls_WLZ",
    p_cow = "ObsPercentCows_WLZ",
    p_calf = "ObsPercentCalves_WLZ",
    n_moose = "NMoose_WMU"
  ) %>%
  mutate(
    Tag_WMU = parse_factor(
      Tag_WMU,
      levels = WMU_level,
    ),
    ### calaulte age/sex class population sizes using proportions and correct 
    ### for proportions that don't add up to 1.
    p_sum = p_bull + p_cow + p_calf,
    Bull = p_bull*n_moose/p_sum,
    Cow = p_cow*n_moose/p_sum,
    Calf = p_calf*n_moose/p_sum
  ) %>%
  pivot_longer(Bull:Calf,
               names_to = "Tag_Moose",
               values_to = "cpop_size") %>%
  mutate(
    Tag_Moose = parse_factor(
      Tag_Moose,
      levels = moose_level
    ),
    Survey_year = Year,
    Year = Year - 1
  ) %>%
  select(Year, Survey_year, Tag_WMU, Tag_Moose, cpop_size)

### combine MAI data sets with moose habitat area and total harvest
tidy_mai <- bind_rows(tidy_mai_2018, tidy_mai_2020) %>%
  left_join(tidy_mooserange) %>%
  left_join(tidy_killed) %>%
  mutate(
    ### calculate corrected population size based on harvest
    cpop_size_harv = cpop_size + kill_count,
    ### calculate moose density based on available habitat area
    cpop_dens = cpop_size_harv / Range
  ) %>%
  drop_na(Tag_WMU, cpop_dens)
saveRDS(tidy_mai, "data/processed/tidy_mai.rds")

  

# Import and tidy hunting season timing data ----
### import raw hunting season charts for moose (compiled into csv)
tidy_season_raw <- read_csv("data/raw/Season_Changes_unedit.csv") %>%
  rename(Firearm = "Class of Firearm",
         Moose = "Age of Moose that may be Hunted",
         Open_Season_Residents = "Open Season  Residents",
         Open_Season_Non_Residents = "Open Season  Non-Residents"
  ) %>%
  ### combine text for entries with multiple season specifications
  group_by(Year, WMU, Firearm, Moose) %>%
  summarise(
    Open_Season_Residents = str_c(Open_Season_Residents, collapse = " "),
    Open_Season_Non_Residents = str_c(Open_Season_Non_Residents, collapse = " ")
  ) %>% 
  ungroup() %>%
  ### create extra rows for entries with disjoint seasons
  mutate(dup = ifelse(str_detect(Open_Season_Residents,"AND"),
                      2,1)
  ) %>%
  uncount(dup, .id = "dup") %>%
  ### create unique id for each WMU-season 
  group_by(WMU, Firearm, Moose, Open_Season_Residents, 
           Open_Season_Non_Residents, dup) %>%
  mutate(
    id = cur_group_id(),
    .after = Year
  ) %>%
  ungroup() %>%
  select(-dup)

### save as csv (I manually identified season start and end months and weeks
### based on this distinct list of season data)
tidy_season_raw %>%
  distinct(WMU, Firearm, Moose, 
           Open_Season_Residents, Open_Season_Non_Residents,
           .keep_all = TRUE) %>%
  write_csv("data/processed/Season_Changes_raw.csv")

### create look up table
tidy_season_lut <- tidy_season_raw %>%
  mutate(
    ### set up data duplication column to fill in years where seasons
    ### did not change 
    dup = case_when(
      Year == 2003 ~ 4,
      Year == 2009 ~ 2,
      Year == 2017 ~ 3,
      Year == 2021 ~ 2,
      TRUE ~ 1
    ),
    ### split up the WMUs
    WMU = str_split(as.character(WMU), ","),
  ) %>%
  uncount(dup, .id = "dup") %>%
  unnest(WMU) %>%
  mutate(
    ### fill in the missing years
    Year = ifelse(Year <= 2003,
                  Year - dup + 1,
                  Year + dup - 1),
    WMU = parse_factor(
      str_replace(
        WMU,
        "60A",
        "60"
      ),
      levels = WMU_level
    )
  ) %>%
  select(Year, WMU, Firearm, Moose, id)

### import manually edited season date data and match to associated WMUs based 
### on the id field
tidy_season_dates <- read_csv("data/processed/Season_Changes_edit.csv") %>%
  mutate(
    ### ensure there are no errors in the season start and end months and weeks
    Res_Start_Month = parse_factor(
      Res_Start_Month,
      levels = month_level,
      include_na = FALSE
    ),
    Res_End_Month = parse_factor(
      Res_End_Month,
      levels = month_level,
      include_na = FALSE
    ),
    Tour_Start_Month = parse_factor(
      Tour_Start_Month,
      levels = month_level,
      include_na = FALSE
    ),
    Tour_End_Month = parse_factor(
      Tour_End_Month,
      levels = month_level,
      include_na = FALSE
    ),
    Res_Start_Wday = as.integer(fct_recode(
      parse_factor(
        Res_Start_Wday,
        levels = wday_level,
        include_na = FALSE
      ),!!!wday_level_cor
    )),
    Res_End_Wday = as.integer(fct_recode(
      parse_factor(
        Res_End_Wday,
        levels = wday_level,
        include_na = FALSE
      ),!!!wday_level_cor
    )),
    Res_End_Wday_Close = as.integer(fct_recode(
      parse_factor(
        Res_End_Wday_Close,
        levels = wday_level,
        include_na = FALSE
      ),!!!wday_level_cor
    )),
    Tour_Start_Wday_Close = as.integer(fct_recode(
      parse_factor(
        Tour_Start_Wday_Close,
        levels = wday_level,
        include_na = FALSE
      ),!!!wday_level_cor
    )),
    Tour_Start_Wday = as.integer(fct_recode(
      parse_factor(
        Tour_Start_Wday,
        levels = wday_level,
        include_na = FALSE
      ),!!!wday_level_cor
    )),
    Tour_End_Wday = as.integer(fct_recode(
      parse_factor(
        Tour_End_Wday,
        levels = wday_level,
        include_na = FALSE
      ),!!!wday_level_cor
    )),
    Tour_End_Wday_Close = as.integer(fct_recode(
      parse_factor(
        Tour_End_Wday_Close,
        levels = wday_level,
        include_na = FALSE
      ),!!!wday_level_cor
    ))
  ) %>%
  select(-Year, -WMU, -Firearm, -Open_Season_Residents,
         -Open_Season_Non_Residents, -Moose) %>%
  ### join the look up table data to associate WMUs with seasons
  right_join(tidy_season_lut, by = "id") %>%
  ### Determine season start and end dates based on the various ways that seasons
  ### are determined by the government
  mutate(
    ### if there isn't an entry for the start day field,
    ### then it is the first day of the month
    Res_Start_Day = ifelse(
      is.na(Res_Start_Day),
      1,
      Res_Start_Day
    ),
    ### option 1: start date is some day of the month
    ### option 2: start date is some day of the week following a specified day
    ### of the month
    ### option 3: start date is some day of the week following a certain number
    ### of weeks after a specified day of the month
    Res_Start_Date_op1 = ymd(str_c(Year,Res_Start_Month,Res_Start_Day)),
    ### figure out day of the week for option 1
    Res_Start_Date_WDay = wday(Res_Start_Date_op1),
    ### figure out how many days are between option 1's day of the week and
    ### the day of the week for the start of the season
    Res_Start_Date_diff1 = Res_Start_Wday - Res_Start_Date_WDay,
    ### if the start day of the week is before option 1's day of the week,
    ### determine how many days until the next start day of the week.
    Res_Start_Date_diff2 = sign(Res_Start_Date_diff1)*-7 + Res_Start_Date_diff1,
    ### pick diff1 or diff2 depending on if the start day of the week occurs 
    ### during the same week as the option 1 day of the week or during the next week
    Res_Start_Date_op2 = ifelse(
      abs(Res_Start_Date_diff1) < abs(Res_Start_Date_diff2),
      Res_Start_Date_op1 + Res_Start_Date_diff1,
      Res_Start_Date_op1 + Res_Start_Date_diff2
    ),
    Res_Start_Date_op3 = Res_Start_Date_op1 + 
      pmax(Res_Start_Date_diff1, Res_Start_Date_diff2) +
      7*(Res_Start_Wnum - 1),
    Res_Start_Date_op2 = as_date(Res_Start_Date_op2),
    Res_Start_Date_op3 = as_date(Res_Start_Date_op3),
    ### Figure out which option corresponds to each WMU season
    Res_Start_Date = case_when(
      !is.na(Res_Start_Wnum) ~ Res_Start_Date_op3,
      !is.na(Res_Start_Wday) ~ Res_Start_Date_op2,
      TRUE ~ Res_Start_Date_op1
    ),
    ### Repeat for season end date
    ### option 1: end date is some day of the month
    ### option 2: end date is some day of the week following a specified day
    ### of the month
    ### option 3: end date is some day of the week following a certain number
    ### of weeks the start of the season
    Res_End_Date_op1 = ymd(str_c(Year,Res_End_Month,Res_End_Day)),
    Res_End_Date_WDay = wday(Res_End_Date_op1),
    Res_End_Date_diff1 = Res_End_Wday_Close - Res_End_Date_WDay,
    Res_End_Date_diff2 = sign(Res_End_Date_diff1)*-7 + Res_End_Date_diff1,
    Res_End_Wday_diff = Res_End_Wday_Close - Res_End_Wday,
    Res_End_Date_op2 = ifelse(
      abs(Res_End_Date_diff1) < abs(Res_End_Date_diff2),
      Res_End_Date_op1 + Res_End_Date_diff1 - Res_End_Wday_diff,
      Res_End_Date_op1 + Res_End_Date_diff2 - Res_End_Wday_diff
    ),
    Res_End_Date_op2 = as_date(Res_End_Date_op2),
    ### calculate difference between end day of week and start day of week
    Res_End_Date_diff3 = Res_End_Wday - wday(Res_Start_Date),
    Res_End_Date_diff4 = sign(Res_End_Date_diff3)*-7 + Res_End_Date_diff3,
    Res_End_Date_op3 = Res_Start_Date + 
      pmax(Res_End_Date_diff3, Res_End_Date_diff4) +
      7*(Res_End_InstanceWDay - 1),
    Res_End_Date_op3 = as_date(Res_End_Date_op3),
    Res_End_Date = case_when(
      !is.na(Res_End_InstanceWDay) ~ Res_End_Date_op3,
      !is.na(Res_End_Wday_Close) ~ Res_End_Date_op2,
      TRUE ~ Res_End_Date_op1
    ),
    ### repeat for tourism seasons
    ### option 1: start date is some day of the month
    ### option 2: start date is some day of the week following a specified day
    ### of the month
    ### option 3: start date is some day of the week following a day of the week
    ### of the week after a specified day of the month
    ### option 4: start date is some day of the week following a certain number
    ### of weeks after a specified day of the month
    ### option 4: start date is some day of the week following a certain number
    ### of weeks after day of the week following a specified day of the month
    Tour_Start_Day = ifelse(
      is.na(Tour_Start_Day),
      1,
      Tour_Start_Day
    ),
    Tour_Start_Date_op1 = ymd(str_c(Year,Tour_Start_Month,Tour_Start_Day)),
    Tour_Start_Date_WDay = wday(Tour_Start_Date_op1),
    Tour_Start_Date_diff1 = Tour_Start_Wday - Tour_Start_Date_WDay,
    Tour_Start_Date_diff2 = sign(Tour_Start_Date_diff1)*-7 + Tour_Start_Date_diff1,
    ### calculate difference between start day of the week and option 2 day of 
    ### the week
    Tour_Start_Date_diff3 = Tour_Start_Wday_Close - Tour_Start_Wday,
    Tour_Start_Date_diff4 = sign(Tour_Start_Date_diff3)*-7 + Tour_Start_Date_diff3,
    Tour_Start_Date_op2 = ifelse(
      abs(Tour_Start_Date_diff1) < abs(Tour_Start_Date_diff2),
      Tour_Start_Date_op1 + Tour_Start_Date_diff1,
      Tour_Start_Date_op1 + Tour_Start_Date_diff2
    ),
    Tour_Start_Date_op3 = Tour_Start_Date_op2 +
      pmax(Tour_Start_Date_diff3, Tour_Start_Date_diff4),
    Tour_Start_Date_op4 = Tour_Start_Date_op1 + 
      pmax(Tour_Start_Date_diff1, Tour_Start_Date_diff2) +
      7*(Tour_Start_Wnum - 1),
    Tour_Start_Date_op5 =  Tour_Start_Date_op4 +
      pmax(Tour_Start_Date_diff3, Tour_Start_Date_diff4),
    Tour_Start_Date_op2 = as_date(Tour_Start_Date_op2),
    Tour_Start_Date_op3 = as_date(Tour_Start_Date_op3),
    Tour_Start_Date_op4 = as_date(Tour_Start_Date_op4),
    Tour_Start_Date_op5 = as_date(Tour_Start_Date_op5),
    Tour_Start_Date = case_when(
      !is.na(Tour_Start_Wday_Close) &
        !is.na(Tour_Start_Wnum) ~ Tour_Start_Date_op5,
      !is.na(Tour_Start_Wday_Close) ~ Tour_Start_Date_op3,
      !is.na(Tour_Start_Wnum) ~ Tour_Start_Date_op4,
      !is.na(Tour_Start_Wday) ~ Tour_Start_Date_op2,
      TRUE ~ Tour_Start_Date_op1
    ),
    ### repeat for tourism end date
    ### option 1: end date is some day of the month
    ### option 2: end date is some day of the week following a specified day
    ### of the month
    ### option 3: end date is some day of the week following a certain number
    ### of weeks the start of the season
    Tour_End_Date_op1 = ymd(str_c(Year,Tour_End_Month,Tour_End_Day)),
    Tour_End_Date_WDay = wday(Tour_End_Date_op1),
    Tour_End_Date_diff1 = Tour_End_Wday_Close - Tour_End_Date_WDay,
    Tour_End_Date_diff2 = sign(Tour_End_Date_diff1)*-7 + Tour_End_Date_diff1,
    Tour_End_Wday_diff = Tour_End_Wday_Close - Tour_End_Wday,
    Tour_End_Date_op2 = ifelse(
      abs(Tour_End_Date_diff1) < abs(Tour_End_Date_diff2),
      Tour_End_Date_op1 + Tour_End_Date_diff1 - Tour_End_Wday_diff,
      Tour_End_Date_op1 + Tour_End_Date_diff2 - Tour_End_Wday_diff
    ),
    Tour_End_Date_op2 = as_date(Tour_End_Date_op2),
    Tour_End_Date_diff3 = Tour_End_Wday - wday(Tour_Start_Date),
    Tour_End_Date_diff4 = sign(Tour_End_Date_diff3)*-7 + Tour_End_Date_diff3,
    Tour_End_Date_op3 = Tour_Start_Date + 
      pmax(Tour_End_Date_diff3, Tour_End_Date_diff4) +
      7*(Tour_End_InstanceWDay - 1),
    Tour_End_Date_op3 = as_date(Tour_End_Date_op3),
    Tour_End_Date = case_when(
      !is.na(Tour_End_InstanceWDay) ~ Tour_End_Date_op3,
      !is.na(Tour_End_Wday_Close) ~ Tour_End_Date_op2,
      TRUE ~ Tour_End_Date_op1
    ),
  ) %>%
  ### determine moose age/sex and firearm assocaited with season
  mutate(
    Tag_FA = case_when(
      Firearm == 1 ~ "A",
      Firearm == 2 ~ "A,G",
      Firearm == 4 ~ "G",
      Firearm == 7 ~ "A,G"
    ),
    Tag_FA = str_split(Tag_FA, ","),
    Tag_Moose = case_when(
      Moose == "Adult moose" ~ "Bull,Cow",
      Moose == "Adult moose or calf moose" ~ "Bull,Cow,Calf",
      is.na(Moose) ~ "Bull,Cow,Calf"
    ),
    Tag_Moose = str_split(Tag_Moose, ",")
  ) %>%
  unnest(Tag_FA) %>%
  unnest(Tag_Moose) %>%
  select(Year, WMU, Tag_FA, Tag_Moose, Res_Start_Date,
         Res_End_Date, Tour_Start_Date, Tour_End_Date) %>%
  pivot_longer(Res_Start_Date:Tour_End_Date,
               names_to = c( "Tag_Res",".value"),
               names_pattern = "(.+)_(.+_.+)") %>%
  mutate(
    Tag_FA = parse_factor(
      Tag_FA,
      levels = firearm_level
    ),
    Tag_Moose = parse_factor(
      Tag_Moose,
      levels = moose_level
    )
  ) %>%
  ## Identify rows where start and end dates overlap, e.g. bow seasons which 
  ### include gun-bow seasons and bow-only seasons
  arrange(Year, WMU, Tag_Moose, Tag_FA, Tag_Res, Start_Date) %>%
  drop_na() %>%
  group_by(Year, WMU, Tag_Moose, Tag_FA, Tag_Res) %>%
  mutate(indx = c(0, cumsum(as.numeric(lead(Start_Date)) >
                              cummax(as.numeric(End_Date)))[-n()])) %>%
  group_by(Year, WMU, Tag_Moose, Tag_FA, Tag_Res, indx)  %>%
  ### keep only the earliest start date and latest end dates (i.e. no overlap)
  summarise(Start_Date = first(Start_Date), End_Date = last(End_Date)) %>%
  ungroup() %>%
  ### determine length of season component
  mutate(length = as.numeric(End_Date - Start_Date)) %>%
  rename(Tag_WMU = "WMU") %>%
  mutate(
    ### flag certain WMUs that do not have a separate resident and tourism season
    flag = ifelse(Tag_WMU %in% c("47", "48", "49", "50", "53",
                                 "54", "55A", "55B"),
                  2, 1)
  ) %>%
  ### duplicate flagged seasons and specify tourism for tag type of duplicated row
  uncount(flag, .id = "flag") %>%
  mutate(
    Tag_Res = ifelse(
      flag == 2,
      "Tour",
      Tag_Res
    ),
    Tag_Res = parse_factor(
      Tag_Res,
      levels = residence_level
    )
  ) %>%
  dplyr::select(-flag)
saveRDS(tidy_season_dates, "data/processed/tidy_season_dates.rds")

### determine season start day of year and season lengths for analysis
tidy_season <- tidy_season_dates %>%
  group_by(Year, Tag_WMU, Tag_Moose, Tag_FA, Tag_Res)  %>%
  summarise(
    ### determine start day of year for earliest start date entry
    Season_Start = yday(min(Start_Date)),
    Season_Length = sum(length)
  ) %>%
  ungroup()
saveRDS(tidy_season, "data/processed/tidy_season.rds")


# Import daily weather data ----

### calculate average daily temperature during each season
tidy_temp_daily <- readRDS("data/processed/tidy_season_dates.rds") %>%
  ### add rows for each day of the season
  uncount(length+1) %>%
  group_by(Year, Tag_WMU, Tag_Moose, Tag_FA, Tag_Res, indx) %>%
  ### determine date of each day of season
  mutate(n = 1:n(),
         Date = Start_Date + n - 1) %>%
  group_by(Year, Tag_WMU, Tag_Moose, Tag_FA, Tag_Res) %>%
  arrange(Date) %>%
  mutate(n2 = 1:n()) %>%
  ungroup() %>%
  ### Look at the first 14 days of season. Based on communication with Kyle Morrison,
  ### this is when most hunters complete their hunts
  filter(n2 <= 14) %>%
  ### join in the temperature data
  left_join(readRDS("data/processed/WMU_temp_eccc.rds")) %>%
  ### calculate average temperature during the first 14 days
  group_by(Year, Tag_WMU, Tag_Moose, Tag_FA, Tag_Res) %>%
  summarise(temp = mean(pred)) %>%
  ungroup()
saveRDS(tidy_temp_daily, "data/processed/tidy_temp_daily.rds")
  
### complete the same avergaing for daily precipitation
tidy_precip_daily <- readRDS("data/processed/tidy_season_dates.rds") %>%
  uncount(length+1) %>%
  group_by(Year, Tag_WMU, Tag_Moose, Tag_FA, Tag_Res, indx) %>%
  mutate(n = 1:n(),
         Date = Start_Date + n - 1) %>%
  group_by(Year, Tag_WMU, Tag_Moose, Tag_FA, Tag_Res) %>%
  arrange(Date) %>%
  mutate(n2 = 1:n()) %>%
  ungroup() %>%
  filter(n2 <= 14) %>%
  left_join(readRDS("data/processed/WMU_precip_eccc.rds")) %>%
  group_by(Year, Tag_WMU, Tag_Moose, Tag_FA, Tag_Res) %>%
  summarise(precip = mean(pred)) %>%
  ungroup()
saveRDS(tidy_precip_daily, "data/processed/tidy_precip_daily.rds")


# Import road data ----
### load road data preprocessed in ArcGIS to select non-municipal roads
### in ARcGIS using "Orn Road Net Element" layer from data/raw/roads.gdb, 
### select all roads where JURISDICTION is not "City" or "Town" and ROAD CLASS
### is not "Alleyway", "Collector", "Ramp", " Rapid Transit", or "Service".
### Then intersect with WMU shapefile to and save as csv table.
tidy_road <- read_csv("data/raw/road_data_orn.csv") %>%
  rename(WMU = NAME) %>%
  mutate(
    ### change WMU names for consistency
    WMU = parse_factor(
      str_replace_all(
        WMU,
        c("^53.$" = "53",
          "^63.$" = "63")),
      levels = WMU_level,
      include_na = FALSE
    )
  ) %>%
  ### calculate road length by WMU
  group_by(WMU) %>%
  summarise(length = sum(road_length)) %>%
  left_join(readRDS("data/processed/WMU_shp.rds") %>% sf::st_drop_geometry()) %>%
  ### divide road length by area to get road density
  mutate(Area = Area / 1000^2,
         Area = as.numeric(Area),
         road_density = length / Area,
         ### if there isn't any relevant roads, set density to zero
         road_density = ifelse(is.na(road_density), 0, road_density)) %>%
  rename(Tag_WMU = WMU)
saveRDS(tidy_road, "data/processed/tidy_road.rds")

# Get Snow Data ----
### create shapefile for snow data
wmu_snow <- WMU_shp %>%
  st_transform(3161) 

### The following code taken from Robby Marrotte's code for snow netowrk data extraction
path_to_snow = "data/raw/Robby_paper_data/"
stations <- read.csv(paste0(path_to_snow,"CRS_GIS_2020_Marrotte.csv"))
stations <- stations[!is.na(stations$Longitude) |
                       !is.na(stations$Latitude),] # No coords
sp::coordinates(stations) <- ~Longitude+Latitude
sp::proj4string(stations) <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
stations <- sf::st_as_sf(stations)
stations <- sf::st_transform(stations, sf::st_crs(wmu_snow)) 
snow <- read.csv(paste0("data/raw/Wildlife_snow_data_NDMNRF_1952_2022_March.csv"))
snow <- snow %>%filter(Parameter == "SDepth_Avg")
snow$AVGDEPTH <- suppressWarnings(as.numeric(as.character(snow$Value)))
snow$station_name <- as.character(snow$Station_Name)
snow$Date <- as.character(snow$Date)
snow$Date <- as.Date(snow$Date, format = "%m/%d/%Y")
snow$Year <- format(snow$Date,"%Y")
snow$Week <- as.numeric(strftime(snow$Date, format = "%V"))

get_snow <- function(polys,
                     week,
                     year){
  
  pred <- tibble(Tag_WMU = polys$WMU, Week = week, Year = year)

  snow_yr <- as_tibble(snow) %>%
    filter(Week == week,
           Year == year)
  
  if(dim(snow_yr)[1] == 0){
    # Return sp
    pred <- pred %>%
      mutate(pred = 0)
  }else{
    
    
    # Sum the snowdepth average
    sum_snow_yr <- snow_yr %>%
      group_by(station_name) %>%
      summarise(Snow = mean(AVGDEPTH, na.rm = TRUE)) %>%
      as.data.frame()
    
    # Make spatial
    sum_snow_yr <- merge(stations,sum_snow_yr,
                         by.y="station_name",
                         by.x="station_name", all.x=F)
    
    
    # Krige over space
    int_values <- gstat::krige(Snow~1, 
                        locations = sum_snow_yr, 
                        newdata = polys)  
    
    # Summary
    outdat <- st_drop_geometry(int_values)
    
    # Return sp
    pred <- pred %>%
      mutate(pred = outdat$var1.pred)
  }
  return(pred)
}

### run Robby's function for select years and weeks to estimate snow depth
### during each hunting season across the province
tidy_snow <- expand_grid(Year = 2000:2020,
                       Week = 35:49) %>%
  pmap_dfr(function(Week, Year) {
    get_snow(polys = wmu_snow,
             week = Week,
             year = Year)
  }
  )
saveRDS(tidy_snow, "data/processed/tidy_snow.rds")

### Calculate average snow depth by WMU
tidy_snow_weeks <- readRDS("data/processed/tidy_season_dates.rds") %>%
  uncount(length+1) %>%
  group_by(Year, Tag_WMU, Tag_Moose, Tag_FA, Tag_Res, indx) %>%
  ### create row for each day of season
  mutate(n = 1:n(),
         Date = Start_Date + n - 1,
         Week = as.numeric(strftime(Date, format = "%V"))) %>%
  group_by(Year, Tag_WMU, Tag_Moose, Tag_FA, Tag_Res) %>%
  arrange(Date) %>%
  mutate(n2 = 1:n()) %>%
  ungroup() %>%
  ### keep only first 14 days of seasons
  filter(n2 <= 14) %>%
  ### Snow depth average are by week so calculate the proportion of each winter
  ### week that makes up the first 14 days of the hunting season
  group_by(Year, Tag_WMU, Tag_Moose, Tag_FA, Tag_Res, Week) %>%
  summarise(p = n()/14) %>%
  ### estimate average snow depth
  left_join(readRDS("data/processed/tidy_snow.rds")) %>%
  group_by(Year, Tag_WMU, Tag_Moose, Tag_FA, Tag_Res) %>%
  summarise(snow = sum(p*pred)) %>%
  ungroup()
saveRDS(tidy_snow_weeks,"data/processed/tidy_snow_weeks.rds")

# Import raw hunter reporting data ----

### list and load hunter reporting data from 2000-2018 and 2019-2020 
### separately as they are formatted separately
list_late <- list.files(
  "data/raw/Moose_Tag_Data",
  pattern = "^[^f]",
  full.names = TRUE)
list_early <- list.files(
  "data/raw/Moose_Tag_Data",
  pattern = "^f",
  full.names = TRUE)
years_late <- list_late %>%
  lapply(
    read_csv,
    col_types = cols(.default = col_character())
  ) %>%
  bind_rows() %>%
  type_convert()
years_early <- list_early %>%
  lapply(
    read_csv,
    col_types = cols(.default = col_character())
  ) %>%
  bind_rows() %>%
  type_convert()


# Tidy reporting data years 2019 - 2020 ----

tidy_late <- years_late %>%
  ### remove entries where huners didn't specify if they hunted
  filter(!is.na(Hunted)) %>%
  rename(Year = SurveyYear,
         Tag_WMU = WMU) %>%
  group_by_all() %>%
  # remove duplicated entries that have an OC id
  mutate(dupe_id = row_number()) %>%
  filter(!(dupe_id > 1 & !is.na(OC))) %>%
  ungroup() %>%
  ### extract tag ID info from SampleCat column
  tidyr::extract(
    SampleCat,
    into = "Tag_Res",
    regex = "(^[RTS])",
    remove = FALSE
  ) %>%
  rowid_to_column("id") %>%
  mutate(
    OC = as.character(OC),
    Tag_Res = parse_factor(
      str_replace_all(
        Tag_Res,
        c("[RS]" = "Res", "[T]" = "Tour")
      ), levels = residence_level,
      include_na = FALSE
    ),
    ### specfy firearm of tag type
    Tag_FA = parse_factor(
      str_replace_all(
        FirearmCode,
        c("[AB]" = "A", "[E]" = "G", "[X]" = NA_character_)
      ), levels = firearm_level,
      include_na = FALSE
    ),
    ### specify age/sex class of tag type
    Tag_Moose = parse_factor(
      ifelse(
        SampleCat == "RCA",
        "Calf",
        str_replace_all(
         AnimalCode,
         c("MB" = "Bull",
            "MW" = "Cow",
            "MC" = "Calf")
        )
      ), levels = moose_level,
      include_na = FALSE
    ),
    ### specify whether tag was acquired through the lottery (vs. calf tag)
    Tag_Lottery = !is.na(Tag_Moose) & 
      str_detect(
        SampleCat,
        "^.CA$"
      ) == FALSE,
    ### specify whether hunter actually spent days hunting
    Hunted = parse_factor(
      as.character(Hunted),
      levels = logical_level,
      include_na = FALSE
    ) == "1",
    ### specify whether hunter used their tag (harvested an animal)
    UsedTag = parse_factor(
      as.character(UsedTag),
      levels = logical_level,
      include_na = FALSE
    ) == "1",
    ### specify what gender and age of animal that was harvested
    Gender = parse_factor(
      str_replace_all(
        Gender,
        c("^[Mm][Aa][Ll][Ee]" = "Male", "^[Ff][Ee][Mm][Aa][Ll][Ee]" = "Female")
      ),levels = sex_level,
      include_na = FALSE
    ),
    AgeClass = parse_factor(
      str_replace_all(
        AgeClass,
        c("[aA][Dd][Uu][Ll][Tt]" = "Adult", "[cC]alf" = "Calf")
      ), levels = age_level,
      include_na = FALSE
    ),
    Moose_Hunted = parse_factor(
      case_when(
        Gender == "Male" & AgeClass == "Adult" ~ "Bull",
        Gender == "Female" & AgeClass == "Adult" ~ "Cow",
        AgeClass == "Calf" ~ "Calf",
        TRUE ~ str_c(Gender,AgeClass)
      ), levels = moose_level,
      include_na = FALSE
    ),
    ### specify WMU associated with tag
    Tag_WMU = parse_factor(
      str_replace_all(
        str_to_upper(Tag_WMU),
        c("PROV" = NA_character_,
          "99Z" = NA_character_,
          "^0" = "")
      ),
      levels = WMU_level,
      include_na = FALSE
    ),
    ### specify WMU where moose was killed
    WMUKilled = parse_factor(
      str_replace(
        str_to_upper(WMUKilled),
        "^0",
        ""
      ),
      levels = WMU_level,
      include_na = FALSE
    ),
    ### does harvested moose match moose on tag
    Tag_Match_Moose = Tag_Moose == Moose_Hunted,
    ### specify date of harvest
    date1 = as_date(KillDate),
    date2 = as_date(str_sub(KillDate,end=-9), format = "%b %d %Y"),
    KillDate = ifelse(is.na(date1),date2,date1),
    KillDate = as_date(KillDate)
  ) %>%
  ### pivot the table longer to combine data from all three WMUs that hunters
  ### could list
  pivot_longer(
    WMU1:WSeen3,
    names_to = c(".value","num_WMU"),
    names_pattern = "(.+)([123])",
    names_repair="minimal"
  ) %>%
  rename(WMU_Hunted = WMU) %>%
  mutate(
    WMU_Hunted = parse_factor(
      str_replace(
        str_to_upper(WMU_Hunted),
        "^0",
        ""
      ),
      levels = WMU_level,
      include_na = FALSE
    ),
    ### does the WMU that was hunted in match the WMU associated with the tag
    Tag_Match_WMU = Tag_WMU == WMU_Hunted,
    ### does the WMU where the harvest took place match the WMU assocaited with
    ### the tag
    Success_Match_WMU = WMU_Hunted == WMUKilled
  ) %>%
  ### remove rows where WMU hunting data is missing
  filter(!(num_WMU > 1 & is.na(WMU_Hunted))) %>%
  ### remove duplicate rows
  distinct(id, WMU_Hunted, .keep_all = TRUE) %>%
  select(
    Year, id, OC, Tag_Res, Tag_Lottery, Tag_FA, Tag_Moose,
    Tag_WMU, Hunted, UsedTag, WMUKilled, WMU_Hunted, Days, 
    Moose_Hunted, Tag_Match_WMU, Tag_Match_Moose, 
    Success_Match_WMU, Source, ValidforKill, ValidforDays, num_WMU,
    KillDate
  ) %>%
  relocate(
    Year, id, OC, Tag_Res, Tag_Lottery, Tag_FA, Tag_Moose,
    Tag_WMU, Hunted, UsedTag, WMUKilled, WMU_Hunted, Days, 
    Moose_Hunted, Tag_Match_WMU, Tag_Match_Moose, 
    Success_Match_WMU, Source, ValidforKill, ValidforDays,
    KillDate
  )


# Tidy reporting data years 2000 - 2018 ----

### use same steps as 2019 - 2020
tidy_early <- years_early %>%
  filter(!is.na(Hunted)) %>%
  rename(
    Year = SurveyYear,
    Tag_WMU = WMUSample,
    Tag_Res = DrawType,
    Moose_Hunted = GenderAgeClass
  ) %>%
  group_by_all() %>%
  # remove duplicated entries that have an OC id
  mutate(dupe_id = row_number()) %>%
  filter(!(dupe_id > 1 & !is.na(OC))) %>%
  ungroup() %>%
  rowid_to_column("id") %>%
  mutate(
    id = id + max(tidy_late$id),
    Tag_Lottery = VttCode != "0" & VttCode != "8" ,
    Tag_Res = parse_factor(
      str_replace_all(
        as.character(Tag_Res),
        c("[14]" = "Res", "[3]" = "Tour")
      ), levels = residence_level,
      include_na = FALSE
    ),
    Tag_FA = parse_factor(
      str_replace_all(
        as.character(VttCode),
        c("^[34]$" = "A",
          "^13$" = "A",
          "^[12]$" = "G",
          "^12$" = "G",
          "^[08]$" = NA_character_)
      ), levels = firearm_level,
      include_na = FALSE
    ),
    Tag_Moose = parse_factor(
      str_replace_all(
        as.character(VttCode),
        c("^[13]$" = "Bull",
          "^[24]$" = "Cow",
          "^0$" = "Calf",
          "^12$" = "Calf",
          "^13$" = "Calf",
          "^8$" = NA_character_)
      ), levels = moose_level,
      include_na = FALSE
    ),
    Hunted = parse_factor(
      str_replace_all(
        Hunted,
        c("[Yy]" = "Y", "[Nn]" = "N")
      ),
      levels = logical_level2,
      include_na = FALSE
    ) == "Y",
    UsedTag = parse_factor(
      str_replace_all(
        UsedTag,
        c("[Yy]" = "Y", "[Nn]" = "N")
      ),
      levels = logical_level2,
      include_na = FALSE
    ) == "Y",
    Moose_Hunted = parse_factor(
      Moose_Hunted,
      levels = moose_level,
      include_na = FALSE
    ),
    Tag_WMU = parse_factor(
      str_replace_all(
        str_to_upper(Tag_WMU),
        c("PROV" = NA_character_,
          "99Z" = NA_character_,
          "48X" = "48",
          "^0" = "",
          "^18$" = NA_character_)
      ),
      levels = WMU_level,
      include_na = FALSE
    ),
    WMUKilled = parse_factor(
      str_replace_all(
        str_to_upper(WMUKilled),
        c("^6$" = "06",
          "^1$" = NA_character_,
          "^3$" = "03",
          "^8$" = "08",
          "^1C$" = "01C",
          "^16$" = "16C",
          "^0" = "",
          "^18$" = NA_character_)
      ),
      levels = WMU_level,
      include_na = FALSE
    ),
    Tag_Match_Moose = Tag_Moose == Moose_Hunted
  ) %>%
  pivot_longer(
    WMU1:WSeen3,
    names_to = c(".value","num_WMU"),
    names_pattern = "(.+)([123])",
    names_repair="minimal"
  ) %>%
  rename(WMU_Hunted = WMU) %>%
  mutate(
    WMU_Hunted = parse_factor(
      str_replace_all(
        str_to_upper(WMU_Hunted),
        c("21N" = NA_character_,
          "32\\." = "32",
          "1 C" = "01C",
          "^1$" = NA_character_,
          "08A" = "08",
          "^3$" = "03",
          "31B" = "21B",
          "^21$" = "21A",
          "^09$" = "09A",
          "17B" = "11B",
          "^15$" = "15B",
          "^12$" = "12A",
          "^0" = "",
          "^18$" = NA_character_
          )
      ),
      levels = WMU_level,
      include_na = FALSE
    ),
    Tag_Match_WMU = Tag_WMU == WMU_Hunted,
    Success_Match_WMU = WMU_Hunted == WMUKilled,
    KillDate = as_date(KillDate)
  ) %>%
  filter(!(num_WMU > 1 & is.na(WMU_Hunted))) %>%
  distinct(id, WMU_Hunted, .keep_all = TRUE) %>%
  select(
    Year, id, OC, Tag_Res, Tag_Lottery, Tag_FA, Tag_Moose,
    Tag_WMU, Hunted, UsedTag, WMUKilled, WMU_Hunted, Days, 
    Moose_Hunted, Tag_Match_WMU, Tag_Match_Moose, 
    Success_Match_WMU, Source, ValidforKill, ValidforDays, num_WMU,
    KillDate
  ) %>%
  relocate(
    Year, id, OC, Tag_Res, Tag_Lottery, Tag_FA, Tag_Moose,
    Tag_WMU, Hunted, UsedTag, WMUKilled, WMU_Hunted, Days, 
    Moose_Hunted, Tag_Match_WMU, Tag_Match_Moose, 
    Success_Match_WMU, Source, ValidforKill, ValidforDays,
    KillDate
  )


# Bind all reporting data years ----

tidy_long <- bind_rows(tidy_early, tidy_late)
saveRDS(tidy_long, "data/processed/tidy_long.rds")


# Summarize by hunter ----
### summarise whether a moose was harvested for each unter with a tag
tidy_short <- readRDS("data/processed/tidy_long.rds") %>%
  group_by(
    Year, id, OC, Tag_Res, Tag_FA, Tag_Moose, Tag_Lottery,
    Tag_WMU, Hunted, UsedTag, WMUKilled, 
    Moose_Hunted, Tag_Match_Moose
  ) %>%
  summarise(
    ### do any of the WMUs hunted in match the tag's WMU
    Tag_Match_WMU = ifelse(
      all(is.na(Tag_Match_WMU)), 
      NA, 
      any(Tag_Match_WMU,na.rm = TRUE)
    ),
    ### does the WMU where the moose was harvested match the tag's WMU
    Success_Match_WMU = ifelse(
      all(is.na(Success_Match_WMU)), 
      NA, 
      any(Success_Match_WMU,na.rm = TRUE)
    )
  ) %>%
  ungroup()
saveRDS(tidy_short, "data/processed/tidy_short.rds")


# Calculate number of hunters per WMU per year ----
tidy_hunters <- read_csv(
  "data/raw/FinalMooseCombinedPCResultsResTIJan262022v1_S2.csv",
  col_types = cols(.default = col_character())) %>%
  rename(
    Tag_WMU = WMU,
    Year = YEAR,
    hunters = `Estimated Hunters           (3 WMU)`,
    moose = `Estimated # Moose Seen     (3 WMU)`,
    days = `Estimated Days (3 WMU)`
  ) %>%
  mutate(
    Tag_WMU = parse_factor(
      str_replace_all(
        str_to_upper(Tag_WMU),
        c("PROV" = NA_character_,
          "99Z" = NA_character_,
          "^0" = "",
          "^[KLMNP]$" = NA_character_)
      ),
      levels = WMU_level,
      include_na = FALSE
    ),
    Tag_Res = ifelse(str_detect(SampleCat,"^T.*"),"Tour","Res"),
    hunters = parse_integer(hunters),
    moose = parse_integer(moose),
    days = parse_integer(days),
    Year = parse_number(Year)
  ) %>%
  ### calculate the number of hunters per WMU year
  group_by(Tag_WMU, Year) %>%
  summarise(nhunters = sum(hunters, na.rm = TRUE)
  ) %>%
  left_join(readRDS("data/processed/tidy_mooserange.rds")) %>%
  drop_na() %>%
  ### divide hunters by moose area to calculate density
  mutate(dhunters = nhunters/Range) %>%
  ungroup() %>%
  select(-Range)
saveRDS(tidy_hunters, "data/processed/tidy_hunters.rds")



