
# * Day 1 -----------------------------------------------------------------


# * Load Packages ---------------------------------------------------------

# From Rob Hyndman

# install.packages(c(
#   "tidyverse",
#   "fpp3",
#   "GGally",
#   "sugrrants"
# ))
# install.packages("fabletools", repos = "https://tidyverts.org")


require(fpp3)
require(GGally)
require(sugrrants)
require(readxl)


# library(tsibble)
# library(tsibbledata)
# require(fable)
# require(feasts)
# require(tidyverse)


# * Lab Session 1 ---------------------------------------------------------

df <- read_excel(
  path = here::here("00_data/", "tourism.xlsx")
)

# Need to Learn this. 
# tmp <- tempfile()
# 
# 
# df3 <- download.file("http://robjhyndman.com/data/tourism.xlsx", tmp) %>% 
#   
# 
# read_excel(path = tmp, sheet = 1)

# tmp <- tempfile(fileext = ".xlsx")
# download.file("http://robjhyndman.com/data/tourism.xlsx", destfile = tmp)
# 
# unzip(tmp)
# read_excel(path = tmp)

## Correct Version 

download.file("http://robjhyndman.com/data/tourism.xlsx", tourism_file <- tempfile())
# my_tourism <- readxl::read_excel(tmp) %>%
  mutate(Quarter = yearquarter(Quarter)) %>%
  as_tsibble(
    index = Quarter,
    key = c(Region, State, Purpose)
  )


df <- df %>% 
  mutate(
    Quarter = yearquarter(Quarter)
    ) %>% 
  as_tsibble(
    key = c(Region:Purpose),
    index = Quarter
  )

df %>% 
  select(
    Region,
    Purpose, 
    Trips
  ) %>% 
  summarise(
    mean_trips = mean(Trips)
  ) %>% 
  arrange(desc(mean_trips))

df %>% 
  group_by(
    Region,
    Purpose
  ) %>% 
  summarise(
    mean_trips = mean(Trips)
  ) %>% 
  arrange(desc(mean_trips))


df2 <- df %>% 
  select(
    Purpose, Region, Quarter, Trips
  ) %>% 
  group_by(Region) %>% 
  summarize(
    Tot_Trips = sum(Trips)
  )  


# * Lab Session 2 ---------------------------------------------------------

aus_production %>% head()
  
aus_production %>% 
  autoplot(Bricks)

aus_production %>% 
  ggplot(
    aes(
      x = Quarter 
    )
  ) +
  geom_line(
    aes(
      y = log(Beer)
    )
  ) +
  geom_line(
    aes(
      y = log(Tobacco)
    )
  )

pelt %>% head()

pelt %>% 
  autoplot(Lynx)

pelt %>% 
  gather(
    key = "Animal",
    value = "No. of Pelts",
    Hare:Lynx
  ) %>% 
  ggplot(
    aes(
      x = Year,
      y = `No. of Pelts`,
      group = Animal
    )
  ) +
  geom_line()

gafa_stock %>% head()

gafa_stock %>% 
  autoplot(Close)

vic_elec %>% head()

vic_elec %>% 
  autoplot(Demand)

vic_elec %>% 
  gg_season(Demand)

vic_elec %>% 
  gg_season(
    Demand, 
    period = "week"
  )
vic_elec %>% 
  gg_season(
    Demand, 
    period = "day"
  )

vic_elec %>% 
  filter(year(Date) ==2014) %>% 
  mutate(hour = hour(Time))
  frame_calendar(
    x = hour,
    y = Demand,
    date = Date,
    nrow = 4
  ) %>% 
    
    

# * Lab Session 3 ---------------------------------------------------------

data("tourism")

snowy <- df %>% 
    filter(
      Region == "Snowy Mountains"
    ) 


snowy %>% 
  autoplot()


snowy %>% 
  gg_season()

snowy %>% 
  gg_subseries()


pedestrian %>% skimr::skim()
pedestrian %>% count(Sensor_Name)

pedestrian %>% 
  mutate(
    hour_time = lubridate::ydm_hms(Date_Time)
  ) 

  as_tsibble(
    index = lubridate::hour(Date_Time),
    keys = c(Sensor_Name, Hourly_Counts)
  )
  autoplot()
  
p1 <-   pedestrian %>% 
    as_tsibble(
      key = c(Sensor_Name),
      index = Date_Time
    ) %>%
  filter(Sensor_Name == "Collins Place (North)",
         Year == 2016) %>% 
  frame_calendar(
    x = Time,
    y = Hourly_Counts,
    date = Date,
    nrow = 4
  ) %>% 
  ggplot(
    aes(
      x = .Time,
      y =  .Hourly_Counts,
      group = Date
    )  
  ) +
    geom_line()  

    prettify(p1,
      size = 3, 
      label.padding = unit(0.15, "lines")
    )
  
    
    # 
    # calendar_df <- tsibble::pedestrian %>% 
    #   filter(Sensor == "Sothern Cross Station",
    #          year(Date_Time) == 2015) %>% 
    #   as_tsibble(
    #     key = c(Sensor_Name),
    #     index = Date_Time
    #   ) %>%
    #   frame_calendar(
    #     x = Time,
    #     y = Hourly_Counts,
    #     date = Date,
    #     nrow = 4
    #   ) %>% 
    #   ggplot(
    #     aes(
    #       x = .Time,
    #       y =  .Hourly_Counts,
    #       group = Date
    #     )  
    #   ) +
    #   geom_line()  
    # 
    # prettify(p1,
    #          size = 3, 
    #          label.padding = unit(0.15, "lines")
    # )
    
    
    new_prod <- aus_production %>% 
      filter(year(Quarter) >= 1992)

    new_prod %>% gg_lag(Beer)    
    new_prod %>% gg_lag(Beer, geom = "point")    

    
    new_prod %>% 
      ACF(
        Beer, 
        lag_max = 9
      ) %>% 
    autoplot()

        new_prod %>% 
      ACF(
        Beer
        ) %>% 
    autoplot()

        
        holidays %>% ACF(Trips)
        

# * Lab Session 4 ---------------------------------------------------------

aus_production %>% 
          gg_lag(Bricks)
        
aus_production %>% 
    gg_lag(Bricks, geom = "point")

aus_production %>% 
  ACF(
    Bricks, 
    lag_max = 10
  ) %>% 
  autoplot()

pelt %>% 
  gg_lag(Lynx) 

pelt %>% 
  gg_lag(Lynx, geom = "point") 

pelt %>% 
  ACF(
    Lynx
  ) %>% 
  autoplot()

gafa_stock %>% 
  filter(Symbol == "AMZN") %>% 
  mutate(trading_day = row_number()) %>% 
  update_tsibble(
    index = trading_day,
    regular = TRUE
  ) %>% 
  ACF(
    Close,
    lag_max = 40
  ) %>% 
  autoplot()

vic_elec %>% 
  gg_lag(
    Demand
  )

vic_elec %>% 
  ACF(
    Demand
  ) %>% 
  autoplot()


# * Lab Session 5 ---------------------------------------------------------

dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))

dgoog %>% 
  ACF(diff) %>% 
  autoplot



# * Lab Session 6 ---------------------------------------------------------


ge2 <- global_economy %>% 
  mutate(per_cap = GDP / Population) 

ge2 %>% 
  filter(Country == "Canada") %>% 
  autoplot(per_cap)

ge2 %>% 
  arrange(desc(per_cap))

ge2 %>% 
  filter(Country == "Monaco") %>% 
  autoplot(per_cap)

ge2 %>%  
  autoplot(per_cap) +
  theme(legend.position = "none")

ge2 %>% 
  filter(Country == "Monaco") %>% 
  autoplot(per_cap)



# * Lab Session 7 ---------------------------------------------------------

global_economy %>% 
  filter(Country == "United States") %>% 
  autoplot(GDP)

global_economy %>% 
  filter(Country == "United States") %>% 
  autoplot(GDP^2)
global_economy %>% 
  filter(Country == "United States") %>% 
  autoplot(GDP^3)
global_economy %>% 
  filter(Country == "United States") %>% 
  autoplot(log(GDP))

global_economy %>% 
  filter(Country == "United States") %>% 
  features(GDP, features = guerrero)
global_economy %>% 
  filter(Country == "United States") %>% 
  autoplot(box_cox(GDP, lambda = 0.282)) 


aus_livestock %>% 
  filter(Animal == "Bulls, bullocks and steers", 
         State == "Victoria") %>% 
  autoplot(Count)

aus_livestock %>% 
  filter(Animal == "Bulls, bullocks and steers", 
         State == "Victoria") %>% 
  autoplot(Count^2)

aus_livestock %>% 
  filter(Animal == "Bulls, bullocks and steers", 
         State == "Victoria") %>% 
  autoplot(1 / Count)

aus_livestock %>% 
  filter(Animal == "Bulls, bullocks and steers", 
         State == "Victoria") %>% 
  autoplot(Count^3)

aus_livestock %>% 
  filter(Animal == "Bulls, bullocks and steers", 
         State == "Victoria") %>% 
  autoplot(log(Count))

aus_livestock %>% 
  filter(Animal == "Bulls, bullocks and steers", 
         State == "Victoria") %>% 
  features(Count, features = guerrero)

aus_livestock %>% 
  filter(Animal == "Bulls, bullocks and steers", 
         State == "Victoria") %>% 
  autoplot(box_cox(Count, lambda = -0.07))

aus_production %>% 
  autoplot(box_cox(Gas, 0.1))
aus_production %>% 
  autoplot(Gas)

vic_elec %>% 
  autoplot(Demand)
vic_elec %>% 
  autoplot(log(Demand))

canadian_gas %>% 
  autoplot()


# * Lab Session 8 ---------------------------------------------------------

canadian_gas %>% 
  model(STL(Volume ~ season(window = 7) + trend(window = 11))) %>% 
  components() %>% 
  autoplot()


canadian_gas %>% 
  model(STL(Volume ~ season(window = 13) + trend(window = 20))) %>% 
  components() %>% 
  autoplot()

canadian_gas %>% 
  model(STL(Volume ~ season(window = 13) + trend(window = 11))) %>% 
  components() %>% 
  gg_season()

canadian_gas %>% 
  model(STL(Volume ~ season(window = 13) + trend(window = 11))) %>% 
  components() %>% 
  gg_season(season_year)

canadian_gas %>% 
  model(STL(Volume ~ season(window = 13) + trend(window = 11))) %>% 
  components() %>% 
  gg_season(season_adjust)

canadian_gas %>% 
  model(STL(Volume ~ season(window = 7) + trend(window = 11))) %>% 
  components() %>% 
select(Month, season_adjust) %>% 
  autoplot()



# * Lab Session 9 ---------------------------------------------------------

trsm_feat <- tourism %>% 
  features(Trips, feat_stl) 
trms


ggpairs(
  trsm_feat,
  columns = c(
    seasonal_peak_year,
    trend_strength
    )
  )
  ggpairs(
    trsm_feat,
    State, seasonal_strength_year
  )


  trsm_feat %>% 
    ggpairs(
      columns = c("State", "seasonal_peak_year")
    )
  
  trsm_feat %>% 
    ggpairs(
      columns = c("State", "seasonal_trough_year")
    )
  trsm_feat %>% 
    select(-Region, -State, -Purpose) %>%
    ggpairs()

  ggpairs(trsm_feat, columns = 4:ncol(.))  
  
  trsm_feat %>% 
    filter(spikiness > 2000)

  
  tourism %>% 
    group_by(State) %>% 
    summarise(Trips = sum(Trips)) %>% 
    features(Trips, feat_stl) %>% 
    select(State, seasonal_peak_year)
  
  
  tourism_features <- tourism %>% 
    features(Trips, feature_set(pkgs = "feasts"))

  
  pcs <- tourism_features %>% 
    select(-State, -Region, -Purpose) %>% 
    prcomp(scale = TRUE) %>% 
    broom::augment(tourism_features)
  
  
  pcs %>% 
    ggplot(
      aes(
        x = .fittedPC1,
        y = .fittedPC2,
        col = State
      )
    ) +
    geom_point()

    
  pcs %>% 
    ggplot(
      aes(
        x = .fittedPC1,
        y = .fittedPC2,
        col = Purpose
      )
    ) +
    geom_point()
  
  outliers <- pcs %>% 
    filter(.fittedPC1 > 12 & .fittedPC2 > 0)
outliers



# * Lab Session 10 --------------------------------------------------------

pbs_features <- PBS %>% 
  features(Cost, feature_set(pkg = "feasts"))

pbs_no_zeroes <- PBS %>% 
  group_by_key() %>% 
  filter(!all(Cost == 0)) %>% 
  ungroup() %>% 
  fill_gaps()

pbs_no_zero_features <- pbs_no_zeroes %>% 
  features(Cost, feature_set(pkg = "feasts"))



pbs_pcs <- pbs_no_zero_features %>%
  select_if(is.numeric) %>%
  prcomp(scale = TRUE) %>% 
  broom::augment(pbs_no_zeroes)
  
  filter(!is.nan(stat_arch_lm))
    