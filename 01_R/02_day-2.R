
# * Day 2 -----------------------------------------------------------------



# * Load Packages ---------------------------------------------------------

require(fpp3)
require(GGally)
require(sugrrants)
require(readxl)

# Example 

brick_fit <- aus_production %>% 
  filter(!is.na(Bricks)) %>% 
  model(
    `snaive` = SNAIVE(Bricks),
    `naive` = NAIVE(Bricks),
    `drift` = RW(Bricks ~ drift()),
    `means` = MEAN(Bricks)
  )

brick_fc <- brick_fit %>% 
  forecast(h = "5 years")

brick_fc

brick_fc %>% autoplot() 
brick_fc %>% autoplot(aus_production) 
brick_fc %>% autoplot(aus_production, level = 90) 
brick_fc %>% autoplot(aus_production, level = NULL) 


brick_fc %>% hilo(level = c(50, 75))  
brick_fc %>% hilo(level = c(50, 75))  %>% unnest()



# * Lab Session 11 --------------------------------------------------------

hh_budget %>% autoplot(Wealth)

hh_fit <- hh_budget %>%
  model(
    snaive = SNAIVE(Wealth),
    naive = NAIVE(Wealth),
    drift = RW(Wealth ~ drift()),
    means = MEAN(Wealth)
  )

hh_pred <- hh_fit %>% 
  forecast(h = "5 years")

hh_pred

hh_pred %>% autoplot() +
  autolayer(hh_budget, Wealth)

skimr::skim(aus_retail)

hh_budget %>% 
  model(
    drift = RW(Wealth ~ drift())
  ) %>% 
  forecast(h="5 years") %>% 
  autoplot()

aus_takeaway <- aus_retail %>% 
  filter(Industry == "Food retailing") %>% 
  summarise(Turnover = sum(Turnover))

aus_takeaway %>% autoplot()


aus_takeaway %>% 
  model(
    snaive = SNAIVE(log(Turnover) ~ drift())) %>% 
    forecast(h = "5 years") %>% 
    autoplot()  
  

hh_fit %>% gg_tsresiduals()

beer <- aus_production %>% 
  model(
    beer_snaive = SNAIVE(Beer)
  )

beer_fit <- beer %>% 
  forecast(
    h = "5 years"
  )
beer_fit %>% autoplot()

beer_fit %>% gg_tsresiduals(beer_snaive)

beer_fit <- model(aus_production, SNAIVE(Beer))

beer_pred <- forecast(beer_fit, h = "2 years")

beer_fit %>% gg_tsresiduals()

augment(beer_fit) %>% features(.resid, ljung_box, dof = 0, lag = 10)
augment(beer_fit) %>% features(.resid, ljung_box, dof = 0, lag = 24)



# * Lab Session 13 --------------------------------------------------------

hh_train <- hh_budget %>% 
  filter(
    Year < 2013
  )

# Use this
# year <= max(Year) - 4

hh_mod <- hh_train %>%
  model(
    snaive = SNAIVE(Wealth),
    naive = NAIVE(Wealth),
    drift = RW(Wealth ~ drift()),
    means = MEAN(Wealth)
  )


hh_mod_fit <- hh_mod %>% 
  forecast(h = "4 years") 

hh_mod_fit %>% autoplot(hh_budget, level = NULL)

accuracy(hh_mod_fit, hh_budget) %>% 
  arrange(MASE)

hh_mod_fit %>% 
  accuracy(hh_budget) %>% 
  group_by(.model) %>% 
  summarise_if(is.numeric, mean)


aus_takeaway_train <- aus_takeaway %>% 
  filter(year(Month) <= max(year(Month) - 4 )) 


aus_takeaway_forecast <- aus_takeaway_train
  model
  
  

# * Lab Session 14 --------------------------------------------------------

ch_gdp <- global_economy %>% 
    filter(Country == "China")

fit <- ch_gdp %>% 
  model(ch_ets = ETS(GDP))

report(fit)    

fit_2 <- ch_gdp %>% 
  model(ch_log = ETS(log(GDP)))

ch_gdp %>% 
  model(ch_damp = ETS(GDP ~ trend("Ad"))) %>% 
  forecast(h = 20) %>% 
  autoplot(ch_gdp)


ch_gdp %>% features(GDP, features = guerrero)
ch_gdp %>% 
  model(ch_box_cox = ETS(box_cox(GDP, lambda = .01))) %>% 
  forecast(h = 20) %>% 
  autoplot(ch_gdp)

report(fit_2)

glance(fit_2)
tidy(fit_2)
coef(fit_2)

fit %>% 
  forecast(h = 20) %>% 
  autoplot(ch_gdp)

fit_2 %>% 
  forecast(h = 20) %>% 
  autoplot(ch_gdp)


# * Lab Session 15 --------------------------------------------------------

gas_train <- aus_production %>% 
filter(year(Quarter) <= max(year(Quarter)) - 4)


gas_mod <- gas_train %>% 
  model(
    ets = ETS(Gas)
  )

report(gas_mod)

gas_mod %>% 
  forecast(h = "4 years") %>% 
  autoplot(aus_production, level = NULL)


gas_mod_damp <- gas_train %>% 
  model(
    ets_damp = ETS(Gas ~ trend("Ad"))
  )
gas_mod_damp %>% 
  forecast(h = "4 years") %>% 
  autoplot(aus_production, level = NULL)

gas_mod %>% accuracy()
gas_mod_damp %>% accuracy()


fgit <- model(canadian_gas, ets= ETS(Volume))
fgit
report(fgit)

fgit %>% forecast(h= "6 years") %>% autoplot(canadian_gas)

pelt %>% autoplot(Lynx)


pelt %>% model(ets = ETS(Lynx)) %>% forecast(h = "30 years") %>% autoplot(pelt)



# * Lab Session 16 --------------------------------------------------------

us_gdp <- global_economy %>% 
  filter(Country == "United States")

fit <- us_gdp %>% 
  model(
    us_arima = ARIMA(GDP),
    logged_us_arima = ARIMA(log(GDP))
  )

report(fit)

fit %>% 
  forecast(
    h = "5 years"
  ) %>% 
  autoplot(us_gdp) +
  scale_x_continuous(
    limits = c(2010, NA)
  )


fit %>% select(logged_us_arima) %>% report()

# his_model <- us_gdp %>% 
#   model(
#     arima = ARIMA(log(GDP)),
#     arima0 = ARIMA(log(GDP) ~ pdq(d = 1)),
#     arima0 = ARIMA(log(GDP) ~ pdq(d = 0)
#     arima = ARIMA(log(GDP))
#     arima = ARIMA(log(GDP))
#   )

glance(fit)


# * Lab Session 17 --------------------------------------------------------

hol_tourism <- 
  tourism %>% 
  filter(Purpose == "Holiday")


hol_mod <-  hol_tourism %>% 
  model(
    hol_arima = ARIMA(Trips)
  )

glance(hol_mod) 

hol_mod %>% 
  forecast(
    h = "4 years"
  ) %>% 
  autoplot(
    hol_tourism
  )


hol_mod %>% 
  filter(Region == "Barossa") %>%
  forecast( h = "4 years") %>% 
  autoplot(hol_tourism)


hol_mod %>% 
  filter(Region == "Adelaide") %>%
  forecast( h = "4 years") %>% 
  autoplot(hol_tourism)

hol_mod %>% 
  filter(Region == "Adelaide") %>%
  forecast( h = "4 years") %>% 
  autoplot(hol_tourism)



# * Lab Session 18 --------------------------------------------------------



# * Lab Session 20 --------------------------------------------------------


toursim_agg <- tourism %>% 
  aggregate_key(Purpose * (State / Region),
                Trips = sum(Trips))

fc <- toursim_agg %>% 
  filter_index(. ~ yearquarter("2015 Q4")) %>% 
  model(ets = ETS(Trips)) %>% 
  reconcile(ets_adjusted = min_trace(ets)) %>% 
  forecast(h = "2 years")


fc %>% 
  filter(is_aggregated(Purpose) & is_aggregated(State)) %>% 
  autoplot(toursim_agg, level = 95)

pbs_agg <- PBS %>% 
  aggregate_key(Concession / Type / ATC1, Costs = sum(Cost)) 

pbs_agg_mod <- pbs_agg %>% 
  filter(year(Month) <= max(year(Month) - 3)) %>% 
  model(
    ets = ETS(Costs),
    arima = ARIMA(Costs),
    snaive = SNAIVE(Costs)
  )


## His solutions

pbs_aggregate <- PBS %>% 
  aggregate_key(Concession * Type * ATC1, 
  Cost = sum(Cost) / 1e6
  )

fit <- pbs_aggregate %>% 
  filter(Month <= yearmonth("2005 June")) %>% 
  model(
    ets = ETS(Costs),
    arima = ARIMA(Costs),
    snaive = SNAIVE(Costs)
  )

fc <- fit %>% 
  reconcile(
    ets_adj = min_trace(ets),
    arima_adj = min_trace(arima),
    snaive_adj = min_trace(snaive)
  ) %>% 
  forecast(h = "3 years")

accuracy(fc, pbs_aggregate) %>% 
  group_by(.model) %>% 
  summarise(MASE = mean(MASE)) %>% 
  arrange(MASE)
  


