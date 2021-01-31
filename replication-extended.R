# This is the code for extending the GDP figures and replicating the analysis in
# Nunn 2008

library(tidyverse)
library(tidymodels)
library(moderndive)
# For loading .dta files from Stata
library(haven)

# ------------------------------

# Extending the dataset

# Loads original and new data
slave_trade <- read_dta('slave_trade_QJE.dta')
slave_trade$isocode[slave_trade$isocode == 'ZAR'] <- 'COD'

new_gdp <- read.csv('gdp-phm-1950.csv',
                    strip.white = TRUE)


# Merges new GDP figures with original dataset, both 2000 (for comparison) and
# 2015 (for replication)

# Merging 2015 data
gdp_2015 <- new_gdp %>%
  select(Country, ISO3, Year, GDP2015 = Maddison.ID..1990.base.year.) %>%
  filter(ISO3 %in% slave_trade$isocode,
         Year == 2015)
gdp_2015$ln_GDP2015 <- log(as.numeric(gsub(',', '', gdp_2015$GDP2015)))


extended_slave_trade <- merge(x = slave_trade,
                         y = gdp_2015[, c('ISO3', 'ln_GDP2015')],
                         by.x = 'isocode',
                         by.y = 'ISO3')

# Merging 2000 data
gdp_2000 <- new_gdp %>%
  select(Country, ISO3, Year, GDP2000 = Maddison.ID..1990.base.year.) %>%
  filter(ISO3 %in% slave_trade$isocode,
         Year == 2000)
gdp_2000$ln_GDP2000 <- log(as.numeric(gsub(',', '', gdp_2000$GDP2000)))

extended_slave_trade <- merge(x = extended_slave_trade,
                         y = gdp_2000[, c('ISO3', 'ln_GDP2000')],
                         by.x = 'isocode',
                         by.y = 'ISO3')



# ------------------------------

# Visualizing the differences in GDP figures

comparison <- extended_slave_trade %>%
  select(country, ln_maddison_pcgdp2000, ln_GDP2000, ln_GDP2015) %>%
  mutate(n2000_sub_madd = ln_GDP2000 - ln_maddison_pcgdp2000,
         n2015_sub_madd = ln_GDP2015 - ln_maddison_pcgdp2000)


new2000_v_madd <- ggplot(comparison,
       aes(x = ln_maddison_pcgdp2000,
           y = ln_GDP2000)) +
  geom_point() +
  geom_smooth(se = FALSE,
              method = 'lm') +
  labs(title = 'Points are countries') +
  xlim(4, 10) +
  ylim(4, 10)

new2015_v_madd <- ggplot(comparison,
       aes(x = ln_maddison_pcgdp2000,
           y = ln_GDP2015)) +
  geom_point() +
  geom_smooth(se = FALSE,
              method = 'lm') +
  labs(title = 'Points are countries') +
  xlim(4, 10) +
  ylim(4, 10)



# ------------------------------

# Creating and testing linear models on extended dataset using 2015 GDP figures
basic_model <- lm(formula = ln_GDP2015 ~ ln_export_area,
                  data = extended_slave_trade)

get_regression_table(basic_model)

# Model 1: Controls for colonizer fixed effects
colonizer_1 <- lm(formula = ln_GDP2015 ~ ln_export_area + colony0 + 
                    colony1 + colony2 + colony3 + colony4 +
                    colony5 + colony6 + colony7,
                  data = extended_slave_trade)

get_regression_table(colonizer_1)

# Model 6: All control variables and omit North African and island countries
iso_exclude <- c('MAR', 'DZA', 'TUN', 'LBY', 'EGY', 
                 'SYC', 'MUS', 'COM', 'STP', 'CPV')

all_and_drop_6 <- lm(formula = ln_GDP2015 ~ ln_export_area +
                       colony0 + colony1 + colony2 + colony3 + colony4 +
                       colony5 + colony6 + colony7 + abs_latitude +
                       longitude + rain_min + humid_max + low_temp +
                       ln_coastline_area + island_dum + islam + legor_fr +
                       region_n + ln_avg_gold_pop + ln_avg_oil_pop + 
                       ln_avg_all_diamonds_pop,
                     data = filter(extended_slave_trade,
                                   !(isocode %in% iso_exclude))
)

get_regression_table(all_and_drop_6)



# Creating and testing linear models on extended dataset using 2000 GDP figures
basic_model <- lm(formula = ln_GDP2000 ~ ln_export_area,
                  data = extended_slave_trade)

get_regression_table(basic_model)

# Model 1: Controls for colonizer fixed effects
colonizer_1 <- lm(formula = ln_GDP2000 ~ ln_export_area + colony0 + 
                    colony1 + colony2 + colony3 + colony4 +
                    colony5 + colony6 + colony7,
                  data = extended_slave_trade)

get_regression_table(colonizer_1)

# Model 6: All control variables and omit North African and island countries
iso_exclude <- c('MAR', 'DZA', 'TUN', 'LBY', 'EGY', 
                 'SYC', 'MUS', 'COM', 'STP', 'CPV')

all_and_drop_6 <- lm(formula = ln_GDP2000 ~ ln_export_area +
                       colony0 + colony1 + colony2 + colony3 + colony4 +
                       colony5 + colony6 + colony7 + abs_latitude +
                       longitude + rain_min + humid_max + low_temp +
                       ln_coastline_area + island_dum + islam + legor_fr +
                       region_n + ln_avg_gold_pop + ln_avg_oil_pop + 
                       ln_avg_all_diamonds_pop,
                     data = filter(extended_slave_trade,
                                   !(isocode %in% iso_exclude))
)

get_regression_table(all_and_drop_6)
