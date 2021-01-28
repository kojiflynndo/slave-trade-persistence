# This is code for replicating the original analysis in Nunn 2008.

library(tidyverse)
library(tidymodels)
library(moderndive)

# For loading .dta files from Stata
library(haven)

# Loads the data
slave_trade <- read_dta('data/slave_trade_QJE.dta')


# Replication of table III -----
# Basic bivariate linear regression
basic_model <- lm(formula = ln_maddison_pcgdp2000 ~ ln_export_area,
                  data = slave_trade)

get_regression_table(basic_model)


# Model 1: Controls for colonizer fixed effects
colonizer_1 <- lm(formula = ln_maddison_pcgdp2000 ~ ln_export_area + colony0 + 
                    colony1 + colony2 + colony3 + colony4 +
                    colony5 + colony6 + colony7,
                  data = slave_trade)

get_regression_table(colonizer_1)

# Model 2: Adds geographic indicators
geographic_2 <- lm(formula = ln_maddison_pcgdp2000 ~ ln_export_area +
                     colony0 + colony1 + colony2 + colony3 + colony4 +
                     colony5 + colony6 + colony7 + abs_latitude +
                     longitude + rain_min + humid_max + low_temp +
                     ln_coastline_area,
                        data = slave_trade)

get_regression_table(geographic_2)

# Model 3: Omits North African and island countries

iso_exclude <- c('MAR', 'DZA', 'TUN', 'LBY', 'EGY', 'SYC', 'MUS', 'COM', 'STP', 'CPV')

northaf_islands_3 <- lm(formula = ln_maddison_pcgdp2000 ~ ln_export_area +
                          colony0 + colony1 + colony2 + colony3 + colony4 +
                          colony5 + colony6 + colony7 + abs_latitude +
                          longitude + rain_min + humid_max + low_temp +
                          ln_coastline_area,
                        data = filter(slave_trade,
                                      !(isocode %in% iso_exclude))
                        )

get_regression_table(northaf_islands_3)

# Model 4: Returns omitted countries, adds North Africa fixed effect, island
#           fixed effect, percentage of population that is Islamic, 
#           and French legal origin

northaf_islands_4 <- lm(formula = ln_maddison_pcgdp2000 ~ ln_export_area +
                          colony0 + colony1 + colony2 + colony3 + colony4 +
                          colony5 + colony6 + colony7 + abs_latitude +
                          longitude + rain_min + humid_max + low_temp +
                          ln_coastline_area + island_dum + islam + legor_fr +
                          region_n,
                        data = slave_trade)


get_regression_table(northaf_islands_4)

# Model 5: Adds natural resource endowments

resource_endowment_5 <- lm(formula = ln_maddison_pcgdp2000 ~ ln_export_area +
                             colony0 + colony1 + colony2 + colony3 + colony4 +
                             colony5 + colony6 + colony7 + abs_latitude +
                             longitude + rain_min + humid_max + low_temp +
                             ln_coastline_area + island_dum + islam + legor_fr +
                             region_n + ln_avg_gold_pop + ln_avg_oil_pop + 
                             ln_avg_all_diamonds_pop,
                           data = slave_trade)


get_regression_table(resource_endowment_5)

# Model 6: All control variables and omit North African and island countries

all_and_drop_6 <- lm(formula = ln_maddison_pcgdp2000 ~ ln_export_area +
                   colony0 + colony1 + colony2 + colony3 + colony4 +
                   colony5 + colony6 + colony7 + abs_latitude +
                   longitude + rain_min + humid_max + low_temp +
                   ln_coastline_area + island_dum + islam + legor_fr +
                   region_n + ln_avg_gold_pop + ln_avg_oil_pop + 
                   ln_avg_all_diamonds_pop,
                 data = filter(slave_trade,
                               !(isocode %in% iso_exclude))
                 )

get_regression_table(all_and_drop_6)


# Replication of one standard deviation decrease -----

# "If for purely illustrative purposes one interprets the OLS estimates as causal, 
# then according to the estimate from column (5), for a country initially with 
# the mean level of income of $1,249, a one-standard-deviation decrease in the 
# slave export variable will raise income to $1,864,
# which is a 50% increase in income."

coeff <- resource_endowment_5[["coefficients"]][["ln_export_area"]]

# Natural log of income level associated with one standard deviation decrease in
#      unit of slave exports

osd_ln_income <- mean(slave_trade$ln_maddison_pcgdp2000) + 
  coeff*(-1*sd(slave_trade$ln_export_area))

# Income level associated with with one standard deviation decrease in unit of
#      slave exports
mean_income <- exp(mean(slave_trade$ln_maddison_pcgdp2000))
osd_income <- exp(osd_ln_income)

# Absolute gap between mean and one standard deviation decrease in unit of slave
#      exports
abs_gap <- osd_income - mean_income
pc_gap <- abs_gap/mean_income

# Deciphering normalization by area -----

slave_trade$total_enslaved <- (exp(slave_trade$ln_export_area))*(slave_trade$land_area)*(1000)