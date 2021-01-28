library(tidyverse)
library(tidymodels)
library(haven)


# ------------------------------

# Merging the datasets

slave_trade <- read_dta('data/slave_trade_QJE.dta')
slave_trade$isocode[slave_trade$isocode == 'ZAR'] <- 'COD'

raw_gdp <- read.csv('data/gdp_phm_1950.csv',
                    strip.white = TRUE)


new_gdp <- raw_gdp %>%
  select(ISO3, Year, pcGDP = Maddison.ID..1990.base.year.) %>%
  filter(ISO3 %in% slave_trade$isocode)
new_gdp$ln_pcGDP <- log(as.numeric(gsub(',', '', new_gdp$pcGDP)))


extended_dataset <- merge(x = slave_trade,
                          y = new_gdp[, c('ISO3', 'Year', 'ln_pcGDP')],
                          by.x = 'isocode',
                          by.y = 'ISO3'
                          )

# ------------------------------

# Defining each linear regression equation

base <- ln_pcGDP ~ ln_export_area

formula1 <- ln_pcGDP ~ ln_export_area + colony0 + 
  colony1 + colony2 + colony3 + colony4 +
  colony5 + colony6 + colony7

formula2 <- ln_pcGDP ~ ln_export_area +
  colony0 + colony1 + colony2 + colony3 + colony4 +
  colony5 + colony6 + colony7 + abs_latitude +
  longitude + rain_min + humid_max + low_temp +
  ln_coastline_area

formula3 <- ln_pcGDP ~ ln_export_area +
  colony0 + colony1 + colony2 + colony3 + colony4 +
  colony5 + colony6 + colony7 + abs_latitude +
  longitude + rain_min + humid_max + low_temp +
  ln_coastline_area

formula4 <- ln_pcGDP ~ ln_export_area +
  colony0 + colony1 + colony2 + colony3 + colony4 +
  colony5 + colony6 + colony7 + abs_latitude +
  longitude + rain_min + humid_max + low_temp +
  ln_coastline_area + island_dum + islam + legor_fr +
  region_n

formula5 <- ln_pcGDP ~ ln_export_area +
  colony0 + colony1 + colony2 + colony3 + colony4 +
  colony5 + colony6 + colony7 + abs_latitude +
  longitude + rain_min + humid_max + low_temp +
  ln_coastline_area + island_dum + islam + legor_fr +
  region_n + ln_avg_gold_pop + ln_avg_oil_pop + 
  ln_avg_all_diamonds_pop

formula6 <- ln_pcGDP ~ ln_export_area +
  colony0 + colony1 + colony2 + colony3 + colony4 +
  colony5 + colony6 + colony7 + abs_latitude +
  longitude + rain_min + humid_max + low_temp +
  ln_coastline_area + island_dum + islam + legor_fr +
  region_n + ln_avg_gold_pop + ln_avg_oil_pop + 
  ln_avg_all_diamonds_pop

fns <- c(base,
         formula1, formula2, formula3, 
         formula4, formula5, formula6)

# ------------------------------
iso_exclude <- c('MAR', 'DZA', 'TUN', 'LBY', 'EGY', 
                 'SYC', 'MUS', 'COM', 'STP', 'CPV')
excluding_models <- c(4, 7)

years <- (1950:2015)

# creates the df for storing data
time_series_df <- data.frame(
  Year = rep(0, 65),
  
  Base_Coeff = rep(0, 65),
  Base_Mean_Income = rep(0, 65),
  Base_Abs_Income_Gain = rep(0, 65),
  Base_PC_Income_Gain = rep(0, 65),
  
  M1_Coeff = rep(0, 65),
  M1_Mean_Income = rep(0, 65),
  M1_Abs_Income_Gain = rep(0, 65),
  M1_PC_Income_Gain = rep(0, 65),
  
  M2_Coeff = rep(0, 65),
  M2_Mean_Income = rep(0, 65),
  M2_Abs_Income_Gain = rep(0, 65),
  M2_PC_Income_Gain = rep(0, 65),
  
  M3_Coeff = rep(0, 65),
  M3_Mean_Income = rep(0, 65),
  M3_Abs_Income_Gain = rep(0, 65),
  M3_PC_Income_Gain = rep(0, 65),
  
  M4_Coeff = rep(0, 65),
  M4_Mean_Income = rep(0, 65),
  M4_Abs_Income_Gain = rep(0, 65),
  M4_PC_Income_Gain = rep(0, 65),
  
  M5_Coeff = rep(0, 65),
  M5_Mean_Income = rep(0, 65),
  M5_Abs_Income_Gain = rep(0, 65),
  M5_PC_Income_Gain = rep(0, 65),
  
  M6_Coeff = rep(0, 65),
  M6_Mean_Income = rep(0, 65),
  M6_Abs_Income_Gain = rep(0, 65),
  M6_PC_Income_Gain = rep(0, 65)
 
)

# loops through each year
for (i in seq_along(years)) {
  
  year = as.integer(years[[i]])

  # creates temporary list for storing results of this year
  ls_temp <- c(year)
  
  # creates the datasets that models will evaluate
  temp <- extended_dataset %>%
    filter(Year == year)
  temp_dropped <- temp %>%
    filter(!(isocode %in% iso_exclude))

  # identifies the models which drop north african states and islands
  count <- as.numeric(0)
  
  # loops through each model
  for (j in seq_along(fns)){
    
    count <- count + 1
    
    if (count %in% excluding_models) {
      
      model <- lm(formula = fns[[j]],
                  data = temp_dropped)
      
      # calculates the income difference between mean income and one standard
      # deviation decrease in ln_export_area
      coeff <- model[["coefficients"]][["ln_export_area"]]
      
      osd_ln_income <- mean(temp_dropped$ln_pcGDP) + 
        coeff*(-1*sd(temp_dropped$ln_export_area))
      
      mean_income <- exp(mean(temp_dropped$ln_pcGDP))
      osd_income <- exp(osd_ln_income)
      
      abs_gap <- osd_income - mean_income
      pc_gap <- abs_gap/mean_income
    } 
    else {
      
      model <- lm(formula = fns[[j]],
                  data = temp)
      
      # calculates the income difference between mean income and one standard
      # deviation decrease in ln_export_area
      coeff <- model[["coefficients"]][["ln_export_area"]]
      
      osd_ln_income <- mean(temp$ln_pcGDP) + 
        coeff*(-1*sd(temp$ln_export_area))
      
      mean_income <- exp(mean(temp$ln_pcGDP))
      osd_income <- exp(osd_ln_income)
      
      abs_gap <- osd_income - mean_income
      pc_gap <- abs_gap/mean_income
    }
    

    # adds the coefficient results to the temporary list
    ls_temp[2+(j-1)*4] <- coeff
    
    # adds the mean income to the temporary list
    ls_temp[3+(j-1)*4] <- mean_income
    
    # adds absolute income gap to temporary list
    ls_temp[4+(j-1)*4] <- abs_gap
    
    # adds percent income gap to temporary list
    ls_temp[5+(j-1)*4] <- pc_gap
    
  }
  
  # adds the results for the year to the dataset
  time_series_df[i,] <- ls_temp
  
}


write_csv(time_series_df,
          'data/time_series_effects.csv')

ggplot(time_series_df) +
  geom_line(aes(x = Year,
                y = M5_Mean_Income)) +
  geom_line(aes(x = Year,
                y = M6_Mean_Income + M5_Abs_Income_Gain)) +
  labs(title = 'Mean income vs income associated with one standard deviation reduction in slaves stolen')

ggplot(time_series_df) +
  geom_line(aes(x = Year,
                y = M5_PC_Income_Gain))
            