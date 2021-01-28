



# Setup   ------------------------------


# Load Libraries     ------------------------------
library(shiny)
library(tidyverse)
library(ggthemes)
library(viridis)
library(plotly)
library(maptools)
library(sf)
library(haven)
library(grid)
library(gridExtra)
library(rgeos)
library(ggtext)
library(gganimate)
library(gifski)
library(kableExtra)

# Load dataset that are used in multiple places    ----------------
slave_trade <- read_dta('data/slave_trade_QJE.dta')

slave_trade$isocode[slave_trade$isocode == 'ZAR'] <- 'COD'

gdp <- read.csv('data/gdp_phm_1950.csv',
                strip.white = TRUE)

gdp_2015 <- gdp %>%
    select(Country, ISO3, Year, GDP2015 = Maddison.ID..1990.base.year.) %>%
    filter(ISO3 %in% slave_trade$isocode,
           Year == 2015)
gdp_2015$ln_GDP2015 <-
    log(as.numeric(gsub(',', '', gdp_2015$GDP2015)))

slave_trade_2015 <- merge(x = slave_trade,
                          y = gdp_2015[, c('ISO3', 'ln_GDP2015')],
                          by.x = 'isocode',
                          by.y = 'ISO3')

renamed_slave_trade <- slave_trade_2015 %>%
    rename(
        ISO = isocode,
        Country = country,
        Ln_per_cap_GDP_2000 = ln_maddison_pcgdp2000,
        Ln_per_cap_GDP_2015 = ln_GDP2015,
        Ln_Enslavement_by_Area = ln_export_area,
        Ln_Enslavement_by_Population = ln_export_pop,
        Precolonial_State_Development = state_dev,
        Ethnic_Fractionalization = ethnic_fractionalization,
        Land_Area = land_area,
        Ln_Population_Density_1400 = ln_pop_dens_1400
    ) %>%
    mutate(
        Per_cap_GDP_2000 = as.integer(exp(Ln_per_cap_GDP_2000)),
        Per_cap_GDP_2015 = as.integer(exp(Ln_per_cap_GDP_2015)),
        Enslavement_by_Area = exp(Ln_Enslavement_by_Area)
    )

enslavement_by_country <- read.csv('data/enslavement_by_country.csv')
renamed_slave_trade <- merge(x = renamed_slave_trade,
                             y = enslavement_by_country,
                             by.x = 'ISO',
                             by.y = 'iso3')


# Create ggplot themes     ------------------------------
custom <-
    theme_minimal() + theme(
        text = element_text(family = 'serif', size = 12),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#FFFAFA")
    )


map_custom <-
    theme_void() + theme(text = element_text(family = 'serif',
                                             size = 12),
                         legend.position = 'bottom')




# Define UI  ------------------------------
ui <- fluidPage(

    navbarPage(
        title = "The Persistent Economic Impacts of Africa's Slave Trades",
        
        # Intro Panel -----
        tabPanel(
            'Home',
            
            mainPanel(
                h1("The Persistent Economic Impacts of Africa's Slave Trades"),
                
                # Section: Introduction -----
                p(
                    "The world has become much wealthier in the past several centuries since the Industrial Revolution. Extreme poverty (less than $1.90 per day) has declined from 94.4 percent in 1820 to 9.6 percent in 2015, though the COVID-19 pandemic has reversed a consistent trend toward declining worldwide poverty for the first time in decades. While global trends have been encouraging (prior to COVID-19), sub-Saharan Africa has seen an increase in absolute extreme poverty from the 1990s to 2015. Almost half a billion Africans suffer from extreme poverty."
                ),
                
                p(
                    "What explains why many African countries have largely remained so poor while the rest of the world has become much wealthier? Two legacies stand out: colonization and the slave trades. Many historians contend that these historical experiences contributed to negative structural features in contemporary African societies, including high corruption, poor institutional functioning, and a lack of legitimacy in governments."
                ),
                
                p(
                    "Of the two, the slave trades lasted much longer. Official colonial rule lasted from around 1885 to 1960, compared with the slave trades' nearly 500-year duration from approximately 1400 to 1900. Over that period, approximately 18 million enslaved Africans were stolen and sold in the trans-Atlantic, trans-Saharan, Red Sea, and Indian Ocean slave trades-an unprecedented volume which sets them apart from previous slave trades. Also setting them apart is the fact that individuals from the same ethnicities enslaved each other to be sold into the trade, as opposed to previous enslavement by capture. It stands to reason that this could have increased ethnic fractionalization, broken bonds of trust between individuals, and destroyed governance structures."
                ),
                
                p(
                    "This is exactly what the historical record and the data show; if we divide countries into two groups, one with countries that experienced above-median enslavement (proportional to their land area) and the other with countries that experience below-median enslavement, the below-median group's average per capita income has at times exceeded 150 percent of the above-median group's."
                ),
                
                h3(
                    "Figure 1: Differing Paths of Economic Development Since 1950"
                    ),
                
                h4(
                    HTML(
                        'Groups of 26 African countries; <span style = "color: #d95f02">Above-Median</span> and <span style = "color: #1b9e77">Below-Median</span> Enslavement'
                    )
                ),
                
                plotOutput(outputId = 'growth_trajectories'),
                
                p(),
                p(),
                
                p(
                    "To what extent can the paths of subsequent economic development be attributed to Africa's slave trades? The central question here is if those centuries-old effects persist and what causal mechanisms we can confidently claim to identify."
                ),
                
                
                # Section: Existing Research -----
                h2(
                    "Lessons from Existing Research"
                ),
                
                p(
                   "The dataset I visualize here is drawn from a paper by economist Nathan Nunn, 'The Long-Term Effects of Africa's Slave Trades' (Quarterly Journal of Economics, 2008). After controlling for a variety of plausibly relevant factors, Nunn finds a statistically significant negative relationship between measures of people enslaved and stolen (normalized by area) and per capita GDP in 2000. (Visualize those results in the Plot and Map Relationships pages.) Turning to the question of causality, Nunn tests a several alternative hypotheses and finds that none pass muster. For instance, it could be that, for example, societies that were already underdeveloped tended to be selected into the slave trade and so continue to be today for that reason, rather than the slave trade. In fact, the opposite seems true--it was the wealthiest African societies which saw the most enslavement. (See Methodological Notes for more details.) Instead, he suggests that the causal mechanism was the slave trade's deletirious influence on early state development and, in later research, points to the mistrust engendered by the slave trades as one cause."
                ),
                
                p(
                    "The implications for explaining contemporary poverty and prosperity are significant. Starting from a country with mean per capita income in 2000 ($1,249), a one-standard-deviation decrease in the variable measuring intensity of enslavement is associated with per capita income rising to $1,864, or a 50 percent increase in per capita income levels (using Model 5)."
                ),
                
                # Section: New Analysis -----
                h2(
                    "Results of New Analysis"
                ),
                
                p(
                    "The main contribution of my work here is extending this analysis using per capita GDP figures from 1950 to 2015. In the original paper, the measure of contemporary economic performance was per capita GDP in 2000. In this analysis, I evaluate the persistent effects of enslavement across more than six decades. For each year, I analyze each of Nunn's seven regression models and compile the results."
                ),
                
                p(
                    "Similar to above, I have calculated the magnitude of the effects on per capita income associated with a one-standard-deviation decrease in the variable measuring intensity of enslavement for each year in my time-series. The absolute difference in per capita income steadily grows over the entire time-series, reaching $677 per capita in 2015. The relative difference, on the other hand, peaks in 2000. At that point, a one-standard-deviation decrease in the variable measuring intensity of enslavement was associated with per capita income rising to $1,864 from the mean of $1,264, or a 49 percent increase in per capita income levels."
                ),
                
                p(
                    "In actual figures, one standard-deviation in the variable measuring intensity of enslavement corresponds with approximately 147,700 people stolen, enslaved, and sold into the slave trades per ten thousand square kilometers."
                ),
                
                
                
                h3(
                    "Figure 2: Enslavement's Persistent Effects on Poverty and Prosperity"
                ),
                
                h4(
                    HTML(
                    "<span style = 'color: #e5c494'>Mean income</span> and <span style = 'color: #8da0cb'>income associated one-standard-deviation lower enslavement</span>"
                
                    )
                ),
                
                plotOutput(outputId = 'sd_decrease'),
                
                p(),
                p(),
                
                p(
                    "One argument explaining the long term effects of the slave trade suggests that the impacts may have been most significant after official decolonization because precolonial state capacity and centralization then became highly influential on political and economic development. Consistent with that hypothesis, I find that the magnitude of the effect trends upward starting around 1960, at which point a majority (28) of African countries had gained independence."
                ),
                
                h3(
                    "Figure 3: The Magnitude of the Slave Trades' Legacy Increased After Decolonization"
                ),
    
                
                h4(
                    HTML(
                        "Size of income increase associated one-standard-deviation lower enslavement
                        <br>
                        <span style = 'color: #e78ac3'>Regression model with best controls</span> highlighted"
        
                    )
                ),
                
                plotOutput(outputId = 'effect_size'),
                
                p(),
                p(),
                
                p(
                    "What explains the seeming decline in importance around 2000? This is a question that may have already been answered in the vast literature on the economic history of persistence, but I am currently unaware of the reason. One observation is that around 2000, average per capita GDP began to grow at a much more rapid pace than in the previous decades. As noted above, the absolute effect has only grown and peaks in 2015, the last year I had data to analyze. It may be that the relative effect diminishes in the future as African countries become wealthier."
                ),
                
                h2(
                    "Conclusion"
                ),
                
                p(
                    "The historical experience of five centuries of slave trades profoundly damaged the trajectory of African societies' politics, economics, and social structure. Contemporary researchers in development economics, evolutionary anthropology, and quantitative history focusing on the long-run persistence of historical events continue to shed light on the importance of the past to the future. Here, I visualize one of the most striking findings from this research agenda and extend the analysis to better grapple with the effects of enslavement on our world today."
                ),
                
                p(
                    "Explore the data by creating your own visualizations of the research by clicking to the Plot and Map Relationships page."
                ),
                
                p(),
                p(),
                p(),
                p(),
                p()
            )
            
        ),
        
        # Plots Panel -----
        tabPanel(
            'Plot Relationships',
            
            sidebarLayout(
                sidebarPanel(
                    
                    # Select variable for y-axis
                    selectInput(
                        inputId = "y",
                        label = 'Outcome variable (y-axis):',
                        choices = c(
                            'Natural logarithm, per capita GDP 2000' = 'Ln_per_cap_GDP_2000',
                            'Per capita GDP 2000' = 'Per_cap_GDP_2000',
                            
                            'Natural logarithm, per capita GDP 2015' = 'Ln_per_cap_GDP_2015',
                            'Per capita GDP 2015' = 'Per_cap_GDP_2015',
                            
                            'Ethnic fractionalization' = 'Ethnic_Fractionalization',
                            
                            'Precolonial state development' = 'Precolonial_State_Development'
                        ),
                        selected = 'Ln_per_cap_GDP_2000'
                    ),
                    
                    selectInput(
                        inputId = "x",
                        label = 'Explanatory variable (x-axis):',
                        choices = c(
                            'Natural logarithm, number of enslaved normalized by area' = 'Ln_Enslavement_by_Area',
                            
                            'Precolonial state development' = 'Precolonial_State_Development',
                            
                            'Ethnic fractionalization' = 'Ethnic_Fractionalization'
                        ),
                        selected = 'Ln_Enslavement_by_Area'
                    ),
                    
                    checkboxInput(
                        inputId = 'size_yn',
                        label = 'Size points?',
                        value = TRUE
                    ),
                    
                    selectInput(
                        inputId = "size",
                        label = 'Scale point size by:',
                        choices = c(
                            'Land area' = 'Land_Area',
                            
                            'Per capita GDP 2000' = 'Per_cap_GDP_2000',
                            
                            'Per capita GDP 2015' = 'Per_cap_GDP_2015',
                    
                            'Natural logarithm, number of enslaved normalized by area' = 'Ln_Enslavement_by_Area',
                            
                            'Natural logarithm, population density in 1400' = 'ln_pop_dens_1400'
                        ),
                        selected = 'Land_Area'
                    ),
                    
                    selectInput(
                        inputId = "color",
                        label = 'Shade points by:',
                        choices = c(
                            'Natural logarithm, per capita GDP 2000' = 'Ln_per_cap_GDP_2000',
                            
                            'Ethnic fractionalization' = 'Ethnic_Fractionalization',
                            
                            'Precolonial state development' = 'Precolonial_State_Development'
                        ),
                        selected = 'ln_maddison_pcgdp2000'
                    ),
                    
                    checkboxInput(
                        inputId = 'add_line',
                        label = 'Add Trend Line?',
                        value = TRUE
                    )
                ),
                
                mainPanel(
                    plotlyOutput(outputId = 'scatterplot'),
                    
                    
                    h3(
                        "How to Use This Tool"
                    ),
                    
                    p(
                        "On the sidebar, select which variables you would like to visualize relationships between, how you would like to size and shade the points, and if you would like to add a trend line to the relationship. Hover over the points to see which country they correspond to."
                    ),
                    
                    p(
                        "There are two color scales used for shading: one from yellow to red (for variables where higher scores indicate worse conditions) and the other from red to blue (for variables where higher scores indicate better conditions)."
                    ),
                    
                    p(
                        "By default, the visualization plots the core explanatory and outcome variables of the research: the x-axis shows the measure of intensity of enslavement and the y-axis shows the measure of poverty and prosperity. Points are sized by the land area of the country they represent and are shaded by the measure of poverty and prosperity."
                    ),
                    
                    p(
                        "For more information on what exactly these variables measure, go to the View Data page and click the Details tab."
                    )
                    
                    
                )
            )
            
        ),
        # Maps Panel ------
        tabPanel('Map Relationships',
                 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "map1fill",
                             label = 'Shade left map by:',
                             choices = c(
                                 'Total enslaved' = 'Total_Enslaved',
                                 
                                 'Natural logarithm, per capita GDP 2000' = 'Ln_per_cap_GDP_2000',
                                 
                                 'Per capita GDP 2000' = 'Per_cap_GDP_2000',
                                 
                                 'Natural logarithm, per capita GDP 2015' = 'Ln_per_cap_GDP_2015',
                                 
                                 'Per capita GDP 2015' = 'Per_cap_GDP_2015',
                                 
                                 'Ethnic fractionalization' = 'Ethnic_Fractionalization',
                                 
                                 'Precolonial state development' = 'Precolonial_State_Development',
                                 
                                 'Enslavement normalized by country area' = 'Enslavement_by_Area',
                                 
                                 'Natural logarithm, enslavement normalized by country area' = 'Ln_Enslavement_by_Area'
                             ),
                             selected = 'Ln_Enslavement_by_Area'
                         ),
                         
                         selectInput(
                             inputId = "map2fill",
                             label = 'Shade right map by:',
                             choices = c(
                                 'Natural logarithm, per capita GDP 2000' = 'Ln_per_cap_GDP_2000',
                                 
                                 'Per capita GDP 2000' = 'Per_cap_GDP_2000',
                                 
                                 'Natural logarithm, per capita GDP 2015' = 'Ln_per_cap_GDP_2015',
                                 
                                 'Per capita GDP 2015' = 'Per_cap_GDP_2015',
                                 
                                 'Ethnic fractionalization' = 'Ethnic_Fractionalization',
                                 
                                 'Precolonial state development' = 'Precolonial_State_Development',
                                 
                                 'Enslavement normalized by country area' = 'Enslavement_by_Area',
                                 
                                 'Natural logarithm, number of enslaved normalized by area' = 'Ln_Enslavement_by_Area'
                             ),
                             selected = 'Ln_per_cap_GDP_2000'
                         )
                     ),
                     
                     mainPanel(plotOutput(outputId = 'maps'),
                               
                               
                               h3(
                                   "How to Use This Tool"
                               ),
                               
                               p(
                                   "On the sidebar, select which variables you would like each map to be shaded by. There are two color scales used for shading: one from yellow to red (for variables where higher scores indicate worse conditions) and the other from red to blue (for variables where higher scores indicate better conditions)."
                               ),
                               
                               p(
                                   "By default, the maps plot the core explanatory and outcome variables of the research: the left map shows the measure of intensity of enslavement and the right map shows the measure of poverty and prosperity."
                               ),
                               
                               p(
                                   "For more information on what exactly these variables measure, go to the View Data page and click the Details tab."
                               )
                               
                               )
                 )),
        
        
        # Data Table Panel -----
        tabPanel('View Data',
                 
                 sidebarLayout(
                     sidebarPanel(
                         
                         checkboxGroupInput(
                             inputId = 'variables_dt',
                             label = 'Display:',
                             
                             # Add data on actual numbers of enslavement, from table II in Nunn 08
                             choices = c('Country' = 'Country',
                                         
                                         'ISO Code' = 'ISO',
                                         
                                         'Total stolen as slaves' = 'Total_Enslaved',
                                         
                                         'Natural logarithm, number of enslaved normalized by area' = 'Ln_Enslavement_by_Area',
                                         
                                         'Ethnic fractionalization index (Alesina et al. 2003)' = 
                                             'Ethnic_Fractionalization',
                                         
                                         'Precolonial state development index (Gennaioli & Rainer 2003)' = 
                                             'Precolonial_State_Development',
                                         
                                         'Land area (millions of square kms)' = 'Land_Area',
                                         
                                         'Per capita GDP, 2000' = 'Per_cap_GDP_2000',
                                         
                                         'Per capita GDP 2015' = 'Per_cap_GDP_2015'
                                         ),
                             
                             selected = c('Country',
                                          
                                          'Total_Enslaved',
                                          
                                          'Per_cap_GDP_2015')
                         ),
                         
                         sliderInput(
                             inputId = 'table_font_size',
                             label = 'Text Size',
                             min = 6,
                             max = 18,
                             value = 12
                         )
                         
                         
                     ),
                     
                     
                     mainPanel(
                         tabsetPanel(
                         tabPanel(
                                  'Data Table',
                                  
                                  tableOutput('data_table')
                                  ),
                         
                         tabPanel(
                             'Details',
                             
                             p(
                                 HTML(
                                 
                                     "Below are details for all variables available on the data table. The study includes more than 40 variables to control for a variety of plausibly relevant factors. To download the complete data used on this website, go to my <a href = 'https://github.com/kojiflynndo/slave-trade-persistence'>GitHub page</a>."
                                     
                                 )
                             ),
                             
                             h4(
                                 "Geographic Variables"
                             ),
                             
                             p(
                                 "Country: Country name"
                             ),
                             
                             p(
                                 "ISO: ISO country codes from the International Organization for Standardization"
                             ),
                             
                             p(
                                 "Land Area: Country land area in millions of square kilometers (from Parker 1997)"
                             ),
                             
                             p(),
                             
                             
                             
                             h4(
                                 "Enslavement Variables"
                             ),
                             
                             p(
                                 "Total enslaved: Estimated number of people enslaved from  country (from Nunn 2008)"
                             ),
                             
                             p(
                                 "Natural logarithm, enslavement normalized by area: Natural logarithm of people enslaved from country, normalized by area. Units are in ln(number of people per 1000 square kilometers) (from Nunn 2008)"
                             )
                            ,
                            
                            p(
                                "Enslavement normalized by country area: Number of people enslaved from country per 1000 square kilometers"
                            ),
                            
                            p(),
                            
                            
                            
                            h4(
                                "Economic Variables"
                            ),
                            
                            p(
                                "Natural logarithm, per capita GDP 2000/2015: Natural logarithm of per capita GDP in USD (from Angus Maddison 2003 and James et al. 2012)"
                            ),
                            
                            p(
                                "Per capita GDP 200/2015: Per capita GDP in USD (from Angus Maddison 2003 and James et al. 2012)"
                            ),
                            
                            
                            
                            h4(
                                "Socio-Political Variables"
                            ),
                            
                            p(
                                "Ethnic fractionalization: Index of ethnic fractionalization, ranging from 0 to 1. Mean index for sub-Saharan Africa is 0.658 (from Alesina et. al 2003)"
                            ),
                            
                            p(
                                "Precolonial state development: Index of precolonial state development, ranging from 0 to 1. Includes political, ethnic, and infrastructural centralization, for example (from Gennaoili and Rainer 2007)"
                            )
                            
                             
                              )
                         )
                     )
                 )),
        # Methodological Notes Panel -----
        tabPanel('Methodological Notes',
                 
                 mainPanel(
                     h3(
                         'Forthcoming! This page is under construction'
                     ),
                     
                     p(
                         HTML(
                             "For now, you can see the dataset and code on my <a href = 'https://github.com/kojiflynndo/slave-trade-persistence'>GitHub page</a>."
                         )
                     )
                 )
                 ),
        
        # Citations and Resources Panel -----
        tabPanel('Citations and Resources',
                 
                 mainPanel(
                     h3(
                         'Forthcoming! This page is under construction'
                     ),
                     
                     p(
                         HTML(
                             "For now, you can see the dataset and code on my <a href = 'https://github.com/kojiflynndo/slave-trade-persistence'>GitHub page</a>."
                         )
                     )
                 )
                 ),
        
        # Import CSS theme-----
        tags$head(
            tags$link(rel = "stylesheet", 
                      type = "text/css", 
                      href = "custom.css")
        )
    )
)


# Define server logic   ------------------------------
server <- function(input, output) {
    time_series_data <- read_csv('data/time_series_effects.csv')
    # Defining Color Scale Directions -----
    pos <- c('Ln_per_cap_GDP_2000',
             'Per_cap_GDP_2000',
             
             'Ln_per_cap_GDP_2015',
             'Per_cap_GDP_2015',
             
             'Precolonial_State_Development')
    
    neg <- c('Ln_Enslavement_by_Area',
             
             'Total_Enslaved',
             
             'Enslavement_by_Area', 
             
             'Ethnic_Fractionalization')
    
    # Scatterplot Output -----
    output$scatterplot <- renderPlotly({
        p1 <- ggplot(
            data = renamed_slave_trade,
            mapping = aes_string(x = input$x,
                                 y = input$y),
            alpha = 0.3
        ) +
            custom +
            labs(x = str_to_title(str_replace_all(input$x, "_", " ")),
                 y = str_to_title(str_replace_all(input$y, "_", " ")))
        
        if (input$color %in% pos) {
            color_scheme <- 'RdYlBu'
            direc <- 1
        } else if (input$color %in% neg) {
            color_scheme <- 'YlOrRd'
            direc <- 1
        }
        
        if (input$size_yn) {
            p1 <- p1 + geom_point(aes_string(
                size = input$size,
                color = input$color,
                text = 'Country'
            ))  +
                scale_color_distiller(palette = color_scheme,
                                      direction = direc,
                                      guide = 'none') +
                scale_size_continuous(guide = 'none')
        } else {
            p1 <- p1 + geom_point(aes_string(color = input$color,
                                             text = 'Country'))  +
                scale_color_distiller(palette = color_scheme,
                                      direction = direc,
                                      guide = 'none')
            
        }
        
        if (input$add_line) {
            p1 <- p1 + geom_smooth(
                formula = y ~ x,
                method = 'lm',
                se = FALSE,
                color = 'gray30',
                alpha = 0.5
            )
        }
        
        ggplotly(p1, tooltip = 'text')
        
    })
    
    
    # Maps Output ------
    output$maps <- renderPlot({
        if (input$map1fill %in% pos) {
            color_scheme1 <- 'RdYlBu'
            direc1 <- 1
        } else if (input$map1fill %in% neg) {
            color_scheme1 <- 'YlOrRd'
            direc1 <- 1
        }
        
        if (input$map2fill %in% pos) {
            color_scheme2 <- 'RdYlBu'
            direc2 <- 1
        } else if (input$map2fill %in% neg) {
            color_scheme2 <- 'YlOrRd'
            direc2 <- 1
        }
        
        
        # create the base map
        data(wrld_simpl)
        afr <- wrld_simpl[wrld_simpl$REGION == 2,]
        afr_proj <- afr %>%
            st_as_sf %>%
            st_transform(3857)
        
        map_data <- merge(
            x = afr_proj,
            y = renamed_slave_trade, ## CHANGE MADE HERE--switched which dataset
            by.x = 'ISO3',
            by.y = 'ISO',
            all = TRUE
        )
        
        map <- ggplot(data = map_data)
        
        
        map1 <- ggplot(data = map_data) +
            geom_sf(mapping = aes_string(fill = input$map1fill)) +
            scale_fill_distiller(palette = color_scheme1,
                                 direction = direc1) +
            guides(fill = 'colorbar') +
            theme_void() +
            map_custom + 
            theme(legend.title=element_blank())
        
        map2 <- ggplot(data = map_data) +
            geom_sf(mapping = aes_string(fill = input$map2fill)) +
            scale_fill_distiller(palette = color_scheme2,
                                 direction = direc2) +
            guides(fill = 'colorbar') +
            theme_void() +
            map_custom +
            theme(legend.title=element_blank())
        
        grid.arrange(map1, map2, ncol = 2)
    })
    
    
    
    
    # Growth Trajectories Plot Output -----
    output$growth_trajectories <- renderPlot({
        
        # Data wrangling for the weighted per capita GDP trajectories graph
        export_levels <- read_csv('data/geo_and_slave_trade.csv')
        population <- read_csv('data/population_by_country.csv')
        
        export_levels$high_export <- export_levels$ln_export_area >
            median(export_levels$ln_export_area, na.rm = TRUE)
        export_levels <-
            export_levels[!is.na(export_levels$high_export),]
        
        pop2015 <- population %>%
            select(
                Country,
                Country_Code,
                Population2015 = Year_2015,
                Population2000 = Year_2000
            ) %>%
            filter(Country_Code %in% export_levels$ISO3)
        
        sum2015 <- sum(pop2015$Population2015)
        
        # gets per capita gdp, then converts to numerics
        gdp_countries <- gdp %>%
            select(Country, ISO3, Year, GDP = Maddison.ID..1990.base.year.) %>%
            filter(ISO3 %in% export_levels$ISO3)
        gdp_countries$GDP <-
            as.numeric(gsub(',', '', gdp_countries$GDP))
        
        # weights per capita gdp by population in 2015
        gdp_countries <-
            merge(
                x = gdp_countries,
                y = pop2015,
                by.x = 'ISO3',
                by.y = 'Country_Code'
            ) %>%
            mutate(Weighted2015 = GDP * Population2015,
                   Country = Country.x)
        
        # groups per capita gdp by high/low exports
        gdp_grouped <- merge(
            x = gdp_countries,
            y = export_levels[, c("ISO3", "high_export")],
            by.x = 'ISO3',
            by.y = 'ISO3'
        ) %>%
            pivot_wider(
                id_cols = c(Year, high_export),
                names_from = Country.x,
                values_from = Weighted2015
            )
        
        # sums to get weighted average of per capita gdp by high/low exports
        total <- gdp_grouped %>%
            mutate(TotalGDP = select(., Angola:Zimbabwe)) %>%
            rowSums(na.rm = TRUE)
        gdp_grouped$WeightedPerCapGDP <- total / sum2015
        
        growth_static <- gdp_grouped %>%
            select(Year, HighExport = high_export, WeightedPerCapGDP) %>%
            mutate(Export = ifelse(
                HighExport == TRUE,
                'Higher\nEnslavement',
                'Lower\nEnslavement'
            )) %>%
            ggplot(mapping = aes(
                x = Year,
                y = WeightedPerCapGDP,
                color = HighExport
            )) +
            geom_line(size = 1.5,
                      alpha = 0.7) +
            scale_color_brewer(palette = 'Dark2',
                               guide = NULL) +
            scale_y_continuous(
                name = 'Average per capita GDP',
                breaks = seq(500, 2500, by = 500),
                labels = paste('$', seq(500, 2500, by = 500)),
                limits = c(500, 2500)
            ) +
            scale_x_continuous(name = NULL,
                               breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
            custom
        
        growth_anim <- growth_static +
            geom_segment(aes(xend = 2017,
                             yend = WeightedPerCapGDP)) +
            geom_text(aes(
                x = 2017,
                label = Export,
                hjust = 0
            )) +
            xlim(1950, 2025) +
            coord_cartesian(clip = 'off')
        
        # for now, will learn to display with gganimate later
        growth_static
        
        #animate(growth_anim + transition_reveal(Year), nframes = 20)
    })
    
    # Magnitude of Effect Plot Output -----
    output$effect_size <- renderPlot({
        
        effect_size <- ggplot(time_series_data) +
            
            geom_line(
                aes(x = Year,
                    y = M5_PC_Income_Gain),
                color = '#e78ac3',
                alpha = 1,
                size = 1.5) +
            geom_line(
                aes(x = Year,
                    y = M1_PC_Income_Gain),
                color = '#fc8d62',
                alpha = 0.2,
                size = .8) +
            geom_line(
                aes(x = Year,
                    y = M2_PC_Income_Gain),
                color = '#66c2a5',
                alpha = 0.2,
                size = .8) +
            geom_line(
                aes(x = Year,
                    y = M3_PC_Income_Gain),
                color = '#8da0cb',
                alpha = 0.2,
                size = .8) +
            geom_line(
                aes(x = Year,
                    y = M4_PC_Income_Gain),
                color = '#a6d854',
                alpha = 0.2,
                size = .8) +
            geom_line(
                aes(x = Year,
                    y = M6_PC_Income_Gain),
                color = '#ffd92f',
                alpha = 0.2,
                size = .8) +
            custom + 
            scale_y_continuous(name = '% increase in income associated with 1 s.d. decline',
                               breaks = c(.1, .2, .3, .4, .5, .6),
                               labels = scales::percent) +
            scale_x_continuous(name = NULL,
                               breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010))
        
            effect_size
        
    })
    
    # Standard Deviation Decrease Plot -----
    output$sd_decrease <- renderPlot({
        # Aiming to also animate this or make it interactive
        
        
        # One std deviation decrease in slaves stolen -----
        sd_decrease_plot <- ggplot(time_series_data) +
            # Sample mean
            geom_line(aes(x = Year,
                          y = M5_Mean_Income),
                      color = '#e5c494',
                      size = 1.5) +
            
            # Standard deviation decrease in exports
            geom_line(aes(x = Year,
                          y = M5_Mean_Income + M5_Abs_Income_Gain),
                      color = '#8da0cb',
                      size = 1.5) +
            scale_x_continuous(name = NULL,
                               breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
            scale_y_continuous(
                name = 'Per capita GDP',
                breaks = seq(500, 2500, by = 500),
                labels = paste('$', seq(500, 2500, by = 500)),
                limits = c(500, 2500)
            ) +
            custom
        
        sd_decrease_plot
    })
    
    
    # Data Table Output -----
    output$data_table <- function() {
        
        
        renamed_slave_trade %>%
            select(input$variables_dt) %>%
            kbl() %>%
            kable_styling(full_width = F,
                          
                          position = 'left',
                          
                          fixed_thead = TRUE,
                          
                          font_size = input$table_font_size
                          ) %>%
            scroll_box(width = '100%',
                       height = '500px')
    }
}

# Run the application   ------------------------------
shinyApp(ui = ui,
         server = server)
