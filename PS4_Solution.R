# COVID REPORT
#Problem Set- 4-----------------------------------------------------------------

# packages
  library(tidyverse)
  library(scales)
  library(lfe)
  library(modelsummary)
  library(gt)
  library(data.table)
  library(kableExtra)

# create the dataset --------------------------------------------------
## 2021-2022 deaths (BIG FILES)
  covid <-
    data.table::fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv') %>%
    filter(!is.na(fips), state != 'Puerto Rico') %>%
    select(fips, county, state, date, deaths) %>%
    group_by(fips, county, state) %>%
    summarise(deaths = max(deaths, na.rm = T) - min(deaths, na.rm = T))

## estimated mask usage from July 2020 survey
  mask <-
    read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv') %>%
    mutate(
      fips = as.integer(COUNTYFP),
      always.mask = ALWAYS, #always masking
      .keep = 'none'
    ) # for merging   

## prep CDC data from directory
  vax <-
    read_csv('cdc vax mar1.csv') %>%
    filter( # drop unknown/incomplete/questionable reports
      FIPS != 'UNK', 
      Recip_State != 'VI', 
      Completeness_pct > 0, 
      !is.na(Administered_Dose1_Recip)
    ) %>% 
    mutate(
      fips = as.integer(FIPS), 
      population = Census2019,
      vax.complete = Series_Complete_Pop_Pct, # percent vaxd
      svi.index = SVI_CTGY, # social vulnerability index
      .keep = 'none'
    ) 

## merge  
  covid <-
    left_join(covid, mask) %>%
    left_join(vax) %>%
    mutate(deaths.scaled = deaths / population * 100000) %>%
    ungroup() # scale by population
  
  rm(mask, vax)
  
  # Convert summary output into a data frame
  summary_df <- summary(covid)
  kable(summary_df, caption = "Summary Statistics of COVID-19 Data")


  # COVID deaths nationally ----------
  # Histogram of COVID-19 deaths (log scale to reduce skew)
  ggplot(covid, aes(x = (1 + deaths))) +
    geom_histogram(color = 'white', fill = 'steelblue', bins = 30) +
    scale_x_log10() +
    labs(title = "Distribution of COVID-19 Deaths per County",
         x = "Total Deaths (Log Scale)",
         y = "Number of Counties") +
    theme_minimal()
  
  ## stat summary and more
  summary(covid$deaths)
  
  # Mask usage -----------------------
  # Histogram of mask usage
  ggplot(covid, aes(x = always.mask)) +
    geom_histogram(color = 'white', fill = 'darkorange', bins = 30) +
    labs(title = "Distribution of Mask Adherence Across Counties",
         x = "Proportion of People 'Always' Wearing a Mask",
         y = "Number of Counties") +
    theme_minimal()
  
  ## helpers
  summary(covid$always.mask)
  
  # Find counties with highest and lowest mask adherence
  covid %>%
    select(always.mask, state, county) %>%
    filter(always.mask %in% c(min(always.mask, na.rm = TRUE), 
                              max(always.mask, na.rm = TRUE))) %>%
    kable(caption = "Counties with Highest and Lowest Mask Adherence") %>%
    kable_styling()
  
  # Histogram of vaccination rates
  ggplot(covid, aes(x = vax.complete)) +
    geom_histogram(color = 'white', fill = 'seagreen', bins = 30) +
    labs(title = "Distribution of COVID-19 Vaccination Rates",
         x = "Percentage of Fully Vaccinated Population",
         y = "Number of Counties") +
    theme_minimal()
  
  summary(covid$vax.complete)
  
  # Boxplot of vaccination rates by social vulnerability category
  ggplot(covid, aes(y = vax.complete, x = svi.index, fill = svi.index)) +
    geom_boxplot() +
    labs(title = "Vaccination Rates by Social Vulnerability Index",
         x = "Social Vulnerability Index (SVI)",
         y = "Vaccination Rate (%)") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Find counties with highest and lowest vaccination rates
  covid %>%
    select(vax.complete, state, county) %>%
    filter(vax.complete %in% c(min(vax.complete, na.rm = TRUE), 
                               max(vax.complete, na.rm = TRUE))) %>%
    kable(caption = "Counties with Highest and Lowest Vaccination Rates") %>%
    kable_styling()


# Impact on 2022 COVID deaths ------
## regression estimates
  mods <- 
    list(
      m1 = felm(deaths.scaled ~ always.mask + population + svi.index | state, data = covid),
      m2 = felm(deaths.scaled ~ vax.complete + population + svi.index | state, data = covid),
      m3 = felm(deaths.scaled ~ always.mask + vax.complete + svi.index | state, data = covid)
    )

## regression table
  modelsummary(
    mods, 
    gof_map = c('nobs'), 
    stars = TRUE, 
    output = 'gt'
  )


