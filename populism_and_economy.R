# Linking populism to economic performance

# Load libraries ----
library(tidyverse)
library(lubridate)
library(knitr)  #for kable function to format regression results
library(stargazer)  #for displaying multiple model results
library(janitor)
library(MatchIt)

# Load data ----
pop <- read_csv('./data/team_populism_prepared.csv',
                col_types = "ccccccdinfii") %>%
  rename_all(tolower)
 #populism data has one row per leader term

wb <- read_csv('./data/world_bank_econ_20210222.csv', 
               trim_ws = TRUE, 
               na = c('..', NA)) %>% 
  rename_all(tolower) %>% clean_names()
 #world bank data has one row per dyad of country + data_series, with columns for each year of data

vdem <- read_csv('./data/V-Dem-CY-Core-v11.csv')
 #V-Dem data has one row per dyad of country + year
 #columns include myriad indicators, which roll up through mid-level indexes into five high-level indexes

ef <- read_csv('./data/HIEF_data.csv') %>% rename_all(tolower)

# Reshape data ----

## shape econ data with one row per country + year  ----

### transform World Bank data into one row per country + series + year
econ_long <- wb %>% 
  filter(!is.na(country_code)) %>%
  pivot_longer(
    cols = starts_with('x'),
    names_to = 'year',
    names_prefix = 'x',
    values_to = 'value'
  )
econ_long$year <- str_sub(string = econ_long$year, start = 1, end = 4) %>% as.integer()
econ_long <- econ_long %>% filter(!is.na(series_code), series_code != '')  #eliminate hollow rows, which come from footnotes included in CSV

### note characteristics, manually assign variable names
econ_vars <- unique(econ_long$series_code)
unique(econ_long$series_name) # view series names, then manually assign more intelligible names
nickname <- c('cpi', 'inflation', 'gdp', 'gdp_percap', 'gdp_current', 'gdp_percap_current', 'growth', 'growth_percap', 'population', 'labor_rate', 'high_share', 'low_share')

### begin building project dataframe with one row per country + year
pe <- econ_long %>% filter(series_code == econ_vars[1]) %>% select(country_name, country_code, year)

### fill out dataframe with one column for each economic variable
for (i in 1:length(econ_vars)) {               #for each data series pulled from World Bank...
  pe[,nickname[i]] <-                        #...create a new column with manually assigned name...
  econ_long %>%                                #...and fill it with value from subset of econ_long
    filter(series_code == econ_vars[i]) %>% 
    select(value)
}
 

## fit populism data into differently shaped project dataframe ----

###standardize name used for Morales in Bolivia
pop %>% filter(iso_code == 'BOL' & year_begin > 2005) %>% select(leader)
pop$leader[pop$iso_code == 'BOL' & pop$leader == 'Morales'] <- 'Evo Morales'

### pop df uses 1899 to indicate a leader still in office.
### note ongoing tenure, then rewrite as latest year
pop$in_office <- if_else(pop$year_end == 1899, TRUE, FALSE)
pop$year_end <- if_else(pop$year_end == 1899, as.integer(max(pop$year_end)), pop$year_end)

### locate one row with impossible data: tenure ends before it begins
pop %>% mutate(time_travel = year_end - year_begin) %>% filter(time_travel < 0) %>% select(year_begin, year_end, iso_code, country, leader)
### manually change value to 2003, per external sources on Noboa's tenure
pop$year_end <- if_else(pop$year_begin == 2000 & pop$iso_code == 'ECU',
                        as.integer(2003),
                        pop$year_end) 

### recode left-right as factor variable
pop <- pop %>% mutate(ideology = fct_recode(as.factor(left_right),
                                            center = '0',
                                            left = '-1',
                                            right = '1'))

### note length of tenure and number of terms
pop <- pop %>% mutate(tenure = year_end - year_begin)

pop <- pop %>% arrange(leader, year_begin)
pop <- pop %>% group_by(leader) %>% mutate(total_terms = n()) %>% ungroup()


pop[1, 'term_number'] <- 1
for (i in 2:nrow(pop)){
  pop[i, 'term_number'] <-
    if_else(as.character(pop[i, 'leader']) == as.character(pop[(i-1), 'leader']),  #if the prior row is the same leader...
            as.integer(pop[(i-1), 'term_number']) + 1,                             #...then count an additional term
            1)
}

pop <- pop %>% arrange(leader, year_begin)
pop[1, 'cumulative_tenure'] <- as.integer(pop[1, 'tenure'])
for (i in 2:nrow(pop)){
  pop[i, 'cumulative_tenure'] <-
    if_else(as.integer(pop[i, 'term_number']) > 1,                                         #if multiple terms...
            as.integer(pop[(i-1), 'cumulative_tenure']) + as.integer(pop[(i), 'tenure']),  #...then add combined tenure
            as.integer(pop[i, 'tenure']))
}

### create empty columns for variables from pop df
pe$region <- pe$leader <- pe$party <- pe$ideology <- as.character(NA)
pe$president <- pe$term <- pe$tenure_term <- pe$tenure_aggregate <- as.integer(NA)
pe$populism_score <- as.numeric(NA)
pe$in_office <- as.logical(NA)

### fill project df with matching data from pop df
for (i in 1:nrow(pe)) {
  pe_match <- pe[i,]
  
  pop_match <- pop %>% 
    filter(iso_code == pe_match$country_code &
             year_begin <= pe_match$year &
             year_end > pe_match$year)  #choosing to assign overlapping years to incoming leader
  
  if(nrow(pop_match) == 0) next
  
  pe[i,]$region <- pop_match$region
  pe[i,]$leader <- pop_match$leader
  pe[i,]$party <- pop_match$party
  pe[i,]$ideology <- as.character(pop_match$ideology)
  pe[i,]$president <- pop_match$president
  pe[i,]$populism_score <- pop_match$average_score
  pe[i,]$in_office <- pop_match$in_office
  pe[i,]$term <- as.integer(pop_match$term_number)
  pe[i,]$tenure_term <- as.integer(pe_match$year - pop_match$year_begin)
  pe[i,]$tenure_aggregate <- as.integer(pop_match$cumulative_tenure - (pop_match$year_end - pe_match$year))
}

pe$ideology <- as.factor(pe$ideology)

## add V-Dem data to similarly shaped project dataframe  ----
vdem$year <- as.integer(vdem$year)
pe <- left_join(x = pe,
                y = vdem %>% select(country_text_id,
                                    year,
                                    v2x_polyarchy, #five high-level indexes
                                    v2x_libdem,
                                    v2x_partipdem,
                                    v2x_delibdem,
                                    v2x_egaldem,
                                    v2x_liberal, #handful of selected mid-level indexes
                                    v2xcl_rol,
                                    v2x_jucon,
                                    v2xlg_legcon,
                                    v2xdd_dd,
                                    v2xdl_delib,
                                    v2xeg_eqprotec,
                                    v2xeg_eqdr,
                                    v2x_veracc,
                                    v2x_horacc,
                                    v2x_corr,
                                    v2xdd_i_pl),
                by = c('country_code' = 'country_text_id', 'year' = 'year'))

## add ethnic fractionalization data
ef$country_name <- ef$country
pe <- left_join(x = pe,
                y = ef,
                by = c('country_name', 'year'))

pe %>% filter(!is.na(populism_score)) %>% summarize(ef_matched  = sum(!is.na(efindex)), missing = sum(is.na(efindex)))

countries <- unique(pe$country_name[!is.na(pe$populism_score)])
subset(countries, !(countries %in% unique(ef$country_name)))

ef <- ef %>% mutate(country_name = 
                      case_when(country == 'German Federal Republic' ~ 'Germany',
                                country == 'Russia' ~ 'Russian Federation',
                                country == 'Slovakia' ~ 'Slovak Republic',
                                country == 'Venezuela' ~ 'Venezuela, RB',
                                country == 'United States of America' ~ 'United States',
                                TRUE ~ country))

pe <- pe  %>% select(-efindex)
pe <- left_join(x = pe,
                y = ef,
                by = c('country_name', 'year'))

pe %>% filter(!is.na(populism_score)) %>% summarize(ef_matched = sum(!is.na(efindex)), missing = sum(is.na(efindex)))

subset(countries, !(countries %in% unique(ef$country_name)))
pe %>% filter(!is.na(populism_score) & is.na(efindex) & year < 2014)
#France, India, and years after 2013 lack an efindex 

rm(econ_long, pe_match, pop_match, wb, ef)

## lag economic data
pe$cpi_lag1 <- lead(pe$cpi, 1, along_with = pe$year) #credit leader in year Y with outcome in Y+1
pe$inflation_lag1 <- lead(pe$inflation, 1, along_with = pe$year)
pe$gdp_lag1 <- lead(pe$gdp, 1, along_with = pe$year) 
pe$gdp_percap_lag1 <- lead(pe$gdp_percap, 1, along_with = pe$year)
pe$gdp_current_lag1 <- lead(pe$gdp_current, 1, along_with = pe$year)
pe$gdp_percap_current_lag1 <- lead(pe$gdp_percap_current, 1, along_with = pe$year)
pe$growth_lag1 <- lead(pe$growth, 1, along_with = pe$year)
pe$growth_percap_lag1 <- lead(pe$growth_percap, 1, along_with = pe$year)
pe$population_lag1 <- lead(pe$population, 1, along_with = pe$year)
pe$labor_rate_lag1 <- lead(pe$labor_rate, 1, along_with = pe$year)
pe$high_share_lag1 <- lead(pe$high_share, 1, along_with = pe$year)
pe$low_share_lag1 <- lead(pe$low_share, 1, along_with = pe$year)

pe$cpi_lag2 <- lead(pe$cpi, 2, along_with = pe$year) #credit leader in year Y with outcome in Y+2
pe$inflation_lag2 <- lead(pe$inflation, 2, along_with = pe$year)
pe$gdp_lag2 <- lead(pe$gdp, 2, along_with = pe$year) 
pe$gdp_percap_lag2 <- lead(pe$gdp_percap, 2, along_with = pe$year)
pe$gdp_current_lag2 <- lead(pe$gdp_current, 2, along_with = pe$year)
pe$gdp_percap_current_lag2 <- lead(pe$gdp_percap_current, 2, along_with = pe$year)
pe$growth_lag2 <- lead(pe$growth, 2, along_with = pe$year)
pe$growth_percap_lag2 <- lead(pe$growth_percap, 2, along_with = pe$year)
pe$population_lag2 <- lead(pe$population, 2, along_with = pe$year)
pe$labor_rate_lag2 <- lead(pe$labor_rate, 2, along_with = pe$year)
pe$high_share_lag2 <- lead(pe$high_share, 2, along_with = pe$year)
pe$low_share_lag2 <- lead(pe$low_share, 2, along_with = pe$year)

pe <- pe %>%       #note change in stats from prior year
  mutate(cpi_change = cpi_lag1 - cpi,
         inflation_change = inflation_lag1 - inflation,
         gdp_change = gdp_lag1 - gdp,
         gdp_percap_change = gdp_percap_lag1 - gdp_percap,
         growth_change = growth_lag1 - growth,
         labor_rate_change = labor_rate_lag1 - labor_rate,
         high_share_change = high_share_lag1 - high_share,
         low_share_change =  - low_share)

pe$labor_rate_change_lag1 <- lead(pe$labor_rate_change, 1, along_with = pe$year)

# Analyze data ----

# descriptive analysis ----

pop %>% summarize(mean(average_score), median(average_score), sd(average_score))

pop %>% group_by(populist = average_score >= 0.70) %>% summarize(count = n())

kable(
  pe %>% group_by(Populist = if_else(populism_score >= 0.70, 'Populist', 'Not Populist')) %>% 
    summarize(Population = mean(population/1000000, na.rm = TRUE),
              Fractionalization = mean(efindex, na.rm = TRUE),
              Income = mean(gdp_percap, na.rm = TRUE),
              Inflation = mean(cpi, na.rm = TRUE),
              Labor = mean(labor_rate, na.rm = TRUE)),
  digits = 2,
  col.names = c('Populist', 'Population (millions)', 'Ethnic Fractionalization', 'GDP per capita', 'Inflation Rate', 'Labor Force Participation'))

pop %>% group_by(Populist = if_else(average_score >= 0.70, 'Populist', 'Not Populist'), ideology) %>% 
  summarize(n())

pe %>% group_by(Populist = if_else(populism_score >= 0.70, 'Populist', 'Not Populist'), ideology) %>% 
  summarize(count = n()) #note the unit here is leader-years

ideology_pop_colors <- c(left_pop = rgb(178, 34, 34, maxColorValue = 255),
                         right_pop = rgb(178, 34, 34, maxColorValue = 255),
                         left_not = rgb(70, 130, 180, maxColorValue = 255),
                         right_not = rgb(70, 130, 180, maxColorValue = 255),
                         other = 'gray50')

pe %>% filter(!is.na(populism_score)) %>% group_by(is.na(growth_lag1)) %>% summarize(n())
pe %>% filter(!is.na(populism_score)) %>% group_by(is.na(cpi_lag1)) %>% summarize(n())
pe %>% filter(!is.na(populism_score)) %>% group_by(is.na(labor_rate_change_lag1)) %>% summarize(n())
pe %>% filter(!is.na(populism_score), !is.na(growth_lag1), !is.na(cpi_lag1), !is.na(labor_rate_change_lag1)) %>% summarize(n())

summary(pe$efindex)
pe %>% filter(!is.na(populism_score)) %>% summarize(min(efindex, na.rm = TRUE), max(efindex, na.rm = TRUE), mean(efindex, na.rm = TRUE))
pe %>% filter(!is.na(populism_score), !is.na(growth_lag1), !is.na(cpi_lag1), !is.na(labor_rate_change_lag1), !is.na(efindex)) %>%
  summarize(n())

## show distribution of populism
ggplot(data = pop, aes(x = average_score))+
  geom_histogram(binwidth = .1, fill = 'firebrick')+
  theme_minimal()+
  labs(title = "Pronounced Populism is Rare",
       subtitle = '  Distribution of Populist Scores',
       x = 'Degree of Populism',
       y = 'Number of Leader-Terms')+
  geom_vline(xintercept = 0.70, linetype = 'dashed', color = 'gray60')+
  annotate('text', x = .86, y = 25, label = 'A natural break\n to dichotomize\n populism', color = 'gray60')+
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 18),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text = element_text(size = 16),
        panel.grid.minor = element_blank())

ggsave(filename = './visuals/populism_distribution.jpg',
       width = 10,
       height = 6,
       units = 'in')


##show lopsided relationship among populism, ideology, and EF
ggplot(data = (pe %>% mutate(pop_ideology = case_when(populism_score > 0.7 & ideology == 'left' ~ 'left_pop',
                                                      populism_score > 0.7 & ideology == 'right' ~ 'right_pop',
                                                      populism_score <= 0.7 & ideology == 'left' ~ 'left_not',
                                                      populism_score <= 0.7 & ideology == 'right' ~ 'right_not',
                                                      TRUE ~ 'other'))),
       aes(x = pop_ideology, y = efindex))+
  geom_violin(aes(fill = pop_ideology))+
  scale_fill_manual(values = ideology_pop_colors, guide = 'none')+
                   #labels = c('Left Nonpopulist', 'Left Populist', 'Other', 'Right Nonpopulist', 'Right Populist'))+
  theme_minimal()+
  labs(title = 'Left Populists Concentrated in Multiethnic Countries',
       subtitle = '  Distribution of Fractionalization by Ideology and Populism',
       fill = 'Political\n Leader',
       x = NULL,
       y = 'Ethnic Fractionalization Index')+
  scale_x_discrete(labels=c('Left Nonpopulist', 'Left Populist', 'Other', 'Right Nonpopulist', 'Right Populist'))+
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(color = c('steelblue', 'firebrick', 'gray50', 'steelblue', 'firebrick')),
        legend.text = element_text(size = 16),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave(filename = './visuals/populism_ideology_ef_violin.jpg',
       width = 10,
       height = 6,
       units = 'in')



# linear models ----

##nomenclature:
##m1 is straight, bivariate comparison
##m2 adds structural factors: country, year, development level, ethnic fractionalization
##m3 adds political characteristics: ideology, tenure (all variables)
##m4 uses all variables and interacts populism with tenure
##m5 uses all variables and interacts populism with ideology
##m6 uses all variables and interacts populism with ethnict fractionalization
##m7 uses all variables and interacts populism with each of ideology, tenure, ethnic fractionalization

summary.lm(lm(cpi ~ populism_score + country_code + as.factor(year) + gdp_percap, data = pe))$coefficients[2,]
summary(lm(cpi_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap, data = pe))$coefficients[2,]
summary(lm(cpi_lag2 ~ populism_score + country_code + as.factor(year) + gdp_percap, data = pe))$coefficients[2,]

summary(lm(growth_percap ~ populism_score + country_code + as.factor(year) + gdp_percap, data = pe))$coefficients[2,]
summary(lm(growth_percap_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap, data = pe))$coefficients[2,]
summary(lm(growth_percap_lag2 ~ populism_score + country_code + as.factor(year) + gdp_percap, data = pe))$coefficients[2,]

summary(lm(labor_rate ~ populism_score + country_code + as.factor(year) + gdp_percap, data = pe))$coefficients[2,]
summary(lm(labor_rate_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap, data = pe))$coefficients[2,]
summary(lm(labor_rate_lag2 ~ populism_score + country_code + as.factor(year) + gdp_percap, data = pe))$coefficients[2,]

summary(lm(cpi_change ~ populism_score + country_code + as.factor(year) + gdp_percap, data = pe))$coefficients[2,]
summary(lm(growth_change ~ populism_score + country_code + as.factor(year) + gdp_percap, data = pe))$coefficients[2,]
summary(lm(labor_rate_change_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap, data = pe))$coefficients[2,]


growthm1 <- lm(growth_percap_lag1 ~ populism_score, data = pe)
growthm2 <- lm(growth_percap_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap + efindex, data = pe)
growthm3 <- lm(growth_percap_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate, data = pe)
growthm4 <- lm(growth_percap_lag1 ~ (populism_score * tenure_aggregate) + country_code + as.factor(year) + gdp_percap + efindex + ideology, data = pe)
growthm5 <- lm(growth_percap_lag1 ~ (populism_score * ideology) + country_code + as.factor(year) + gdp_percap + efindex + tenure_aggregate, data = pe)
growthm6 <- lm(growth_percap_lag1 ~ (populism_score * efindex) + country_code + as.factor(year) + gdp_percap + tenure_aggregate + ideology, data = pe)
growthm7 <- lm(growth_percap_lag1 ~ (populism_score * tenure_aggregate) + (populism_score * ideology) + (populism_score * efindex) + country_code + as.factor(year) + gdp_percap, data = pe)

cpim1 <- lm(cpi_lag1 ~ populism_score, data = pe)
cpim2 <- lm(cpi_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap + efindex, data = pe)
cpim3 <- lm(cpi_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate, data = pe)
cpim4 <- lm(cpi_lag1 ~ (populism_score * tenure_aggregate) + country_code + as.factor(year) + gdp_percap + efindex + ideology, data = pe)
cpim5 <- lm(cpi_lag1 ~ (populism_score * ideology) + country_code + as.factor(year) + gdp_percap + efindex + tenure_aggregate, data = pe)
cpim6 <- lm(cpi_lag1 ~ (populism_score * efindex) + country_code + as.factor(year) + gdp_percap + tenure_aggregate + ideology, data = pe)
cpim7 <- lm(cpi_lag1 ~ (populism_score * tenure_aggregate) + (populism_score * ideology) + (populism_score * efindex) + country_code + as.factor(year) + gdp_percap, data = pe)

laborm1 <- lm(labor_rate_change_lag1 ~ populism_score, data = pe)
laborm2 <- lm(labor_rate_change_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap + efindex, data = pe)
laborm3 <- lm(labor_rate_change_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate, data = pe)
laborm4 <- lm(labor_rate_change_lag1 ~ (populism_score * tenure_aggregate) + country_code + as.factor(year) + gdp_percap + efindex + ideology, data = pe)
laborm5 <- lm(labor_rate_change_lag1 ~ (populism_score * ideology) + country_code + as.factor(year) + gdp_percap + efindex + tenure_aggregate, data = pe)
laborm6 <- lm(labor_rate_change_lag1 ~ (populism_score * efindex) + country_code + as.factor(year) + gdp_percap + tenure_aggregate + ideology, data = pe)
laborm7 <- lm(labor_rate_change_lag1 ~ (populism_score * tenure_aggregate) + (populism_score * ideology) + (populism_score * efindex) + country_code + as.factor(year) + gdp_percap, data = pe)

#Try growth and inflation models without Venezuala, which had fluctuating growth, then enormous inflation, and some missing data across tenure
pe_nven <- pe %>% filter(country_code != 'VEN')

growthm1a <- lm(growth_percap_lag1 ~ populism_score, data = pe_nven)
growthm2a <- lm(growth_percap_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap + efindex, data = pe_nven)
growthm3a <- lm(growth_percap_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate, data = pe_nven)
growthm4a <- lm(growth_percap_lag1 ~ (populism_score * tenure_aggregate) + country_code + as.factor(year) + gdp_percap + efindex + ideology, data = pe_nven)
growthm5a <- lm(growth_percap_lag1 ~ (populism_score * ideology) + country_code + as.factor(year) + gdp_percap + efindex + tenure_aggregate, data = pe_nven)
growthm6a <- lm(growth_percap_lag1 ~ (populism_score * efindex) + country_code + as.factor(year) + gdp_percap + tenure_aggregate + ideology, data = pe_nven)
growthm7a <- lm(growth_percap_lag1 ~ (populism_score * tenure_aggregate) + (populism_score * ideology) + (populism_score * efindex) + country_code + as.factor(year) + gdp_percap, data = pe_nven)

cpim1a <- lm(cpi_lag1 ~ populism_score, data = pe_nven)
cpim2a <- lm(cpi_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap + efindex, data = pe_nven)
cpim3a <- lm(cpi_lag1 ~ populism_score + country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate, data = pe_nven)
cpim4a <- lm(cpi_lag1 ~ (populism_score * tenure_aggregate) + country_code + as.factor(year) + gdp_percap + efindex + ideology, data = pe_nven)
cpim5a <- lm(cpi_lag1 ~ (populism_score * ideology) + country_code + as.factor(year) + gdp_percap + efindex + tenure_aggregate, data = pe_nven)
cpim6a <- lm(cpi_lag1 ~ (populism_score * efindex) + country_code + as.factor(year) + gdp_percap + tenure_aggregate + ideology, data = pe_nven)
cpim7a <- lm(cpi_lag1 ~ (populism_score * tenure_aggregate) + (populism_score * ideology) + (populism_score * efindex) + country_code + as.factor(year) + gdp_percap, data = pe_nven)


stargazer(growthm1, growthm2, growthm3, growthm4, growthm5, growthm6, growthm7, 
          title = 'Linear Regression Models for GDP Growth',
          keep = c('populism_score', 'gdp_percap', 'ideology', 'tenure_aggregate', 'efindex'),
          dep.var.labels = 'GDP per capita, lagged one year',
          model.names = TRUE,
          covariate.labels = c('Populism', 'GDP per capita',  'Ethnic Fractionalization', 'Leftist', 'Rightist', 'Populism x Tenure', 'Populism x Ethnic Fractionalization', 'Tenure', 'Populism x Leftist', 'Populism x Rightist'),
          nobs = TRUE,
          type = 'html',
          out = './models/growth_model_results.doc')

stargazer(growthm1a, growthm2a, growthm3a, growthm4a, growthm5a, growthm6a, growthm7a, 
          title = 'Linear Regression Models for GDP Growth',
          keep = c('populism_score', 'gdp_percap', 'ideology', 'tenure_aggregate', 'efindex'),
          dep.var.labels = 'GDP per capita, lagged one year',
          model.names = TRUE,
          covariate.labels = c('Populism', 'GDP per capita',  'Ethnic Fractionalization', 'Leftist', 'Rightist', 'Populism x Tenure', 'Populism x Ethnic Fractionalization', 'Tenure', 'Populism x Leftist', 'Populism x Rightist'),
          nobs = TRUE,
          type = 'html',
          out = './models/growth_model_results_drop_venezuela.doc')

stargazer(cpim1, cpim2, cpim3, cpim4, cpim5, cpim6, cpim7,
          title = 'Linear Regression Models for Inflation',
          keep = c('populism_score', 'gdp_percap', 'ideology', 'tenure_aggregate', 'efindex'),
          dep.var.labels = 'CPI, lagged one year',
          model.names = TRUE,
          covariate.labels = c('Populism', 'GDP per capita',  'Ethnic Fractionalization', 'Leftist', 'Rightist', 'Populism x Tenure', 'Populism x Ethnic Fractionalization', 'Tenure', 'Populism x Leftist', 'Populism x Rightist'),
          nobs = TRUE,
          type = 'html',
          out = './models/cpi_model_results.doc')

stargazer(cpim1a, cpim2a, cpim3a, cpim4a, cpim5a, cpim6a, cpim7a,
          title = 'Linear Regression Models for Inflation',
          keep = c('populism_score', 'gdp_percap', 'ideology', 'tenure_aggregate', 'efindex'),
          dep.var.labels = 'CPI, lagged one year',
          model.names = TRUE,
          covariate.labels = c('Populism', 'GDP per capita',  'Ethnic Fractionalization', 'Leftist', 'Rightist', 'Populism x Tenure', 'Populism x Ethnic Fractionalization', 'Tenure', 'Populism x Leftist', 'Populism x Rightist'),
          nobs = TRUE,
          type = 'html',
          out = './models/cpi_model_results_drop_venezuela.doc')

stargazer(laborm1, laborm2, laborm3, laborm4, laborm5, laborm6, laborm7,
          title = 'Linear Regression Models for Labor Force Participation',
          keep = c('populism_score', 'gdp_percap', 'ideology', 'tenure_aggregate', 'efindex'),
          dep.var.labels = 'Change in labor force participation rate, lagged one year',
          model.names = TRUE,
          covariate.labels = c('Populism', 'GDP per capita',  'Ethnic Fractionalization', 'Leftist', 'Rightist', 'Populism x Tenure', 'Populism x Ethnic Fractionalization', 'Tenure', 'Populism x Leftist', 'Populism x Rightist'),
          nobs = TRUE,
          type = 'html',
          out = './models/labor_model_results.doc')



### create dataframe to illustrate model results
nonpop <- mean(pe$populism_score[pe$populism_score < 0.75], na.rm = TRUE)
avgpop <- mean(pe$populism_score[pe$populism_score >= 0.75], na.rm = TRUE)
pe %>% filter(!is.na(growth_lag1), !is.na(cpi_lag1), !is.na(labor_rate_change_lag1), !is.na(populism_score), !is.na(efindex)) %>% summarize(mimir_year = max(year))

populism_scale = seq(from = min(pe$populism_score, na.rm = TRUE), to = max(pe$populism_score, na.rm = TRUE), by = 0.1)
tenure_scale = seq(from = min(pe$tenure_aggregate, na.rm = TRUE), to = max(pe$tenure_aggregate, na.rm = TRUE), by = 1)
mimir <- data.frame(populism_score = rep(populism_scale, times = length(tenure_scale)),
                    tenure_aggregate = rep(tenure_scale, each = length(populism_scale)),
                    country_code = as.factor('BRA'),
                    year = as.numeric(2013),
                    gdp_percap = mean(pe$gdp_percap, na.rm = TRUE),
                    ideology = as.factor('center'),
                    efindex = mean(pe$efindex, na.rm = TRUE))

mimir_tenure <- data.frame(populism_score = rep(c(nonpop, avgpop), times = length(tenure_scale)),
                           tenure_aggregate = rep(tenure_scale, each = 2),
                           country_code = as.factor('BRA'),
                           year = as.numeric(2013),
                           gdp_percap = mean(pe$gdp_percap, na.rm = TRUE),
                           ideology = as.factor('center'),
                           efindex = mean(pe$efindex, na.rm = TRUE))

 #make predictions using model that interacted tenure
mimir_tenure$growth_percap_lag1_m <- predict(growthm4, newdata = mimir_tenure)
mimir_tenure$cpi_lag1_m <- predict(cpim4, newdata = mimir_tenure)
mimir_tenure$labor_rate_change_lag1_m <- predict(laborm4, newdata = mimir_tenure)

#holding all constant, populists over tenure drive up growth...  
ggplot(data = mimir_tenure, aes(x = tenure_aggregate))+
  geom_line(aes(y = growth_percap_lag1_m, color = as.factor(populism_score)))

#...down cpi
ggplot(data = mimir_tenure, aes(x = tenure_aggregate))+
  geom_line(aes(y = cpi_lag1_m, color = as.factor(populism_score)))

#...and down labor force participation
ggplot(data = mimir_tenure, aes(x = tenure_aggregate))+
  geom_line(aes(y = labor_rate_change_lag1_m, color = as.factor(populism_score)))

##repeat to isolate effect of ideology
mimir_ideology <- data.frame(populism_score = rep(populism_scale, times = 3),
                             ideology = as.factor(rep(c('left', 'right', 'center'), each = length(populism_scale))),
                             tenure_aggregate = as.integer(mean(pe$tenure_aggregate, na.rm = TRUE)),
                             country_code = as.factor('BRA'),
                             year = as.numeric(2013),
                             gdp_percap = mean(pe$gdp_percap, na.rm = TRUE),
                             ideology = as.factor('center'),
                             efindex = mean(pe$efindex, na.rm = TRUE))
 
 #make predictions using model that interacted ideology
mimir_ideology$growth_percap_lag1_m <- predict(growthm5, newdata = mimir_ideology)
mimir_ideology$cpi_lag1_m <- predict(cpim5, newdata = mimir_ideology)
mimir_ideology$labor_rate_change_lag1_m <- predict(laborm5, newdata = mimir_ideology)

ggplot(data = mimir_ideology, aes(x = populism_score))+
  geom_line(aes(y = growth_percap_lag1_m, color = ideology))

ggplot(data = mimir_ideology, aes(x = populism_score))+
  geom_line(aes(y = cpi_lag1_m, color = ideology))

ggplot(data = mimir_ideology, aes(x = populism_score))+
  geom_line(aes(y = labor_rate_change_lag1_m, color = ideology))
 #all else equal, populism has the most economically beneficial effect on right governments

 #repeat to isolate effects of ethnic fractionalization
ef_scale = seq(from = 0, to = 1, by = 0.1)

mimir_ef <- data.frame(populism_score = rep(c(nonpop, avgpop), times = length(ef_scale)),
                       efindex = rep(ef_scale, each = length(populism_scale)),
                       tenure_aggregate = as.integer(mean(pe$tenure_aggregate, na.rm = TRUE)),
                       country_code = as.factor('BRA'),
                       year = as.numeric(2013),
                       gdp_percap = mean(pe$gdp_percap, na.rm = TRUE),
                       ideology = as.factor('center'))

#make predictions using model that interacted efindex
mimir_ef$growth_percap_lag1_m <- predict(growthm6, newdata = mimir_ef)
mimir_ef$cpi_lag1_m <- predict(cpim6, newdata = mimir_ef)
mimir_ef$labor_rate_change_lag1_m <- predict(laborm6, newdata = mimir_ef)

ggplot(data = mimir_ef, aes(x = efindex))+
  geom_line(aes(y = growth_percap_lag1_m, color = as.factor(populism_score)))

ggplot(data = mimir_ef, aes(x = efindex))+
  geom_line(aes(y = cpi_lag1_m, color = as.factor(populism_score)))

ggplot(data = mimir_ef, aes(x = efindex))+
  geom_line(aes(y = labor_rate_change_lag1_m, color = as.factor(populism_score)))
 #populists slightly better at growth and cpi among higher efindex, but worse at labor participation

#repeat using model 7 (with three interactions), adding confidence interval to growth
mimir_ef$growth_percap_lag1_m <- predict(growthm7, newdata = mimir_ef, interval = 'confidence')[,1]
mimir_ef$growth_percap_lag1_mlow <- predict(growthm7, newdata = mimir_ef, interval = 'confidence')[,2]
mimir_ef$growth_percap_lag1_mupp <- predict(growthm7, newdata = mimir_ef, interval = 'confidence')[,3]
mimir_ef$cpi_lag1_m <- predict(cpim7, newdata = mimir_ef)
mimir_ef$labor_rate_change_lag1_m <- predict(laborm7, newdata = mimir_ef)

ggplot(data = mimir_ef, aes(x = efindex))+
  geom_line(aes(y = growth_percap_lag1_m/100, color = as.factor(populism_score)), size = 2)+
  scale_color_manual(name = 'Populism',
                     values = c('steelblue', 'firebrick'),
                     labels = c('Nonpopulist', 'Populist'))+
  geom_line(data = (mimir_ef %>% filter(populism_score >= 0.7)),
            aes(y = growth_percap_lag1_mlow/100), color = 'firebrick', alpha = 0.6, linetype = 'dashed')+
  geom_line(data = (mimir_ef %>% filter(populism_score >= 0.7)),
            aes(y = growth_percap_lag1_mupp/100), color = 'firebrick', alpha = 0.6, linetype = 'dashed')+
  coord_cartesian(xlim = c(min(pe$efindex, na.rm = TRUE), max(pe$efindex, na.rm = TRUE)))+
  theme_minimal()+
  labs(title = 'Populists Better Amid Heterogeneity',
       subtitle = '  GDP Growth Predicted by Populism and Fractionalization',
       x = 'Ethnic Fractionalization Index',
       y = 'GDP Per Capita Growth Rate')+
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_y_continuous(labels = scales::percent)


ggsave(filename = './visuals/growth_ef_prediction.jpg',
       width = 10,
       height = 6,
       units = 'in')


ggplot(data = mimir_ef, aes(x = efindex))+
  geom_line(aes(y = cpi_lag1_m, color = as.factor(populism_score)))

ggplot(data = mimir_ef, aes(x = efindex))+
  geom_line(aes(y = labor_rate_change_lag1_m, color = as.factor(populism_score)))

#repeat yet again exaggerating populism differnce to 0 and 2 rather than mean nonpopulist and mean populist
mimir_ef2 <- mimir_ef
mimir_ef2$populism_score <- if_else(mimir_ef2$populism_score < 0.7, 0, 2)
mimir_ef2$growth_percap_lag1_m <- predict(growthm7, newdata = mimir_ef2, interval = 'confidence')[,1]
mimir_ef2$growth_percap_lag1_mlow <- predict(growthm7, newdata = mimir_ef2, interval = 'confidence')[,2]
mimir_ef2$growth_percap_lag1_mupp <- predict(growthm7, newdata = mimir_ef2, interval = 'confidence')[,3]


ggplot(data = mimir_ef2, aes(x = efindex))+
  geom_line(aes(y = growth_percap_lag1_m/100, color = as.factor(populism_score)), size = 2)+
  scale_color_manual(name = 'Populism',
                     values = c('steelblue', 'firebrick'),
                     labels = c('Nonpopulist', 'Populist'))+
  geom_line(data = (mimir_ef2 %>% filter(populism_score >= 0.7)),
            aes(y = growth_percap_lag1_mlow/100), color = 'firebrick', alpha = 0.6, linetype = 'dashed')+
  geom_line(data = (mimir_ef2 %>% filter(populism_score >= 0.7)),
            aes(y = growth_percap_lag1_mupp/100), color = 'firebrick', alpha = 0.6, linetype = 'dashed')+
  coord_cartesian(xlim = c(min(pe$efindex, na.rm = TRUE), max(pe$efindex, na.rm = TRUE)))+
  theme_minimal()+
  labs(title = 'Populists Better Amid Heterogeneity',
       subtitle = '  GDP Growth Predicted by Populism and Fractionalization',
       x = 'Ethnic Fractionalization Index',
       y = 'GDP Per Capita Growth Rate')+
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_y_continuous(labels = scales::percent)

 #end exaggeration#


#relationship between populism and ef seems driven by a few positive examples at high ef
pe %>% filter(!is.na(populism_score)) %>%
  ggplot(aes(x = efindex, y = growth_percap_lag1))+
  geom_point(aes(color = populism_score > 0.75))+
  geom_smooth(aes(color = populism_score > 0.75))

#. . .especially Peru, somewhat Ecuador and less Bolivia
pe %>% filter(populism_score > 0.75, efindex > 0.55) %>% select(country_name, year, growth_percap_lag1, populism_score) %>% arrange(desc(growth_percap_lag1))







# difference in difference for democratic decline ----

summary(pe$v2x_polyarchy)
summary(pe$v2x_libdem)
summary(pe$v2x_partipdem)
summary(pe$v2x_egaldem)
summary(pe$v2x_delibdem)

sd(pe$v2x_polyarchy, na.rm = TRUE)
sd(pe$v2x_libdem, na.rm = TRUE)
sd(pe$v2x_partipdem, na.rm = TRUE)
sd(pe$v2x_egaldem, na.rm = TRUE)
sd(pe$v2x_delibdem, na.rm = TRUE)

pe$vdem_agg <- pe$v2x_polyarchy + pe$v2x_libdem + pe$v2x_partipdem + pe$v2x_egaldem + pe$v2x_delibdem
summary(pe$vdem_agg)
sd(pe$vdem_agg, na.rm = TRUE)

#define democratic decline as drop of 1 standard deviation (of full dataset) from own country's prior max
dd <- pe %>% filter(year > 1991 & !is.na(vdem_agg))
 #note change in vdem score from prior country max
dd$vdem_change <- as.numeric(NA)
for (i in 1:nrow(dd)){
  working_country <- dd$country_code[i]
  working_year <- dd$year[i]
  prior_vdem_max <- max(dd$vdem_agg[dd$country_code == working_country & dd$year < working_year])
  prior_vdem_max <- if_else(prior_vdem_max == -Inf, 0, prior_vdem_max)
  dd$vdem_change[i] <- dd$vdem_agg[i] - prior_vdem_max
}

summary(dd$vdem_change)
vdem_change_sd <- sd(dd$vdem_change, na.rm = TRUE)

 #call in a drop if it is the first year that vdem score is more than 1 sd below max
dd$dem_drop <- if_else(dd$vdem_change < (vdem_change_sd * -1),
                       TRUE,
                       FALSE)
for (i in 1:nrow(dd)){
  working_country <- dd$country_code[i]
  working_year <- dd$year[i]
  prior_dem_drop <- if_else(max(dd$dem_drop[dd$country_code == working_country & dd$year < working_year]) == 1,
                            TRUE,
                            FALSE)
  if (prior_dem_drop == FALSE){
    next
  }
  dd$dem_drop[i] <- FALSE
}

sum(dd$dem_drop, na.rm = TRUE)

 #keep only countries that ever declined in democracy
dd_keep_list <- unique(dd$country_code[dd$dem_drop == TRUE])
dd <- dd %>% filter(country_code %in% dd_keep_list)

 #mark year as time relative to dem drop
dd$dd_rel_year <- as.integer(NA)
for (i in 1:nrow(dd)){
  working_country <- dd$country_code[i]
  working_year <- dd$year[i]
  dem_drop_year <- dd$year[dd$country_code == working_country & dd$dem_drop == TRUE]
  dd$dd_rel_year[i] <- dd$year[i] - dem_drop_year
}

#keep only countries coded for populism at time of democratic decline
dd_drop_list <- unique(dd$country_code[dd$dem_drop == TRUE & is.na(dd$populism_score)])
dd <- dd %>% filter(!country_code %in% dd_drop_list)


dd %>% filter(dd_rel_year == 0) %>% select(country_name, year, populism_score)

dd <- dd %>% mutate(drop_pop = case_when(dd_rel_year < 0 & populism_score >= 0.75 ~ 'pre_pop',
                                         dd_rel_year < 0 & populism_score < 0.75 ~ 'pre_not',
                                         dd_rel_year > 0 & populism_score >= 0.75 ~ 'post_pop',
                                         dd_rel_year > 0 & populism_score < 0.75 ~ 'post_not'))
dd$drop_pop <- as.factor(dd$drop_pop)
summary(dd$drop_pop)
dd <- dd %>% filter(!is.na(drop_pop))
length(unique(dd$country_name))
unique(dd$country_name)
dd %>% filter(dd_rel_year == -1) %>% group_by(populist) %>% summarize(n())

drop_pop_colors <- c(pre_pop = rgb(178, 34, 34, alpha = 140, maxColorValue = 255),
                     post_pop = rgb(178, 34, 34, maxColorValue = 255),
                     pre_not = rgb(70, 130, 180, alpha = 140, maxColorValue = 255),
                     post_not = rgb(70, 130, 180, maxColorValue = 255))

ggplot(data = dd, aes(x = dd_rel_year, y = growth_percap/100))+
  geom_point(aes(color = drop_pop))+
  geom_smooth(aes(color = drop_pop), method = 'lm', se = FALSE, size = 2)+
  scale_color_manual(values = drop_pop_colors,
                     labels = c('Nonpopulist\n (prior)', 'Populist\n (prior)', 'Nonpopulist\n (after)', 'Populist\n (after)'))+
  theme_minimal()+
  geom_vline(xintercept = 0, linetype = 'dashed', size = 1.5)+
  #geom_text(label = 'Democratic Decline', x = 5.8, y = .125, size = 4.8)+
  labs(title = 'Populists Frontload Damage',
       subtitle = '  GDP Change After Democratic Decline',
       color = 'Populism',
       x = 'Years Relative to First Democratic Decline',
       y = 'GDP Per Capita Growth Rate')+
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_y_continuous(labels = scales::percent)

ggsave(filename = './visuals/dif_in_dif.jpg',
       width = 10,
       height = 6,
       units = 'in')


ggplot(data = dd, aes(x = dd_rel_year, y = growth_percap/100))+
  geom_point(aes(color = drop_pop))+
  geom_smooth(aes(color = drop_pop, fill = drop_pop), method = 'lm', size = 2)+
  scale_color_manual(values = drop_pop_colors,
                     labels = c('Nonpopulist\n (prior)', 'Populist\n (prior)', 'Nonpopulist\n (after)', 'Populist\n (after)'))+
  scale_fill_manual(values = drop_pop_colors, guide = 'none')+
  theme_minimal()+
  geom_vline(xintercept = 0, linetype = 'dashed', size = 1.5)+
  #geom_text(label = 'Democratic Decline', x = 5.8, y = .125, size = 4.8)+
  labs(title = 'Populists Frontload Damage',
       subtitle = '  GDP Change After Democratic Decline',
       color = 'Populism',
       x = 'Years Relative to First Democratic Decline',
       y = 'GDP Per Capita Growth Rate')+
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.title.x = element_text(margin = margin(t = 11)),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_y_continuous(labels = scales::percent)


ggsave(filename = './visuals/dif_in_dif_se.jpg',
       width = 10,
       height = 6,
       units = 'in')

#matching analysis ----
pe$populist <- if_else(pe$populism_score >= 0.75, TRUE, FALSE)

#match data with "treatment" being populism
mout_nn_glm <- matchit(populist ~
                        country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate,
                      data = (pe %>% filter(!is.na(growth_percap_lag1),
                                            !is.na(populism_score),
                                            !is.na(gdp_percap),
                                            !is.na(efindex),
                                            !is.na(ideology),
                                            !is.na(tenure_aggregate))),
                      method = 'nearest', distance = 'glm')

summary(mout_nn_glm, un = FALSE)
plot(mout_nn_glm, type = "jitter", interactive = FALSE)

jpeg(file = './visuals/match_propensity_scores_nn.jpg')
plot(mout_nn_glm, type = "jitter", interactive = FALSE)
dev.off()

mout_nn_logit <- matchit(populist ~
                         country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate,
                       data = (pe %>% filter(!is.na(growth_percap_lag1),
                                             !is.na(populism_score),
                                             !is.na(gdp_percap),
                                             !is.na(efindex),
                                             !is.na(ideology),
                                             !is.na(tenure_aggregate))),
                       method = 'nearest', distance = 'glm', link = 'logit')

summary(mout_nn_logit, un = FALSE)
plot(mout_nn_logit, type = "jitter", interactive = FALSE)

mout_f_glm <- matchit(populist ~
                         country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate,
                       data = (pe %>% filter(!is.na(growth_percap_lag1),
                                             !is.na(populism_score),
                                             !is.na(gdp_percap),
                                             !is.na(efindex),
                                             !is.na(ideology),
                                             !is.na(tenure_aggregate))),
                       method = 'full', distance = 'glm')

summary(mout_f_glm, un = FALSE)
plot(mout_f_glm, type = "jitter", interactive = FALSE)

jpeg(file = './visuals/match_propensity_scores_full.jpg')
plot(mout_f_glm, type = "jitter", interactive = FALSE)
dev.off()


#linear regression using matches
pe_near_matched <- match.data(mout_nn_glm)

growthnm1 <- lm(growth_percap_lag1 ~ populist, data = pe_near_matched, weights = weights)
growthnm2 <- lm(growth_percap_lag1 ~ populist + country_code + as.factor(year) + gdp_percap + efindex, data = pe_near_matched, weights = weights)
growthnm3 <- lm(growth_percap_lag1 ~ populist + country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate, data = pe_near_matched, weights = weights)
growthnm4 <- lm(growth_percap_lag1 ~ (populist * tenure_aggregate) + country_code + as.factor(year) + gdp_percap + efindex + ideology, data = pe_near_matched, weights = weights)
growthnm5 <- lm(growth_percap_lag1 ~ (populist * ideology) + country_code + as.factor(year) + gdp_percap + efindex + tenure_aggregate, data = pe_near_matched, weights = weights)
growthnm6 <- lm(growth_percap_lag1 ~ (populist * efindex) + country_code + as.factor(year) + gdp_percap + tenure_aggregate + ideology, data = pe_near_matched, weights = weights)
growthnm7 <- lm(growth_percap_lag1 ~ (populist * tenure_aggregate) + (populist * ideology) + (populist * efindex) + country_code + as.factor(year) + gdp_percap, data = pe_near_matched, weights = weights)

stargazer(growthnm1, growthnm2, growthnm3, growthnm4, growthnm5, growthnm6, growthnm7, 
          title = 'Linear Regression Models for GDP Growth',
          keep = c('populist', 'gdp_percap', 'ideology', 'tenure_aggregate', 'efindex'),
          dep.var.labels = 'GDP per capita, lagged one year',
          model.names = TRUE,
          covariate.labels = c('Populism', 'GDP per capita',  'Ethnic Fractionalization', 'Leftist', 'Rightist', 'Populism x Tenure', 'Populism x Ethnic Fractionalization', 'Tenure', 'Populism x Leftist', 'Populism x Rightist'),
          nobs = TRUE,
          type = 'html',
          out = './models/growth_near_matched_results.doc')

pe_full_matched <- match.data(mout_f_glm)

growthfm1 <- lm(growth_percap_lag1 ~ populist, data = pe_full_matched, weights = weights)
growthfm2 <- lm(growth_percap_lag1 ~ populist + country_code + as.factor(year) + gdp_percap + efindex, data = pe_full_matched, weights = weights)
growthfm3 <- lm(growth_percap_lag1 ~ populist + country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate, data = pe_full_matched, weights = weights)
growthfm4 <- lm(growth_percap_lag1 ~ (populist * tenure_aggregate) + country_code + as.factor(year) + gdp_percap + efindex + ideology, data = pe_full_matched, weights = weights)
growthfm5 <- lm(growth_percap_lag1 ~ (populist * ideology) + country_code + as.factor(year) + gdp_percap + efindex + tenure_aggregate, data = pe_full_matched, weights = weights)
growthfm6 <- lm(growth_percap_lag1 ~ (populist * efindex) + country_code + as.factor(year) + gdp_percap + tenure_aggregate + ideology, data = pe_full_matched, weights = weights)
growthfm7 <- lm(growth_percap_lag1 ~ (populist * tenure_aggregate) + (populist * ideology) + (populist * efindex) + country_code + as.factor(year) + gdp_percap, data = pe_full_matched, weights = weights)

stargazer(growthfm1, growthfm2, growthfm3, growthfm4, growthfm5, growthfm6, growthfm7, 
          title = 'Linear Regression Models for GDP Growth',
          keep = c('populist', 'gdp_percap', 'ideology', 'tenure_aggregate', 'efindex'),
          dep.var.labels = 'GDP per capita, lagged one year',
          model.names = TRUE,
          covariate.labels = c('Populism', 'GDP per capita',  'Ethnic Fractionalization', 'Leftist', 'Rightist', 'Populism x Tenure', 'Populism x Ethnic Fractionalization', 'Tenure', 'Populism x Leftist', 'Populism x Rightist'),
          nobs = TRUE,
          type = 'html',
          out = './models/growth_full_matched_results.doc')

 #matching full rests on handful of control cases
 #populism too predictable from control variables, no matching available at same propensity scores

summary(lm(populist ~ country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate, data = pe))

#repeat matching, blinding year and country
mout_nn_glm_blinded <- matchit(populist ~
                                 gdp_percap + efindex + ideology + tenure_aggregate,
                               data = (pe %>% filter(!is.na(growth_percap_lag1),
                                                     !is.na(populist),
                                             !is.na(gdp_percap),
                                             !is.na(efindex),
                                             !is.na(ideology),
                                             !is.na(tenure_aggregate))),
                               method = 'nearest', distance = 'glm')

summary(mout_nn_glm_blinded, un = FALSE)
plot(mout_nn_glm_blinded, type = "jitter", interactive = FALSE)

jpeg(file = './visuals/match_propensity_scores.jpg')
plot(mout_nn_glm_blinded, type = "jitter", interactive = FALSE)
dev.off()

pe_blind_matched <- match.data(mout_nn_glm_blinded)

pe_blind_matched %>% group_by(populist) %>% summarize(mean(growth_percap_lag1), sd(growth_percap_lag1), 
                                                      mean(cpi_lag1, na.rm = TRUE), sd(cpi_lag1, na.rm = TRUE),
                                                      mean(labor_rate_change_lag1), sd(labor_rate_change_lag1))

growthbm1 <- lm(growth_percap_lag1 ~ populist, data = pe_blind_matched, weights = weights)
growthbm2 <- lm(growth_percap_lag1 ~ populist + country_code + as.factor(year) + gdp_percap + efindex, data = pe_blind_matched, weights = weights)
growthbm3 <- lm(growth_percap_lag1 ~ populist + country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate, data = pe_blind_matched, weights = weights)
growthbm4 <- lm(growth_percap_lag1 ~ (populist * tenure_aggregate) + country_code + as.factor(year) + gdp_percap + efindex + ideology, data = pe_blind_matched, weights = weights)
growthbm5 <- lm(growth_percap_lag1 ~ (populist * ideology) + country_code + as.factor(year) + gdp_percap + efindex + tenure_aggregate, data = pe_blind_matched, weights = weights)
growthbm6 <- lm(growth_percap_lag1 ~ (populist * efindex) + country_code + as.factor(year) + gdp_percap + tenure_aggregate + ideology, data = pe_blind_matched, weights = weights)
growthbm7 <- lm(growth_percap_lag1 ~ (populist * tenure_aggregate) + (populist * ideology) + (populist * efindex) + country_code + as.factor(year) + gdp_percap, data = pe_blind_matched, weights = weights)

stargazer(growthbm1, growthbm2, growthbm3, growthbm4, growthbm5, growthbm6, growthbm7, 
          title = 'Linear Regression Models for GDP Growth, Using Matches',
          keep = c('populist', 'gdp_percap', 'ideology', 'tenure_aggregate', 'efindex'),
          dep.var.labels = 'GDP per capita, lagged one year',
          model.names = TRUE,
          covariate.labels = c('Populism', 'GDP per capita',  'Ethnic Fractionalization', 'Leftist', 'Rightist', 'Populism x Tenure', 'Populism x Ethnic Fractionalization', 'Tenure', 'Populism x Leftist', 'Populism x Rightist'),
          nobs = TRUE,
          type = 'html',
          out = './models/growth_blind_matched_results.doc')

cpibm1 <- lm(cpi_lag1 ~ populist, data = pe_blind_matched, weights = weights)
cpibm2 <- lm(cpi_lag1 ~ populist + country_code + as.factor(year) + gdp_percap + efindex, data = pe_blind_matched, weights = weights)
cpibm3 <- lm(cpi_lag1 ~ populist + country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate, data = pe_blind_matched, weights = weights)
cpibm4 <- lm(cpi_lag1 ~ (populist * tenure_aggregate) + country_code + as.factor(year) + gdp_percap + efindex + ideology, data = pe_blind_matched, weights = weights)
cpibm5 <- lm(cpi_lag1 ~ (populist * ideology) + country_code + as.factor(year) + gdp_percap + efindex + tenure_aggregate, data = pe_blind_matched, weights = weights)
cpibm6 <- lm(cpi_lag1 ~ (populist * efindex) + country_code + as.factor(year) + gdp_percap + tenure_aggregate + ideology, data = pe_blind_matched, weights = weights)
cpibm7 <- lm(cpi_lag1 ~ (populist * tenure_aggregate) + (populist * ideology) + (populist * efindex) + country_code + as.factor(year) + gdp_percap, data = pe_blind_matched, weights = weights)

stargazer(cpibm1, cpibm2, cpibm3, cpibm4, cpibm5, cpibm6, cpibm7, 
          title = 'Linear Regression Models for Inflation, Using Matches',
          keep = c('populist', 'gdp_percap', 'ideology', 'tenure_aggregate', 'efindex'),
          dep.var.labels = 'GDP per capita, lagged one year',
          model.names = TRUE,
          covariate.labels = c('Populism', 'GDP per capita',  'Ethnic Fractionalization', 'Leftist', 'Rightist', 'Populism x Tenure', 'Populism x Ethnic Fractionalization', 'Tenure', 'Populism x Leftist', 'Populism x Rightist'),
          nobs = TRUE,
          type = 'html',
          out = './models/inflation_blind_matched_results.doc')


cpibm1a <- lm(cpi_lag1 ~ populist, data = (pe_blind_matched %>% filter(country_code != 'VEN')), weights = weights)
cpibm2a <- lm(cpi_lag1 ~ populist + country_code + as.factor(year) + gdp_percap + efindex, data = (pe_blind_matched %>% filter(country_code != 'VEN')), weights = weights)
cpibm3a <- lm(cpi_lag1 ~ populist + country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate, data = (pe_blind_matched %>% filter(country_code != 'VEN')), weights = weights)
cpibm4a <- lm(cpi_lag1 ~ (populist * tenure_aggregate) + country_code + as.factor(year) + gdp_percap + efindex + ideology, data = (pe_blind_matched %>% filter(country_code != 'VEN')), weights = weights)
cpibm5a <- lm(cpi_lag1 ~ (populist * ideology) + country_code + as.factor(year) + gdp_percap + efindex + tenure_aggregate, data = (pe_blind_matched %>% filter(country_code != 'VEN')), weights = weights)
cpibm6a <- lm(cpi_lag1 ~ (populist * efindex) + country_code + as.factor(year) + gdp_percap + tenure_aggregate + ideology, data = (pe_blind_matched %>% filter(country_code != 'VEN')), weights = weights)
cpibm7a <- lm(cpi_lag1 ~ (populist * tenure_aggregate) + (populist * ideology) + (populist * efindex) + country_code + as.factor(year) + gdp_percap, data = (pe_blind_matched %>% filter(country_code != 'VEN')), weights = weights)

stargazer(cpibm1a, cpibm2a, cpibm3a, cpibm4a, cpibm5a, cpibm6a, cpibm7a, 
          title = 'Linear Regression Models for Inflation, Using Matches, Without Venezuela',
          keep = c('populist', 'gdp_percap', 'ideology', 'tenure_aggregate', 'efindex'),
          dep.var.labels = 'GDP per capita, lagged one year',
          model.names = TRUE,
          covariate.labels = c('Populism', 'GDP per capita',  'Ethnic Fractionalization', 'Leftist', 'Rightist', 'Populism x Tenure', 'Populism x Ethnic Fractionalization', 'Tenure', 'Populism x Leftist', 'Populism x Rightist'),
          nobs = TRUE,
          type = 'html',
          out = './models/inflation_blind_matched_results_drop_venezuela.doc')

laborbm1 <- lm(labor_rate_change_lag1 ~ populist, data = pe_blind_matched, weights = weights)
laborbm2 <- lm(labor_rate_change_lag1 ~ populist + country_code + as.factor(year) + gdp_percap + efindex, data = pe_blind_matched, weights = weights)
laborbm3 <- lm(labor_rate_change_lag1 ~ populist + country_code + as.factor(year) + gdp_percap + efindex + ideology + tenure_aggregate, data = pe_blind_matched, weights = weights)
laborbm4 <- lm(labor_rate_change_lag1 ~ (populist * tenure_aggregate) + country_code + as.factor(year) + gdp_percap + efindex + ideology, data = pe_blind_matched, weights = weights)
laborbm5 <- lm(labor_rate_change_lag1 ~ (populist * ideology) + country_code + as.factor(year) + gdp_percap + efindex + tenure_aggregate, data = pe_blind_matched, weights = weights)
laborbm6 <- lm(labor_rate_change_lag1 ~ (populist * efindex) + country_code + as.factor(year) + gdp_percap + tenure_aggregate + ideology, data = pe_blind_matched, weights = weights)
laborbm7 <- lm(labor_rate_change_lag1 ~ (populist * tenure_aggregate) + (populist * ideology) + (populist * efindex) + country_code + as.factor(year) + gdp_percap, data = pe_blind_matched, weights = weights)

stargazer(laborbm1, laborbm2, laborbm3, laborbm4, laborbm5, laborbm6, laborbm7, 
          title = 'Linear Regression Models for Labor Participation, Using Matches',
          keep = c('populist', 'gdp_percap', 'ideology', 'tenure_aggregate', 'efindex'),
          dep.var.labels = 'GDP per capita, lagged one year',
          model.names = TRUE,
          covariate.labels = c('Populism', 'GDP per capita',  'Ethnic Fractionalization', 'Leftist', 'Rightist', 'Populism x Tenure', 'Populism x Ethnic Fractionalization', 'Tenure', 'Populism x Leftist', 'Populism x Rightist'),
          nobs = TRUE,
          type = 'html',
          out = './models/labor_blind_matched_results.doc')






#extra ----

## view any one country over time
ggplot(data = (pe %>% filter(country_code == 'VEN')), aes(x = year))+
  geom_line(aes(y = growth_percap), color = 'steelblue')+
  geom_line(aes(y = populism_score * 5), color = 'firebrick')+
  geom_line(aes(y = vdem_agg * 2), color = 'forestgreen')
  #geom_line(aes(y = cpi), color = 'firebrick')


##visualize relationships as scatterplots

ggplot(data = pe, aes(x = populism_score, y = growth_percap_lag1))+
  geom_point()+
  geom_smooth(method = 'loess')+
  scale_y_continuous(limits = c(-10,20))

## note outliers in Latin America's commodities boom
pe %>% filter(growth_percap_lag1 > 5, populism_score > 1.5) %>% select(country_name, year, populism_score, cpi_lag1, growth_lag1, tenure_aggregate)

ggplot(data = pe, aes(x = populism_score, y = cpi_lag1))+
  geom_point()+
  geom_smooth(method = 'loess')+
  scale_y_continuous(limits = c(-2,50))

ggplot(data = pe, aes(x = populism_score, y = labor_rate_lag1))+
  geom_point()+
  geom_smooth(method = 'loess')

ggplot(data = pe, aes(x = efindex, y = populism_score))+
  geom_point()+
  geom_smooth(method = 'loess')

ggplot(data = pe, aes(x = efindex, y = growth_percap))+
  geom_point(alpha = 0.1)+
  geom_smooth(method = 'loess')+
  coord_cartesian(ylim = c(-5,10))

ggplot(data = pe, aes(x = efindex))+
  geom_density(aes(color = populism_score > 0.7), bw = 0.05)

ggplot(data = pe, aes(x = (populism_score > 0.7), y = efindex))+
  geom_violin()

ggplot(data = (pe %>% mutate(pop_ideology = case_when(populism_score > 0.7 & ideology == 'left' ~ 'left_populist',
                                                      populism_score > 0.7 & ideology == 'right' ~ 'right_populist',
                                                      populism_score <= 0.7 & ideology == 'left' ~ 'left_nonpopulist',
                                                      populism_score <= 0.7 & ideology == 'right' ~ 'right_nonpopulist'))),
       aes(x = pop_ideology, y = efindex))+
  geom_violin()

pe %>% filter(populism_score > 0.7 & ideology == 'left') %>% select(leader) %>% unique()

ggplot(data = (pe %>% filter(!is.na(populism_score))),
               aes(x = efindex, y = growth_percap_lag1, color = populism_score > 0.7))+
  geom_point()+
  geom_smooth(method = 'loess')
  scale_y_continuous(limits = c(-10,20))

ggplot(data = pe, aes(x = tenure_aggregate, y = growth_percap_lag1, color = populism_score > 0.75))+
  geom_point()+
  geom_smooth(method = 'loess')+
  scale_y_continuous(limits = c(-10,20))

ggplot(data = pe, aes(x = tenure_aggregate, y = cpi_lag1, color = populism_score > 0.75))+
  geom_point()+
  geom_smooth(method = 'loess')+
  scale_y_continuous(limits = c(-2,50))

ggplot(data = pe, aes(x = tenure_aggregate, y = labor_rate_lag1, color = populism_score > 0.75))+
  geom_point()+
  geom_smooth(method = 'loess')
  #striking difference in labor rate over tenure
  #check change in labor rate
  
ggplot(data = (pe %>% filter(ideology %in% c('left', 'right'))),
               aes(x = tenure_aggregate, y = growth_percap_lag1, color = populism_score > 0.75))+
  geom_point()+
  geom_smooth(method = 'loess')+
  scale_y_continuous(limits = c(-10,20))+
  facet_wrap(~ ideology)

ggplot(data = (pe %>% filter(ideology %in% c('left', 'right'))),
       aes(x = tenure_aggregate, y = cpi_lag1, color = populism_score > 0.75))+
  geom_point()+
  geom_smooth(method = 'loess')+
  scale_y_continuous(limits = c(-2,50))+
  facet_wrap(~ideology)
  
ggplot(data = (pe %>% filter(ideology %in% c('left', 'right'))),
       aes(x = tenure_aggregate, y = labor_rate_lag1, color = populism_score > 0.75))+
    geom_point()+
    geom_smooth(method = 'loess')+
  facet_wrap(~ideology)
  
  
#######watch out for outliers, as how Venezuela warps cpi results

cpi_all <-
  pe %>%
  filter(populism_score > 0.75) %>%
  group_by(tenure_aggregate) %>%
  mutate(cpi_avg = mean(cpi, na.rm = TRUE), n = n()) %>%
  ungroup() %>%
  ggplot(aes(x = tenure_aggregate, y = cpi_avg))+
  geom_line(aes(size = n))+
  theme_minimal()+
  theme(legend.position = 'none')+
  labs(x = 'Tenure', y = 'Inflation')+
  theme(panel.grid.minor = element_blank())

cpi_split <-
  pe %>%
  filter(populism_score > 0.75) %>%
  group_by(tenure_aggregate, country_code == 'VEN') %>%
  mutate(cpi_avg = mean(cpi, na.rm = TRUE), n = n()) %>%
  ungroup() %>%
  ggplot(aes(x = tenure_aggregate, y = cpi_avg))+
  geom_line(aes(size = n, color = country_code == 'VEN'))+
  theme_minimal()+
  theme(legend.position = 'none')+
  labs(x = 'Tenure', y = 'Inflation')+
  theme(panel.grid.minor = element_blank())

ggpubr::ggarrange(cpi_all, cpi_split)

ggsave(filename = './visuals/outlier_inflation.jpg',
       width = 10,
       height = 6,
       units = 'in')


pe %>%
  group_by(populism_score > 0.75, tenure_aggregate) %>%
  mutate(gdp_growth = mean(growth_percap, na.rm = TRUE), n = n()) %>%
  ungroup() %>%
  ggplot(aes(x = tenure_aggregate, y = gdp_growth))+
  geom_line(aes(color = populism_score > 0.75))+
  scale_x_continuous(limits = c(0,9))+
  theme_minimal()


pe %>% 
  filter(!is.na(populism_score), country_code != 'VEN') %>% 
  group_by(populism_score > 0.75) %>% 
  summarise(mean(cpi, na.rm = TRUE), mean(growth_percap, na.rm = TRUE))

