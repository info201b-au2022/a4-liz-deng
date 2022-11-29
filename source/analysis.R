
library("tidyverse")
library("ggplot2") 
library("dplyr")
library("mapproj")

# The functions might be useful for A4
# source("../source/a4-helpers.R")

raw_data <- read.csv("../source/incarceration_trends.csv",
                stringsAsFactors = FALSE)

# View(raw_data)


options(scipen = 100)

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Variable 1: Washington state total population increase compared to jail and
# prison population increase. 

wa_only <- filter(raw_data, state == "WA") %>%
  select(year, state, county_name, total_pop_15to64, total_jail_pop,
         total_prison_pop) %>%
  drop_na()

# View(wa_only) 

year_95 <- wa_only %>%
  filter(year == 1995) %>%
  rename("totalpop_95" = "total_pop_15to64",
         "jailpop_95" = "total_jail_pop",
         "prisonpop_95" = "total_prison_pop") %>%
  select(-year, -state)

# View(year_95)

year_16 <- wa_only %>%
  filter(year == 2016) %>%
  rename("totalpop_16" = "total_pop_15to64",
         "jailpop_16" = "total_jail_pop",
         "prisonpop_16" = "total_prison_pop") %>%
  select(-year, -state)

wa_9516 <- left_join(year_95, year_16, by = 'county_name') %>%
  drop_na()

wapop_95 <- sum(wa_9516$totalpop_95)
wapop_16 <-  sum(wa_9516$totalpop_16)
jailpop_95 <- sum(wa_9516$jailpop_95)
jailpop_16 <- sum(wa_9516$jailpop_16)
prisonpop_95 <- sum(wa_9516$prisonpop_95)
prisonpop_16 <- sum(wa_9516$prisonpop_16)


wapop_change <- round(((wapop_16 - wapop_95) / wapop_95) * 100, digits = 2)
jailpop_change <- round(((jailpop_16 - jailpop_95) / jailpop_95) * 100,
                        digits = 2)
prisonpop_change <- round(((prisonpop_16 - prisonpop_95) / prisonpop_95) * 100,
                          digits = 2)


# Variables 2-4:  Proportions of white, black, and Latino individuals in the
# whole of the state populations and additionally in the prison populations.

recent_data <- raw_data %>%
  filter(year == 2016) %>%
  select(year, state, total_pop_15to64, black_pop_15to64,
         latinx_pop_15to64, white_pop_15to64, 
         total_prison_pop, black_prison_pop, latinx_prison_pop,
         other_race_prison_pop, white_prison_pop) %>%
  drop_na()

# This is only applicable to counties where all data was available

# View(recent_data)

by_state <- recent_data %>%
  group_by(state) %>% 
  summarize(
    total_pop = sum(total_pop_15to64),
    black_pop_15to64 = sum(black_pop_15to64),
    latinx_pop_15to64 = sum(latinx_pop_15to64),
    white_pop_15to64 = sum(white_pop_15to64),
    total_prison_pop = sum(total_prison_pop),
    black_prison_pop = sum(black_prison_pop),
    latinx_prison_pop = sum(latinx_prison_pop),
    otherrace_prison_pop = sum(other_race_prison_pop),
    white_prison_pop = sum(white_prison_pop))


# View(by_state)

states_summary <- data.frame(
  state = by_state$state,
  black_pop = round((by_state$black_pop_15to64 / by_state$total_pop) * 100, 2),
  black_imprisoned = round(
    (by_state$black_prison_pop / by_state$total_prison_pop) * 100, 2),
  latinx_pop = round((by_state$latinx_pop_15to64 / by_state$total_pop) * 100, 2),
  latinx_imprisoned = round(
    (by_state$latinx_prison_pop / by_state$total_prison_pop) * 100, 2),
  white_pop = round((by_state$white_pop_15to64 / by_state$total_pop) * 100, 2),
  white_imprisoned = round(
    (by_state$white_prison_pop / by_state$total_prison_pop) * 100, 2)
)

#View(states_summary)

avg_blackpop <- round(mean(states_summary$black_pop), 2)
avg_blackprison <- round(mean(states_summary$black_imprisoned), 2)
avg_latinxpop <- round(mean(states_summary$latinx_pop), 2)
avg_latinxprison <- round(mean(states_summary$latinx_imprisoned), 2)
avg_whitepop <- round(mean(states_summary$white_pop), 2)
avg_whiteprison <- round(mean(states_summary$white_imprisoned), 2)



#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# Returns a dataframe with the national total of jail populace by year,
# from years 1970-2018.
get_year_jail_pop <- function() {
  jail_by_year <- raw_data %>%
    filter(year >= 1970) %>%
    select(year, state, total_jail_pop) %>%
    drop_na() %>%
    group_by(year) %>%
    summarize(
      total_jail = sum(total_jail_pop)
    )
return(jail_by_year)   
}

# View(get_year_jail_pop())

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  jail_plot <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(
      x = year, y = total_jail)) +    
    xlab("Year") + 
    scale_y_continuous(name="Total Jail Population", 
                       labels = scales::comma) + 
    labs(title = "Increase of Jail Population in U.S. (1970-2018)")
  return(jail_plot)   
} 


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# get_jail_pop_by_states(states): This data wrangling function should return a 
# data frame that is suitable for visualization. The parameter states should be
# a vector of states.
# plot_jail_pop_by_states(states): This plotting function should return the chart.
# The parameter states should be a vector of states. 
# This function should call the data wrangling function.

get_jail_pop_by_states <- function(states){
  jail_bystate <- raw_data %>%
    filter(state %in% states) %>%
    select(year, state, total_jail_pop) %>%
    drop_na() %>%
    group_by(year, state) %>%
    summarize(total_jail = sum(total_jail_pop))
  return(jail_bystate)   
}

# View(get_jail_pop_by_states(c("KY", "MI", "NM", "MA")))

plot_jail_pop_by_states <- function(states) {
  state_plot <- ggplot(data = get_jail_pop_by_states(states)) +
                         geom_line(size=1.5, 
                           mapping = 
                             aes(x = year, y = total_jail, color = state)
                         ) +
    xlab("Year") + 
    scale_y_continuous(name="Total Jail Population", 
                       labels = scales::comma) + 
    labs(title = "Increase of Jail Population in 6 States (1970-2018)")
  return(state_plot)   
}

# plot_jail_pop_by_states(c("KY", "MI", "NM", "MA"))


#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Proportions of Nonwhite Prisoners by Urbanicity
raw_data <- read.csv("../source/incarceration_trends.csv",
                     stringsAsFactors = FALSE) 

race_pris <- filter(raw_data, year >= 2000) %>%
  select(year, state, county_name, urbanicity, total_prison_pop,
         white_prison_pop, aapi_prison_pop, black_prison_pop, 
         latinx_prison_pop, native_prison_pop, other_race_prison_pop ) %>%
  drop_na()


by_urban <- race_pris %>%
  group_by(urbanicity) %>% 
  summarize(avg_total = mean(total_prison_pop),
            avg_white = mean(white_prison_pop),
            avg_aapi = mean(aapi_prison_pop), 
            avg_black = mean(black_prison_pop),
            avg_latinx = mean(latinx_prison_pop),
            avg_native = mean(native_prison_pop),
            avg_other = mean(other_race_prison_pop))

percent <- data.frame(
  urbanicity = by_urban$urbanicity,
  white = round((by_urban$avg_white / by_urban$avg_total) * 100, 2),
  aapi = round((by_urban$avg_aapi/ by_urban$avg_total) * 100, 2),
  black = round((by_urban$avg_black / by_urban$avg_total) * 100, 2),
  latinx = round((by_urban$avg_latinx / by_urban$avg_total) * 100, 2),
  native = round((by_urban$avg_native / by_urban$avg_total) * 100, 2)
) %>%
  gather(key = race, value = Percent, white:native)

# View(percent)

plot_prison <- function(){
  prison_plot <- ggplot(data = percent) +
    geom_col(mapping = 
               aes(x = urbanicity, y = Percent, fill = race)) +
    scale_color_brewer(palette = "Dark2") + 
    labs(
      title = "Percentages of Races in Prison by Urbanicity (2000-2018)",
      x = "Urbanicity",
      y = "Percent")
  return(prison_plot)
}


options(scipen = 100)

gen_pop <- filter(raw_data, year >= 2000) %>%
  select(year, state, county_name, urbanicity, total_pop_15to64,
         aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64,
         native_pop_15to64, white_pop_15to64) %>%
  drop_na() %>%
  group_by(urbanicity) %>%
  summarize(
    total_pop_15to64 = mean(total_pop_15to64),
    white_pop_15to64 = mean(white_pop_15to64),
    aapi_pop_15to64 = mean(aapi_pop_15to64),
    black_pop_15to64 = mean(black_pop_15to64),
    latinx_pop_15to64 = mean(latinx_pop_15to64),
    native_pop_15to64 = mean(native_pop_15to64)
  ) %>%
  filter(!row_number() == 1) %>%
  mutate(
    white = round((white_pop_15to64 / total_pop_15to64) * 100, 2),
    aapi = round((aapi_pop_15to64 / total_pop_15to64) * 100, 2),
    black = round((black_pop_15to64 / total_pop_15to64) * 100, 2),
    latinx = round((latinx_pop_15to64 / total_pop_15to64) * 100, 2),
    native = round((native_pop_15to64 / total_pop_15to64) * 100, 2)
  ) %>%
  select(urbanicity, white, aapi, black,
         latinx, native) %>%
  gather(key = race, value = Percent, white:native)

# View(gen_pop)
plot_gen <- function(){
  gen_plot <- ggplot(data = gen_pop) +
    geom_col(mapping = 
               aes(x = urbanicity, y = Percent, fill = race)) +
    scale_color_brewer(palette = "Dark2") + 
    labs(
      title = "Percentages of Races in Overall Population
      by Urbanicity (2000-2018)",
      x = "Urbanicity",
      y = "Percent")
  return(gen_plot)
}


#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Map of Female Prison Population Rates
#----------------------------------------------------------------------------#


prison_rate <- raw_data %>%
  select(year, state, total_prison_adm_rate) %>%
  group_by(state) %>%
  drop_na() %>%
  summarize(prison_pop = round(mean(total_prison_adm_rate), 2)) %>%
  mutate(state = tolower(state.name[match(state, state.abb)]))


# View(prison_rate)

states <- map_data("state") %>%
  rename(state = region) %>%
  left_join(prison_rate, by="state")

# View(states) 


plot_map <- function() {
  map <- ggplot(states) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = prison_pop),
      color="gray",
      size=.25
    ) +
    coord_map() +
    scale_fill_continuous(low = "White", high = "#ff5100") +
    labs(
      title = "State Prison Admissions Rates",
      fill = "Average Prison Admissions Rate",
      x = " ",
      y = " "
    )
  return(map)
}





