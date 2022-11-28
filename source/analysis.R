library("tidyverse")
library("ggplot2") 

# The functions might be useful for A4
source("../source/a4-helpers.R")

raw_data <- read.csv("../source/incarceration_trends.csv",
                stringsAsFactors = FALSE)

# View(raw_data)




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
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


