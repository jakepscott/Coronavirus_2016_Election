# libraries ---------------------------------------------------------------
library(tidyverse)
library(readr)
library(lubridate)
library(zoo)
library(ggtext)
library(scales)
library(geofacet)
library(patchwork)
library(socviz)


# Loading Raw Data --------------------------------------------------------
Election_2016_By_State <- election
corona_cases_state <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")) %>% 
  rename("State"=state)
state_pop <- read_rds("data/state_pop_clean.RDS") %>% rename("State"=state)
State_Names <- read_rds("data/State_Names.RDS")


# Cleaning Corona Data ----------------------------------------------------
corona_cases_state <- left_join(corona_cases_state,state_pop,by="State") 
corona_cases_state <- corona_cases_state  %>% filter(State %in% State_Names$state | State=="District of Columbia")


# Cleaning Election Data --------------------------------------------------

Election_2016_By_State <- Election_2016_By_State %>% 
  rename("State"=state) %>% 
  mutate(winner=ifelse(winner=="Trump","Trump Won","Clinton Won"),
         trump_win_margin=(pct_trump=pct_clinton)) %>% 
  select(State,winner,trump_win_margin)


# Joining Corona and Election Data ----------------------------------------

state_data <- left_join(corona_cases_state,Election_2016_By_State,by="State")


# Making Cases and Deaths Variables ---------------------------------------
Winner_Population <- state_data %>% 
  distinct(State,.keep_all = T) %>% 
  group_by(winner) %>% 
  summarise(Winner_Population=sum(State_Population))

state_data <- left_join(state_data,Winner_Population,by="winner")

state_data_grouped <- state_data %>%
  group_by(date,winner) %>% summarize(Cases=sum(cases),
                                      Deaths=sum(deaths),
                                      Winner_Population=last(Winner_Population)) %>% 
  ungroup() %>% 
  mutate(Cases_Per_Million=(Cases/Winner_Population)*1000000,
         Deaths_Per_Million=(Deaths/Winner_Population)*1000000) %>% 
  group_by(winner) %>% 
  mutate(New_Cases=Cases-lag(Cases), ##New cases is today's cases minus yesterdays
         New_Cases_Avg=rollmean(New_Cases,k = 7,fill = NA, align = "right"),
         New_Cases_Per_Million=(New_Cases/Winner_Population)*1000000,
         New_Cases_Per_Million_Avg=(New_Cases_Avg/Winner_Population)*1000000,
         
         New_Deaths=Deaths-lag(Deaths), 
         New_Deaths_Avg=rollmean(New_Deaths,k = 7,fill = NA, align = "right"),
         New_Deaths_Per_Million=(New_Deaths/Winner_Population)*1000000,
         New_Deaths_Per_Million_Avg=(New_Deaths_Avg/Winner_Population)*1000000)

# Graphing Data -----------------------------------------------------------

#Absolute New Cases
(a <-   ggplot(state_data_grouped,aes(x=date,y=New_Cases_Avg)) +
   geom_line(aes(color=winner),lwd=2) +
   coord_cartesian(ylim=c(0,27000),xlim=c(as.Date("2020-04-01"),as.Date("2020-06-20"))) +
   scale_x_date(expand = c(0,0),labels =c("April","May","June"), 
                breaks = c(as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"))) +
   scale_y_continuous(expand = c(0,0),label = comma) +
   scale_color_manual(values = c("#2E74C0", "#CB454A")) +
   labs(title = "*States* won by <span style='color: #CB454A'>**Trump**</span> are now seeing more new cases <br>per day than those won by <span style='color: #2E74C0'>**Clinton**</span>",
        subtitle = "7 Day Rolling Average of New Cases",
        x=NULL) +
   theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
   theme(plot.title = element_markdown(face = "bold", size = rel(1.5)),
         plot.subtitle = element_text(face = "plain", size = rel(1.1), color = "grey70"),
         plot.caption = element_text(face = "italic", size = rel(0.8), 
                                     color = "grey70"),
         plot.title.position = "plot", 
         axis.title = element_blank(),
         axis.text = element_text(siz=rel(1)),
         legend.position = "none"))


difference <- state_data_grouped %>% select(date,winner,New_Cases_Avg) %>% 
  pivot_wider(names_from = winner,values_from=New_Cases_Avg) %>% 
  mutate(difference=`Clinton Won`-`Trump Won`,
         more_cases=ifelse(difference>0,"More Clinton","More Trump"))

b <- ggplot(difference,aes(date,difference)) +
  geom_area(aes(fill=more_cases)) +
  coord_cartesian(xlim=c(as.Date("2020-04-01"),as.Date("2020-06-20"))) +
  scale_fill_manual(values = c("#2E74C0", "#CB454A")) +
  labs(caption = "Plot: @jakepscott2020 | Data: New York Times, MIT Election Lab") +
  theme_minimal() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 8) +
  theme(plot.title = element_blank(),
        plot.title.position = "plot", 
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        plot.caption = element_text(face = "italic", size = rel(1.2), 
                                    color = "grey70"))

(plot <- a /
    b + plot_layout(heights = c(8,1)))  
#ggsave("figures/New_Cases_by_State.png",dpi=600)



