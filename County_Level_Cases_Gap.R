# libraries ---------------------------------------------------------------
library(tidyverse)
library(readr)
library(lubridate)
library(zoo)
library(ggtext)
library(scales)
library(geofacet)
library(patchwork)

windowsFonts(`Roboto Condensed` = windowsFont("Roboto Condensed"))

# Raw Data -------------------------------------------------------
#election_results <- read_csv("data/countypres_2000-2016.csv")
#saveRDS(election_results,"data/election_results.rds")
election_results <- read_rds("data/election_results.rds")
county_pop <- read_rds("data/county_pop_clean.RDS") %>% rename("County"=county,"State"=state) %>% 
  mutate(County=ifelse(str_detect(County,"Parish"),str_remove(County," Parish"),County))
state_pop <- read_rds("data/state_pop_clean.RDS") %>% rename("State"=state)
State_Names <- read_rds("data/State_Names.RDS")
corona_cases <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))


# Cleaning County Covid Data-----------------------------------------------
corona_cases <- left_join(corona_cases,State_Names, by=c("state"))
corona_cases <- corona_cases %>% rename("County"=county,"State"=state, "Date"=date, "Cases"=cases,"Deaths"=deaths)
corona_cases <- corona_cases %>% filter(State %in% State_Names$state | State=="District of Columbia") %>% 
  filter(County!="Unknown") %>% filter(Date>="2020-03-015") %>% 
  mutate(County=ifelse(str_detect(County,"city")==TRUE & State=="Virginia", str_remove(County," city"),County))

# Cleaning Election Data --------------------------------------------------
#Getting a winner variable
election_results_2016 <- election_results %>% filter(year==2016, party %in% c("democrat","republican")) %>% select(-version,-party) %>% 
  pivot_wider(names_from = candidate,values_from=candidatevotes) %>% 
  mutate(percent_dem=`Hillary Clinton`/totalvotes,
         percent_rep=`Donald Trump`/totalvotes,
         trump_win_margin=percent_rep-percent_dem,
         winner=ifelse(trump_win_margin>0,"Trump Won","Clinton Won")) %>%
  rename("County"=county,"State"=state) %>% 
  select(State,County,winner,percent_dem,percent_rep,trump_win_margin)
 
##Joining with County and State Population
election_results_2016 <- left_join(election_results_2016,county_pop,by=c("State","County"))
election_results_2016 <- left_join(election_results_2016,state_pop,by=c("State"))
#Getting the Population of the counties that voted for Clinton Versus for Trump
election_results_2016 <- election_results_2016 %>% group_by(winner) %>% 
  mutate(Winner_Population=sum(County_Population,na.rm = T)) %>% 
  filter(!(State=="Virginia" & County=="Bedford" & is.na(winner)),
         !(str_detect(County,"writein")==TRUE))

# Joining Data ------------------------------------------------------------
#Joining election results, cases, and county population
data <- left_join(corona_cases,election_results_2016,by=c("State","County")) %>% 
  mutate(County_Population=ifelse(County=="New York City",8399000,County_Population))


# Adding Winners for Counties Not Included --------------------------------
data <- data %>%  
  mutate(trump_win_margin=case_when(County=="New York City"~(-.7),
                                    County=="St. Louis"~(-.162),
                                    County=="St. Louis city"~(-.63),
                                    County=="Baltimore city"~(-.742),
                                    County=="Anchorage"~(.54),
                                    County=="Aleutians East Borough"~.243,
                                    County=="Aleutians West Census Area"~(-.241),
                                    County=="Bethel Census Area"~(-.327),
                                    County=="Bristol Bay Borough"~.291,
                                    County=="DeSoto"~.275,
                                    County=="DeWitt"~.636,
                                    County=="Dillingham Census Area"~.154,
                                    County=="Doña Ana"~-.178,
                                    County=="Fairbanks North Star Borough"~.209,
                                    County=="Haines Borough"~-.039,
                                    County=="Juneau City and Borough"~-.185,
                                    County=="Kenai Peninsula Borough"~.373,
                                    County=="Ketchikan Gateway Borough"~.237,
                                    County=="Kodiak Island Borough"~.184,
                                    County=="Lac qui Parle"~.256,
                                    County=="Lake and Peninsula Borough"~-.038,
                                    County=="LaSalle"~.796,
                                    County=="Matanuska-Susitna Borough"~.516,
                                    County=="Nome Census Area"~-.229,
                                    County=="North Slope Borough"~-.124,
                                    County=="Northwest Arctic Borough"~.275,
                                    County=="Petersburg Borough"~.203,
                                    County=="Prince of Wales-Hyder Census Area"~.078,
                                    County=="Sitka City and Borough"~-.068,
                                    County=="Southeast Fairbanks Census Area"~.516,
                                    County=="Valdez-Cordova Census Area"~.288,
                                    County=="Wrangell City and Borough"~.428,
                                    County=="Yukon-Koyukuk Census Area"~.273,
                                    TRUE~trump_win_margin),
        winner=case_when(County=="New York City"~"Clinton Won",
                          County=="St. Louis" & State=="Missouri"~"Clinton Won",
                          County=="St. Louis city" & State=="Missouri"~"Clinton Won",
                          County=="Baltimore city"~"Clinton Won",
                          County=="Virgina Beach city"~"Trump Won",
                          County=="Chesapeake city"~"Trump Won",
                          County=="Norfolk city"~"Clinton Won",
                          County=="Richmond city"~"Clinton Won",
                          County=="Doña Ana"~"Clinton Won",
                          County=="Newport News city"~"Clinton Won",
                          County=="Alexandria city"~'Clinton Won',
                          County=="Hampton city"~'Clinton Won',
                          County=="Matanuska-Susitna Borough"~'Trump Won',
                          County=="Roanoke city"~'Clinton Won',
                          County=="Fairbanks North Star Borough"~'Trump Won',
                          County=="Portsmouth city"~'Clinton Won',
                          County=="Suffolk city"~'Clinton Won',
                          County=="Lynchburg city"~'Trump Won',
                          County=="Bedford"~'Trump Won',
                          County=="Kenai Peninsula Borough"~'Trump Won',
                          County=="Harrisonburg city"~'Clinton Won',
                          County=="Charlottesville city"~'Clinton Won',
                          County=="Manassas city"~'Clinton Won',
                          County=="Danville city"~'Clinton Won',
                          County=="Desoto"~'Trump Won',
                          County=="Juneau City and Borough"~'Clinton Won',
                          County=="Petersburg city"~'Clinton Won',
                          County=="Fredericksburg  city"~'Clinton Won',
                          County=="Winchester city"~'Clinton Won',
                          County=="Salem city"~'Trump Won',
                          County=="Staunton city"~'Clinton Won',
                          County=="Fairfax city"~'Clinton Won',
                          County=="Waynesboro city"~'Trump Won',
                          County=="Hopewell city"~'Clinton Won',
                          County=="DeWitt"~'Trump Won',
                          County=="Bethel Census Area"~'Clinton Won',
                          County=="Radford city"~'Clinton Won',
                          County=="Manassas Park city"~'Clinton Won',
                          County=="Colonial Heights city"~'Trump Won',
                          County=="Bristol city"~'Trump Won',
                          County=="Williamsburg city"~'Clinton Won',
                          County=="Falls Church city"~'Clinton Won',
                          County=="Ketchikan Gateway Borough"~'Trump Won',
                          County=="Kodiak Island Borough"~'Trump Won',
                          County=="Poquoson city"~'Trump Won',
                          County=="Nome Census Area"~'Clinton Won',
                          County=="North Slope Borough"~'Clinton Won',
                          County=="Valdez-Cordova Census Area"~'Trump Won',
                          County=="Sitka City and Borough"~'Trump Won',
                          County=="Franklin city"~'Clinton Won',
                          County=="Northwest Arctic Borough"~'Clinton Won',
                          County=="Lexington city"~'Clinton Won',
                          County=="Southeast Fairbanks Census Area"~'Trump Won',
                          County=="Lac qui Parle"~'Trump Won',
                          County=="Buena Vista city"~'Trump Won',
                          County=="Galax city"~'Trump Won',
                          County=="Prince of Wales-Hyder Census Area"~'Trump Won',
                          County=="Aleutians West Census Area"~'Clinton Won',
                          County=="Covington city"~'Trump Won',
                          County=="Emporia city"~'Clinton Won',
                          County=="Yukon-Koyukuk Census Area"~'Clinton Won',
                          County=="Dillingham Census Area"~'Clinton Won',
                          County=="Norton city"~'Trump Won',
                          County=="Aleutians East Borough"~'Trump Won',
                          County=="Petersburg Borough"~'Trump Won',
                          County=="Haines Borough"~'Clinton Won',
                          County=="Wrangell City and Borough"~'Trump Won',
                          County=="Lake and Peninsula Borough"~'Trump Won',
                          County=="Bristol Bay Borough"~'Trump Won',
                          TRUE~winner)) %>% 
  filter(!is.na(winner))



# Getting Cases, Deaths, and New Cases/Deaths Vars for full country ----------------------------------------------------------------
data_grouped <- data %>%
  group_by(Date,winner) %>% summarize(Cases=sum(Cases),
                                      Deaths=sum(Deaths),
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
         
         
# Analysis ----------------------------------------------------------------
# Full Country ------------------------------------------------------------
#Absolute New Cases
(a <-   ggplot(data_grouped,aes(x=Date,y=New_Cases_Avg)) +
    geom_line(aes(color=winner),lwd=2) +
    coord_cartesian(ylim=c(0,27000)) +
    scale_x_date(expand = c(0,0),labels =c("April","May","June"), 
                 breaks = c(as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"))) +
    scale_y_continuous(expand = c(0,0),label = comma) +
    scale_color_manual(values = c("#2E74C0", "#CB454A")) +
    labs(title = "Counties that voted for <span style='color: #CB454A'>**Trump**</span> have consistently seen fewer <br>new cases per day than those that voted for <span style='color: #2E74C0'>**Clinton**</span>",
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


difference <- data_grouped %>% select(Date,winner,New_Cases_Avg) %>% 
  pivot_wider(names_from = winner,values_from=New_Cases_Avg) %>% 
  mutate(difference=`Clinton Won`-`Trump Won`,
         more_cases=ifelse(difference>0,"More Clinton","More Trump"))

b <- ggplot(difference,aes(Date,difference)) +
  geom_area(aes(fill=more_cases)) +
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
#ggsave("figures/Clinton_Vs_Trump_Counties_Absolute.png",dpi=600)

#Per Million Residents

(a <-   ggplot(data_grouped,aes(x=Date,y=New_Cases_Per_Million_Avg)) +
    geom_line(aes(color=winner),lwd=2) +
    coord_cartesian(ylim=c(0,150),xlim=c(as.Date("2020-05-01"),as.Date("2020-06-20"))) +
    scale_x_date(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0),label = comma) +
    scale_color_manual(values = c("#2E74C0", "#CB454A")) +
    labs(title = "However, the gap between <span style='color: #CB454A'>**Trump**</span> and <span style='color: #2E74C0'>**Clinton**</span> counties is <br>shrinking rapidly",
         subtitle = "7 Day Rolling Average of New Cases Per Million Residents",
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


difference <- data_grouped %>% select(Date,winner,New_Cases_Per_Million_Avg) %>% 
  pivot_wider(names_from = winner,values_from=New_Cases_Per_Million_Avg) %>% 
  mutate(difference=`Clinton Won`-`Trump Won`,
         more_cases=ifelse(difference>0,"More Clinton","More Trump"))

b <- ggplot(difference,aes(Date,difference)) +
  geom_area(aes(fill=more_cases)) +
  coord_cartesian(xlim=c(as.Date("2020-05-01"),as.Date("2020-06-20"))) +
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
#ggsave("figures/Clinton_Vs_Trump_Counties.png",dpi=600)

# By State ----------------------------------------------------------------
winner_pop_by_state <- election_results_2016 %>% ungroup() %>% group_by(State,winner) %>% 
  summarise(Winner_Population_State=sum(County_Population,na.rm = T)) %>% 
  mutate(Winner_Population_State=case_when(State=="Alaska" & winner=="Clinton Won"~116454,
                                           State=="Alaska" & winner=="Trump Won"~163387,
                                           TRUE~Winner_Population_State))


state_data_grouped <- left_join(data,winner_pop_by_state,by=c("State","winner"))
  
state_data_grouped <- state_data_grouped %>% 
  group_by(Date,State,winner) %>% summarize(Cases=sum(Cases),
                                            Deaths=sum(Deaths),
                                            Winner_Population_State=last(Winner_Population_State)) %>% 
  ungroup() %>% 
  mutate(Cases_Per_Million=(Cases/Winner_Population_State)*1000000,
         Deaths_Per_Million=(Deaths/Winner_Population_State)*1000000) %>% 
  group_by(State,winner) %>% 
  mutate(New_Cases=Cases-lag(Cases), ##New cases is today's cases minus yesterdays
         New_Cases_Avg=rollmean(New_Cases,k = 7,fill = NA, align = "right"),
         New_Cases_Per_Million=(New_Cases/Winner_Population_State)*1000000,
         New_Cases_Per_Million_Avg=(New_Cases_Avg/Winner_Population_State)*1000000,
         
         New_Deaths=Deaths-lag(Deaths), 
         New_Deaths_Avg=rollmean(New_Deaths,k = 7,fill = NA, align = "right"),
         New_Deaths_Per_Million=(New_Deaths/Winner_Population_State)*1000000,
         New_Deaths_Per_Million_Avg=(New_Deaths_Avg/Winner_Population_State)*1000000)

State_Names <- State_Names %>% rename("State"=state)
state_data_grouped <- left_join(state_data_grouped,State_Names)

ggplot(state_data_grouped,aes(x=Date,y=New_Cases_Per_Million_Avg)) +
  geom_line(aes(color=winner)) +
  facet_geo(~abb,scales="free_y") +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),label = comma) +
  scale_color_manual(values = c("#2E74C0", "#CB454A")) +
  labs(title = "The national trend is not generally reflected within states",
       x=NULL,
       caption = "Plot: @jakepscott2020 | Data: New York Times, MIT Election Lab") +
  theme_bw(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title = element_markdown(face = "bold", size = rel(1.5)),
        plot.subtitle = element_text(face = "plain", size = rel(1.1), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        plot.title.position = "plot", 
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size=rel(.6), margin = margin(.05,0,.05,0, "cm")),
        strip.background=element_rect(color = "black",  fill="grey90"),
        legend.position = "none") 
ggsave("figures/geo_facet.png",dpi=600,width = 7.5,height = 5)


# Deaths ------------------------------------------------------------------

# Deaths ------------------------------------------------------------------
#Per Million Residents

(a <-   ggplot(data_grouped,aes(x=Date,y=New_Deaths_Per_Million_Avg)) +
   geom_line(aes(color=winner),lwd=2) +
   coord_cartesian(xlim=c(as.Date("2020-04-01"),as.Date("2020-06-20")),ylim=c(0,12)) +
   scale_x_date(expand = c(0,0)) +
   scale_y_continuous(expand = c(0,0)) +
   scale_color_manual(values = c("#2E74C0", "#CB454A")) +
   labs(title = "The gap in new deaths is also closing, but more slowly than new \ncases",
        subtitle = "7 Day Rolling Average of New Deaths Per Million Residents",
        x=NULL) +
   theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
   theme(plot.title = element_text(face = "bold", size = rel(1.5)),
         plot.subtitle = element_text(face = "plain", size = rel(1.1), color = "grey70"),
         plot.caption = element_text(face = "italic", size = rel(0.8), 
                                     color = "grey70"),
         plot.title.position = "plot", 
         axis.title = element_blank(),
         axis.text = element_text(siz=rel(1)),
         legend.position = "none"))

ggsave("figures/New_Deaths_by_county.png",dpi=600)
