source("County_Setup_Code.R")         
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
#ggsave("figures/geo_facet.png",dpi=600,width = 7.5,height = 5)


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

#ggsave("figures/New_Deaths_by_county.png",dpi=600)
