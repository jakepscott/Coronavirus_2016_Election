clinton_win <- data %>% filter(trump_win_margin<=0)
quantile(clinton_win$trump_win_margin,seq(0,1,.1))

margin_size %>% filter(margin_size=="Trump 5+") 


data_margin <- left_join(data,margin_size,by=c("State","County"))

data_margin <- data_margin %>% filter(margin_size=="Trump 5+")

data_margin <- data_margin %>% 
  group_by(Date,County) %>% summarize(Cases=sum(Cases),
                                      Deaths=sum(Deaths),
                                      Winner_Population_Margin_Size=last(Winner_Population_Margin_Size)) %>% 
  ungroup() %>% 
  mutate(Cases_Per_Million=(Cases/Winner_Population_Margin_Size)*1000000,
         Deaths_Per_Million=(Deaths/Winner_Population_Margin_Size)*1000000) %>% 
  group_by(County) %>% 
  mutate(New_Cases=Cases-lag(Cases), ##New cases is today's cases minus yesterdays
         New_Cases_Avg=rollmean(New_Cases,k = 7,fill = NA, align = "right"),
         New_Cases_Per_Million=(New_Cases/Winner_Population_Margin_Size)*1000000,
         New_Cases_Per_Million_Avg=(New_Cases_Avg/Winner_Population_Margin_Size)*1000000,
         
         New_Deaths=Deaths-lag(Deaths), 
         New_Deaths_Avg=rollmean(New_Deaths,k = 7,fill = NA, align = "right"),
         New_Deaths_Per_Million=(New_Deaths/Winner_Population_Margin_Size)*1000000,
         New_Deaths_Per_Million_Avg=(New_Deaths_Avg/Winner_Population_Margin_Size)*1000000)


ggplot(data_margin,aes(x=Date,y=New_Cases_Per_Million_Avg)) +
  geom_line(lwd=1) +
  facet_wrap(~County) +
  scale_x_date(expand = c(0,0),labels =c("April","May","June"), 
               breaks = c(as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"))) +
  scale_y_continuous(expand = c(0,0),label = comma) +
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
        axis.text = element_text(siz=rel(1)))


# Turns out it is because of Suffolk County! ------------------------------
margin_size <- election_results_2016 %>% ungroup() %>% 
  mutate(margin_size = case_when(trump_win_margin<=-.3~"Clinton 30+",
                                 trump_win_margin>(-.3) & trump_win_margin<=-.1~"Clinton 10+",
                                 trump_win_margin>-.1 & trump_win_margin<(-.05)~"Clinton 5+",
                                 trump_win_margin>=-.05 & trump_win_margin<=0~"Clinton Within 5",
                                 trump_win_margin>0 & trump_win_margin<=.05~"Trump Within 5",
                                 trump_win_margin>.05 & trump_win_margin<=.1~"Trump 5+",
                                 trump_win_margin>.1 & trump_win_margin<=.3~"Trump 10+",
                                 trump_win_margin>.3~"Trump 30+")) %>% 
  select(State,County,margin_size,County_Population)

margin_size$margin_size <- factor(margin_size$margin_size,
                                  levels = c("Clinton 30+","Clinton 10+","Clinton 5+", "Clinton Within 5",
                                             "Trump Within 5","Trump 5+","Trump 10+", "Trump 30+"),
                                  ordered = T)


winner_pop_by_margin <- margin_size %>% group_by(margin_size) %>% 
  summarise(Winner_Population_Margin_Size=sum(County_Population,na.rm = T))

margin_size <- margin_size %>% select(-County_Population)

margin_size <- left_join(margin_size,winner_pop_by_margin,by="margin_size")

data_margin <- left_join(data,margin_size,by=c("State","County")) %>% filter(!(State=="New York" & County=="Suffolk"))

data_margin <- data_margin %>% 
  group_by(Date,margin_size) %>% summarize(Cases=sum(Cases),
                                           Deaths=sum(Deaths),
                                           Winner_Population_Margin_Size=last(Winner_Population_Margin_Size),
                                           County=last(County),
                                           State=last(State)) %>% 
  ungroup() %>% 
  mutate(Cases_Per_Million=(Cases/Winner_Population_Margin_Size)*1000000,
         Deaths_Per_Million=(Deaths/Winner_Population_Margin_Size)*1000000) %>% 
  group_by(margin_size) %>% 
  mutate(New_Cases=Cases-lag(Cases), ##New cases is today's cases minus yesterdays
         New_Cases_Avg=rollmean(New_Cases,k = 7,fill = NA, align = "right"),
         New_Cases_Per_Million=(New_Cases/Winner_Population_Margin_Size)*1000000,
         New_Cases_Per_Million_Avg=(New_Cases_Avg/Winner_Population_Margin_Size)*1000000,
         
         New_Deaths=Deaths-lag(Deaths), 
         New_Deaths_Avg=rollmean(New_Deaths,k = 7,fill = NA, align = "right"),
         New_Deaths_Per_Million=(New_Deaths/Winner_Population_Margin_Size)*1000000,
         New_Deaths_Per_Million_Avg=(New_Deaths_Avg/Winner_Population_Margin_Size)*1000000)

cc <- scales::seq_gradient_pal("#2E74C0", "#CB454A")(seq(0,1,length.out=8))
data_margin <- data_margin %>% filter(!is.na(margin_size))

ggplot(data_margin,aes(x=Date,y=New_Cases_Per_Million_Avg)) +
  geom_line(aes(color=margin_size),lwd=1) +
  facet_wrap(~margin_size) +
  scale_x_date(expand = c(0,0),labels =c("April","May","June"), 
               breaks = c(as.Date("2020-04-01"),as.Date("2020-05-01"),as.Date("2020-06-01"))) +
  scale_y_continuous(expand = c(0,0),label = comma) +
  scale_color_manual(values=cc) +
  labs(title = "However, the gap between <span style='color: #CB454A'>**Trump**</span> and <span style='color: #2E74C0'>**Clinton**</span> counties is <br>shrinking rapidly",
       subtitle = "7 Day Rolling Average of New Cases Per Million Residents",
       x=NULL) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title = element_markdown(face = "bold", size = rel(1.5)),
        plot.subtitle = element_text(face = "plain", size = rel(1.1), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        panel.grid = element_blank(),
        plot.title.position = "plot", 
        axis.title = element_blank(),
        axis.text = element_text(siz=rel(1)),
        legend.position = "none")

