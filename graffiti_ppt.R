# Data Narative Project

library(officer)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(leaflet)
library(htmltools)
library(rgdal)
library(Rmisc)
library(tidytext)
library(twitteR)
library(stringr)
library(wordcloud)
library(reshape2)
library(maptools)
library(sp)
library(tm)
library(NLP)
library(devtools)
library(streamR)
library(RCurl)
library(ROAuth)
library(graphTweets)
library(igraph)
library(readr)
library(SnowballC)
library(data.table)
library(ggthemes)
library(scales)
library(geojsonio)
library(RColorBrewer)

graffiti <- read_pptx("graffiti.pptx")

# Title Slide
graffiti <- graffiti %>%
  add_slide(layout="Title Slide",master="Ion Boardroom") %>%
  ph_with_text(type="ctrTitle",str="Boston 311: Investigating Graffiti Calls") %>%
  ph_with_text(type="subTitle",str="Data + Narrative with R Workshop")

# Slide 1: Introduction
graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title",str="Introduction") %>%
  ph_with_ul(type="body",index = 1,
             str_list=c("Hanan Alsalamah", "Arvindh Raghavan", "Ian Riaf","Nicole Yin"),
             level_list=c(1,1,1)) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

#+++++++++++
boston311 <- read.csv("311graffiti.csv")
boston311$open_dt <- as.POSIXlt(boston311$open_dt,format="%m/%d/%Y %H:%M",tz=Sys.timezone())
boston311$target_dt <- as.POSIXlt(boston311$target_dt,format="%m/%d/%Y %H:%M",tz=Sys.timezone())
boston311$closed_dt <- as.POSIXlt(boston311$closed_dt,format="%m/%d/%Y %H:%M",tz=Sys.timezone())
#++++++++++


## Slide 2: Top 15 Reasons for 311 Call
#++++++++++

data.311 <- fread("bos311.csv")

info <- data.311 %>% 
  select(REASON, TYPE, neighborhood, Source)
reason <- info %>% 
  group_by(REASON) %>%
  count(REASON) %>% 
  arrange(desc(n))

reason_u <- reason %>% 
  ungroup() %>% 
  top_n(15,n)
reason_u$REASON <- factor(reason_u$REASON, levels = fct_infreq(reason_u$REASON))

neighborhood <- info %>% 
  group_by(neighborhood) %>% 
  count(neighborhood) %>% 
  arrange(desc(n))
neighborhood_u <- neighborhood %>% 
  ungroup() %>% 
  top_n(10,n)
neighborhood_u$neighborhood <- factor(neighborhood_u$neighborhood, 
                                      levels = fct_infreq(neighborhood_u$neighborhood))
neighborhood_d <- neighborhood %>% 
  ungroup() %>% 
  top_n(-10,n)
neighborhood_d$neighborhood <- factor(neighborhood_d$neighborhood, 
                                      levels = fct_infreq(neighborhood_d$neighborhood))
type <- info %>% 
  group_by(REASON) %>% 
  count(TYPE) %>% 
  arrange(desc(n))
type_u <- type %>% 
  ungroup() %>% 
  top_n(10,n)
source <- info %>% 
  group_by(Source) %>% 
  count(Source) %>% 
  arrange(desc(n))
source$Source <- factor(source$Source, levels = fct_infreq(source$Source))
area.color <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "violetred3", NA, NA, NA)
#+++++++++++

topreason <- ggplot(reason_u, aes(x = REASON, y = n, fill = area.color)) + 
  geom_bar(stat = "identity") +
  labs(title = "Top 15 Reasons for 311 Call", x = "Reason", y = "Cases") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5), legend.position = "none") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_y_continuous(labels = comma)

graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title", str="Overview of Most Frequent 311 Calls") %>%
  ph_with_gg(type="body", index=1, value=topreason) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 3: Neighborhoods with Most Frequent 311 Calls
neigh <- ggplot(neighborhood_u, aes (x = neighborhood, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Neighborhoods for 311 Call", x = "Neighborhood", y = "Cases") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_y_continuous(labels = comma)

graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title", str="Most Frequent 311 Calls by Neighborhood") %>%
  ph_with_gg(type="body", index=1, value=neigh) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 4: Graffiti images (Before and After) 

graffiti <- graffiti %>%
  add_slide(layout = "Comparison",master="Ion Boardroom") %>%
  ph_with_text(type="title",str="Looking at Graffiti in Boston") %>%
  ph_with_text(type="body",index=3,str="Before") %>%
  ph_with_img(type="body",index=1,src="img1b.jpg",width=4) %>%
  ph_with_text(type="body",index=2,str="After") %>%
  ph_with_img(type="body",index=4,src="img1a.jpg",width=4) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 5: Word Cloud
#++++++++
graffiti.word <- read.csv("graffiti_word.csv")[,-1]

nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
graffiti_text_sentiment_stat <- graffiti.word %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)
bing_word_counts <- graffiti.word %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  filter(sentiment=="positive")
sentiment <- bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))
#+++++++++

graffiti.word.freq <- graffiti.word %>%
  count(word,sort=T)
gg.sent <- ggplot(sentiment,aes(word, n)) +
  geom_col(show.legend = FALSE,fill="deepskyblue4") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()+
  theme_economist()

graffiti <- graffiti %>%
  add_slide(layout="Two Content",master="Ion Boardroom") %>%
  ph_with_text(type="title", str="Text Analysis of Tweets about Graffiti") %>%
  ph_with_img(type="body", index=1, src = "wordcloud.png") %>%
  ph_with_gg(type="body",index=2,value=gg.sent) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 6: Frequency of Graffiti Calls by Neighborhood
summary2 <- summarySE(na.omit(boston311), measurevar="diffopenclosed", groupvars="neighborhood")

numcases <- ggplot(summary2, aes(x=reorder(neighborhood,-N), y=N)) + 
  theme_economist()+
  scale_colour_economist()+
  geom_bar(position=position_dodge(), stat="identity",fill="deepskyblue4") +
  ylab("Frequency")+
  xlab("Neighborhood")+
  guides(fill=FALSE)+
  theme(axis.text.x=element_text(angle=20,hjust=1,vjust=1))+
  ggtitle("Frequency of Calls by Neighborhood")

graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title", str="Frequency of Graffiti Calls by Neighborhood") %>%
  ph_with_gg(type="body", index=1, value = numcases) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 7: Map of Call Frequency by Neighborhood
graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title",str="Mapping the Boston Neighborhoods") %>%
  ph_with_img(type="body", index=1, src = "map_neighborhood.png",width=5) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 8: Response Time by Neighborhood
#++++++++
boston311$diffopentarget <- difftime(boston311$target_dt,boston311$open_dt,units = "days")
boston311$diffopenclosed <- difftime(boston311$closed_dt,boston311$open_dt,units = "days")
#++++++++

resptime <- ggplot(summary2, aes(x=reorder(neighborhood,-diffopenclosed), y=diffopenclosed)) + 
  geom_bar(position=position_dodge(), stat="identity",fill="deepskyblue4") +
  ylab("Days")+
  xlab("Neighborhood")+
  theme_economist()+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + guides(fill=FALSE)+
  theme(axis.text.x=element_text(angle=20,hjust=1,vjust=1)) +
  geom_text(aes(label=round(diffopenclosed,digit=0),vjust=-0.5)) +
  ggtitle("Response time")

graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title", str="Average Response Time for Graffiti Calls") %>%
  ph_with_gg(type="body", index=1, value = resptime) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 9: Map of Response Time by Neighborhood
graffiti <- graffiti %>%
  add_slide(layout="Title and Content",master="Ion Boardroom") %>%
  ph_with_text(type="title",str="Mapping Response Times by Neighborhood") %>%
  ph_with_img(type="body", index=1, src = "map_responsetime.png",width=5) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")

## Slide 10: Average Graffiti Calls by Month
#+++++++
boston311$Year <- year(boston311$open_dt)
boston311$Month <- month(boston311$open_dt,label=TRUE)
boston311$Day <- format(boston311$open_dt, "%A")
boston311$Day <- ordered(boston311$Day,levels=c("Monday","Tuesday","Wednesday",
                                              "Thursday","Friday","Saturday",
                                              "Sunday"))

bymonth <- as.data.frame(table(boston311$Year,boston311$Month))
colnames(bymonth) <- c("Year","Month","Freq")
avebymo <- aggregate(Freq ~ Month, data=bymonth, mean)
avebymo$Freq <- round(avebymo$Freq,2)
#+++++++

bymo <- ggplot(avebymo,mapping=aes(x=Month, y=Freq))+
  geom_bar(fill="deepskyblue4", stat="identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ggtitle("Average Graffiti Calls by Month")+
  xlab("Months")+
  ylab("Frequency")+
  theme_economist()

Filter <- c("2015","2016","2017")
bymonth1 <- bymonth[bymonth$Year %in% Filter,]

sp <- ggplot(bymonth1, aes(x=Month, y=Freq)) + 
  geom_point(size=3,color="deepskyblue4") + 
  facet_grid(Year ~ .)+
  theme(strip.text.x = element_text(size=8, angle=75),
        strip.text.y = element_text(size=12, face="bold"),
        strip.background = element_rect(colour="Pink", fill="#CCFFFF"),
        panel.background = element_rect(fill = "lightgrey",colour = "lightgrey",size = 0.5, 
                                        linetype = "solid"),
        axis.line = element_line(colour = "black"))+
  theme_economist(dkpanel=TRUE)+
  theme(panel.border = element_rect(color="black",fill=NA))+
  theme(legend.position="right")+
  ggtitle("Monthly Graffiti Calls by Year, 2015-2017")+
  xlab("Months")+
  ylab("Frequency")

graffiti <- graffiti %>%
  add_slide(layout = "Two Content",master="Ion Boardroom") %>%
  ph_with_text(type="title",str="Average Graffiti Calls by Month") %>%
  ph_with_gg(type="body",index=1,value=bymo) %>%
  ph_with_gg(type="body",index=2,value=sp) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")


## Slide 11: Average Response Time by Month
#++++++++
rtbymo <- aggregate(diffopenclosed ~ Year + Month, data = boston311, mean)
avertbymo <- aggregate(diffopenclosed ~ Month, data=boston311, mean)
rtmo1 <- rtbymo[rtbymo$Year %in% Filter,]
rtmo1$diffopenclosed <- round(rtmo1$diffopenclosed,2)
#++++++++

graf.resp <- ggplot(avertbymo)+
  geom_bar(aes(x=Month, y=diffopenclosed),stat="identity",fill="deepskyblue4")+
  ggtitle("Average Response Time by Month")+
  xlab("Month")+
  ylab("Response Time (Days)")+
  theme_economist()

graf.rt <- ggplot(rtmo1, aes(x=Month, y=diffopenclosed)) + 
  geom_point(size=3,color="deepskyblue4") + 
  facet_grid(Year ~ .)+
  theme_economist(dkpanel=TRUE)+
  theme(strip.text.x = element_text(size=8, angle=75),
        strip.text.y = element_text(size=12, face="bold"),
        panel.border = element_rect(color="black",fill=NA),
        axis.line = element_line(colour = "black"))+
  ggtitle("Response Rate by Year, 2015-2017")+
  xlab("Month")+
  ylab("Response Time (Days)")

graffiti <- graffiti %>%
  add_slide(layout = "Two Content",master="Ion Boardroom") %>%
  ph_with_text(type="title",str="Average Response Time by Month") %>%
  ph_with_gg(type="body",index=1,value=graf.resp) %>%
  ph_with_gg(type="body",index=2,value=graf.rt) %>%
  ph_with_text(type="ftr",str="Data + Narrative Workshop")


###

print(graffiti,target="graffiti.pptx")
