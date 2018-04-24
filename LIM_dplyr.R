setwd("C:/Users/wooki/Dropbox/R_class/dplyr")
data<-read.csv("March2018.csv",header=T)
head(data)

#2
numcrime <- data %>% 
  group_by(Description) %>% 
  summarise(count=n()) 
numcrime

answer<-arrange(numcrime, desc(count))

#3
neigh <- data %>% 
  group_by(Neighborhood) %>% 
  summarise(count=n())
neigh

answer<-arrange(neigh, desc(count))


#4

rob <- data %>% 
  group_by(District) %>% 
  dplyr::filter(grepl('ROBBERY', Description)) %>% 
  summarise (count = n()) %>%
  mutate(freq = count / sum(count))


#5
library(ggplot2)
data$DateOccur<-as.Date(data$DateOccur,"%m/%d/%Y")
data_date<-arrange(data, data$DateOccur)
date <- data %>%
  filter(DateOccur > as.Date("2018-1-1")) %>%
  group_by(DateOccur) %>%
  summarise(count=n()) 

ggplot(date, aes(x=DateOccur, y=count)) + 
  geom_line() + 
  labs(
       y="Number of crimes", 
       x="Date", 
       title="Crimes by dates")

#6
data$DateOccur<-as.Date(data$DateOccur,"%m/%d/%Y")
data_date<-arrange(data, data$DateOccur)
date <- data %>%
  filter(DateOccur > as.Date("2018-1-1")) %>%
  group_by(DateOccur,District) %>%
  summarise(count=n()) 

ggplot(date, aes(x=DateOccur, y=count,group=(District))) + 
  geom_line(aes(color=factor(District)))+
  scale_color_manual(name="District",values=c('purple', 'red','blue','green','yellow','pink','black'))+
  labs(
    y="Number of crimes", 
    x="Date", 
    title="Crimes by dates and District")
