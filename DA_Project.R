library(dplyr)
library(lubridate)
library(survey)
library(party)
library(rpart)
library(tree)
library(caret)
library(sampling)
library(ggplot2)


inspections<-read.csv("C:/Users/Vogiral Shivani/Desktop/inspections.csv")
violations<-read.csv("C:/Users/Vogiral Shivani/Desktop/violations.csv")
all<-inspections %>%
  inner_join(violations,by="serial_number")


all <- all[ymd(all$activity_date)>="2017-01-01",]
all<-all[(all$grade!=" "),]

s<-cluster(all,clustername=c("facility_city"),size=15,method="srswr")
a<-unique(s$facility_city)
all<-all[all$facility_city %in% a,]

#cleaning the attribute all$facility_zip
zip<-all$facility_zip
all$facility_zip<-substr(zip,start = 1, stop = 5)

#removing redundant columns

all<-select(all,-facility_address)
all<-select(all,-facility_state)
all<-select(all,-service_code)

write.csv(all,file="C:/Users/Vogiral Shivani/Desktop/all.csv")

#Top 10 Violations
types_violation<-all%>%
  group_by(violation_description)%>%
  summarise(count=n())
types_violation<-types_violation%>%top_n(10)

q<-ggplot(data=types_violation, aes(x=violation_description, y=count,fill=violation_description))
q+geom_bar(stat="identity",width = 0.5)+
  theme(axis.text.x = element_text(angle = 30))+
  theme(legend.position="none")+
  coord_flip()+
  scale_fill_hue(l=45)+
  labs(title="Top 10 Health Code Violations",x="Violations",y="Count")

#
restaurant_violations<-all %>%
  group_by(facility_name)%>%
  summarise(n_violations=n())
restaurant_violations<-top_n(restaurant_violations,10)
p<-ggplot(data=restaurant_violations, aes(x=facility_name, y=n_violations,fill=facility_name))
p+geom_bar(stat="identity",width = 0.5)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(title="10 Restaurants with the highest number of violations",y="Violation Count",x="Restaurants")

#month wise number of health code violations
all <- all %>%
  mutate(activity_date = ymd(activity_date),
         activity_month = round_date(activity_date, "month"))
options(repr.plot.width = 8, repr.plot.height = 6)

month_wise<-all %>%
  group_by(activity_month) %>%
  summarise(n_violations = n())
ggplot(data = month_wise, aes(activity_month, n_violations)) + 
  geom_line() + 
  scale_x_date(date_breaks="1 months",date_labels = "%b-%y") + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(title="Monthly count of health code violations",x="Month",y="Violation Count")


#
a<-all %>% count(facility_city,violation_description)
a<- a%>% group_by(facility_city) %>% top_n(n=5)

ggplot(a, aes(fill=violation_description, y=n, x=facility_city)) + 
  geom_bar( stat="identity", position="fill")+coord_flip()+
  labs(title="Cities and the Proportion of its top 5 violations",x="Cities",y="Proportion of Violations")



#
restaurant_violations<-all %>%
  group_by(facility_name)%>%
  summarise(n_violations=n())
restaurant_violations<-top_n(restaurant_violations,10)
b<-all %>% count(facility_name,violation_description,violation_code)
b<-b[(b$facility_name %in% restaurant_violations$facility_name),]
options(repr.plot.width = 8, repr.plot.height = 6)

ggplot(b, aes(fill=violation_code, y=n, x=facility_name)) + 
  geom_bar( stat="identity", position="fill")+
  theme(legend.position = "bottom")+
  coord_flip()+scale_fill_hue(l=45)+
  labs(title="Restaurant/Markets and the Proportion of its top violations",x="Restaurant/Markets",y="Proportion of Violations")


#Dropping levels from all factor columns of the data frame
all<-lapply(all, function(x) if(is.factor(x)) factor(x) else x)
write.csv(all,file="C:/Users/Vogiral Shivani/Desktop/all.csv")

library(FactoMineR)
library(factoextra)
library(gplots)
t<-table(all$violation_code,all$grade)
res.ca <- CA(t, graph = FALSE)
fviz_ca_biplot(res.ca, repel = TRUE,title="Correspondence analysis Biplot for Grade v/s Violation")

all<-read.csv("C:/Users/Vogiral Shivani/Desktop/all.csv")
all<-select(all,-X)

records_2017<-all %>% count(activity_date)

