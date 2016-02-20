##################
# Amirvahab Fakhfouri #
# Assignment 3 ###########################################
### Q0
print("Amirvahab Fakhfouri")
print(1505020)
print("afakhfou@ucsc.edu")
##############################

### Q1
f<- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/flights.csv", stringsAsFactors = FALSE)
p<- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/planes.csv", stringsAsFactors = FALSE)
w<- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/weather.csv", stringsAsFactors = FALSE)
a<- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/airports.csv", stringsAsFactors = FALSE)
###
### Q2
w$date<-as.Date(w$date)
f$date<-as.Date(f$date)
###
### Q3
flights.2a<- subset(f,dest=="SFO"|dest=="OAK") #3508 obs.
summary(flights.2a$dest)
flights.2b<- subset(f,dep_delay>=60) #10474 obs.
summary(flights.2a$dest)
flights.2c<- subset(f,arr_delay>2*dep_delay) #70772 obs.
summary(flights.2a$dest)
###
### Q4
?dplyr::select
library(dplyr)
f.4a<-select(f, ends_with("delay"))
f.4b<-select(f, contains("delay"))
f.4c<-select(f, matches("delay"))
###
### Q5
library(dplyr)
f.5a<-arrange(f,desc(dep_delay))
?dplyr::arrange
for(i in 1:5){
  print(f.5a[i,])
}
##b
library(dplyr)

f.5b<-f %>% 
  arrange(desc(time))
for(i in 1:5) {
  print(f.5b[i,])
}
f.5b1<-f %>%
  mutate(
    catchup_time=(dep_delay-arr_delay)) %>% 
  arrange(desc(catchuptime)) %>%
  head(5)
###
### Q6
## a
library(dplyr)
f <-  f %>%
  mutate(speed = (dist/time)*60)
## b
################################
f$dep1<-strptime(f$dep,"%H%M")
f$arr1<-strptime(f$arr,"%H%M")
library(dplyr)
f<-f %>%
  mutate(made_up = (arr1-dep1))
f$dep1 <- NULL
f$arr1<- NULL

f<-f %>%
  arrange(desc(made_up))
for(i in 1:5) {
  print(f.6b[i,])
}
f.6b<-delta %>% 
  arrange(desc(delta)) %>%
  head(5)
## c
f.6c<-f %>%
  mutate(lost = made_up-time)
arrange(desc(lost))
for(i in 1:5) {
  print(f.6c[i,])
}
f.6c<-delta %>% 
  arrange(delta) %>%
  head(1)

#################################
### Q7

?dplyr::summarize
flights.7a<- f%>%
  group_by(carrier) %>%
  summarise( num.cancelled=sum(cancelled),
             total = n(),
             percent.cancelled.flights=num.cancelled/total,
             min_delta = min(made_up,na.rm=T), 
             first.Q = quantile(made_up,prob=c(0.25),na.rm=T),
             mean_delta=mean(made_up,na.rm=T),
             third.Q = quantile(made_up,prob=c(0.75),na.rm=T),
             median_delta=median(made_up,na.rm=T),
             max_delta = max(made_up,na.rm=T)
  )


flights.7a <- flights.7a %>%
  arrange(desc(percent.cancelled.flights))

print(flights.7a)
###
### b
day_delay <- f %>%
  filter(!is.na(dep_delay)) %>%
  group_by(date) %>%
  summarize(delay=mean(dep_delay),
            n= n()) %>%
  filter(n>10)


cat("basically with this command we 
    are creating the average daily delay and 
    the number of flihts that flied on that
    specific day by date")  
###
### Q8

?dplyr::lag
day_delay <- day_delay %>%
  arrange((date))

for(i in 2:nrow(day_delay)) {
  day_delay$lag[i-1]<-day_delay$delay[i]-day_delay$delay[i-1]
}

day_delay <- day_delay %>%
  arrange(desc(lag)) 
for(i in 1:5) {
  print(day_delay[i,])
}
###
### Q9
dest_delay <- f %>%
  filter(!is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarize(delay=mean(arr_delay),
            n= n()) %>%
  filter(n>10)
### a
airports<-a %>%
  select(iata,airport,city,state,lat,long) 
colnames(airports)[1]<-"dest"
colnames(airports)[2]<-"name" 

df.9a<-dest_delay %>%
  left_join(airports,by="dest")

df.9a %>%
  arrange(desc(delay)) %>%
  tbl_df
###
### b
df.9b<-dest_delay %>%
  inner_join(airports,by="dest")

df.9a %>%
  arrange(desc(delay)) %>%
  tbl_df
cat("Number of observations are not same")
###
### c
df.9c<-dest_delay %>%
  right_join(airports,by="dest")

cat("When we use right join there are some observation 
    in the second dataset that we do not have them in the 
    first on, So based on that we obseve som NA value on delay")
###
### d

df.9d<-dest_delay %>%
  full_join(airports,by="dest")

cat("observation are equal to 3378 and again we observe so 
    many NA based on the fact that there are some observation 
    on the second dataset that we do not have them on the first one")
###
### Q10
hourly_delay <- f %>%
  filter(!is.na(dep_delay)) %>%
  group_by(date,hour) %>%
  summarize(delay=mean(dep_delay),
            n= n()) %>%
  filter(n>10)

df.10<-hourly_delay %>%
  inner_join(w,by=c("date","hour")) 

df.10a<-df.10 %>%
  arrange(desc(delay)) %>%
  tbl_df
for(i in 1:5) {
  print(df.10a$conditions[i])
}
###
### Q11
### a
install.packages("tidyr")
library(tidyr)
require(tidyr)
require(dplyr)
df <- data.frame(treatment = c("a","b"),subject1 = c(3,4),subject2 = c(5,6))
df
df.1<-df %>% gather(treatment)
colnames(df.1)[2]<-"subject"
for(i in 1:nrow(df.1)) {
  ifelse(df.1$subject[i]=="subject1",df.1$subject[i]<-1,df.1$subject[i]<-2)
}
df.1 %>%
  select(subject,treatment,value)
###
### b
df.b <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value= c(3,4,5,6)
)
df.b
df.b1<-df.b %>% 
  spread(subject,value)
colnames(df.b1)[2]="subject1"
colnames(df.b1)[3]="subject2"
colnames(df.b1)[1]="treatment"
df.b1
###
### c
df.c<- data.frame(
  subject =c(1,2,3,4),
  demo =c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)
df.c
df.c1 <- df.c %>%
  separate(
    demo,
    c("sex","age","state")
  )
df.c1
###
### d
df.d <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)
df.d
df.d1 <- df.d %>%
  unite(
    demo,
    ...=sex,age,city
  )
df.d1






















