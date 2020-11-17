library(tidyverse)
library(readxl)
library(ggplot2)

df<-tibble(read.csv("train.csv"))
df$Survived<-as.factor(df$Survived)
df$Pclass<-as.factor(df$Pclass)

df %>% group_by(Pclass,Sex,Survived) %>% count() %>% 
  ggplot(aes(x=Pclass,y=n,fill=Sex))+
  geom_col()+
  facet_wrap(~Survived)


x<-c(1,-4,3,2,10)
if(x<1){
  print("the statement is true")
}else{
  print("the statement is false")
}


x<-c(1,-4,3,2,10)
ifelse(x<1,"yes",ifelse(x<3,"less than 3","not true"))

df$Age[which(is.na(df$Age))]=0

#to find missing numbers
which(is.na(df$Sex))

x<-df$Age
df %>% mutate(cat_age=ifelse(x<10,0,ifelse(x<20,1,ifelse(x<30,2,ifelse(x>=30,3,100)))))
  
#to make male and female 0 and 1  
df_new<-df %>% mutate(cat_Sex=ifelse(df$Sex=="male",0,1))  
#--------------------------------------------------------------------------------------  

#along column
seq_along(df)
seq_along(df$Survived)
#seq_along=1:length
#loop
for(i in seq_along(df$Survived)){
  print(i)
}
#_______________________________________________________________________________________

#def function(): in python

square_x<-function(x,y){
  return (x*y)
}

square_x(3,4)
#____________________________________________________________________________________
df_new<-df %>% select(Age,SibSp,Parch)

#na.rm=TRUE for any missing value

#for average in each coloumn
mean(df_new$Parch,na.rm=TRUE)

#mean for more than one column
apply(df_new,2,mean,na.rm=TRUE)

df_new<-df %>% mutate(mean=apply(df_new,1,mean,na.rm=TRUE))
