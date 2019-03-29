data<-read.csv('/Users/yanghaoying/Desktop/project/HighNote Data Midterm.csv', header = T)
data_adopter<-read.csv('/Users/yanghaoying/Desktop/adopter.csv', header = T)
data_non_adopter<-read.csv('/Users/yanghaoying/Desktop/non_adopter.csv', header = T)

library(dplyr)
library(pastecs)
-------------------------------------------------------------------------------------
library(pastecs)
data_adopter<-filter(data,data$adopter==1)
data_non_adopter<-filter(data,data$adopter==0)
s1<-stat.desc(data_adopter)
s2<-stat.desc(data_non_adopter)
setwd('/Users/yanghaoying/Desktop/')
write.csv(s1,'s1.csv')
write.csv(s2,'s2.csv')
--------------------------------------------------------------------------------------
#boxplot
library(ggplot2)
data$adopter<-as.character(data$adopter)

ggplot(data, aes(x=adopter, y=age))+geom_boxplot()
ggplot(data, aes(x=adopter, y=friend_cnt))+geom_boxplot()+ylim(c(0,50))
ggplot(data, aes(x=adopter, y=avg_friend_age))+geom_boxplot()
ggplot(data, aes(x=adopter, y=avg_friend_male))+geom_boxplot()
ggplot(data, aes(x=adopter, y=friend_country_cnt))+geom_boxplot()+ylim(c(0,20))
ggplot(data, aes(x=adopter, y=subscriber_friend_cnt))+geom_boxplot()+ylim(c(0,150))
ggplot(data, aes(x=adopter, y=songsListened))+geom_boxplot()
ggplot(data, aes(x=adopter, y=posts))+geom_boxplot()+ylim(c(0,50))
ggplot(data, aes(x=adopter, y=playlists))+geom_boxplot()+ylim(c(0,50))
ggplot(data, aes(x=adopter, y=shouts))+geom_boxplot()+ylim(c(0,50))
ggplot(data, aes(x=adopter, y=tenure))+geom_boxplot()
ggplot(data, aes(x=adopter, y=lovedTracks))+geom_boxplot()+ylim(c(0,4000))


data$adopter<-as.numeric(data$adopter)

------------------------------------------------------------------------------------------
#create new variable called subscriber_friend
data<-mutate(data, subscriber_friend=ifelse(subscriber_friend_cnt==0, 0, 1))
# Pre-analysis using non-matched data
t.test(data$subscriber_friend,data$adopter)

cov <- c('age','male', 'friend_cnt',	'avg_friend_age', 'avg_friend_male',	
         'friend_country_cnt', 'songsListened',	'lovedTracks',
         'posts',	'playlists',	'shouts',	'tenure',	'good_country')

lapply(cov, function(v) {
  t.test(data[, v] ~ data$subscriber_friend)
})

r<-glm(subscriber_friend~age+male+friend_cnt+avg_friend_age+avg_friend_male+
         friend_country_cnt+songsListened+lovedTracks+posts+playlists+
         shouts+tenure+good_country,
       family = binomial(), data = data)
summary(r)
#for variables 'male', 'playlists', 'shouts', 'good_country'are insignificant
r1<-glm(subscriber_friend~age+friend_cnt+avg_friend_age+avg_friend_male+
         friend_country_cnt+songsListened+lovedTracks+tenure+posts,
       family = binomial(), data = data)
summary(r1)

# Using this model, we can now calculate the propensity score for each student. 
prs_df <- data.frame(pr_score = predict(r1, type = "response"),
                     subscriber_friend=r$model$subscriber_friend)
head(prs_df)
head(r$model)
score1<-mean(prs_df$pr_score[prs_df$subscriber_friend==1])
score2<-mean(prs_df$pr_score[prs_df$subscriber_friend==0])

# eliminate missing values
data_nomiss <- data %>%  
  select(adopter,subscriber_friend_cnt, subscriber_friend,one_of(cov)) %>%
  na.omit()
#matchit
mod_match <- matchit(subscriber_friend ~ age+male+friend_cnt+avg_friend_age+avg_friend_male+
                       friend_country_cnt+songsListened+lovedTracks+posts+playlists+
                       shouts+tenure+good_country, 
                     method = "nearest", data = data_nomiss)
summary(mod_match)
plot(mod_match)
# To create a dataframe containing only the matched observations.
dta_m <- match.data(mod_match)
dim(dta_m)
# Difference of means
dta_m %>%
  group_by(subscriber_friend) %>%
  select(one_of(cov)) %>%
  summarise_all(funs(mean))

lapply(cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$subscriber_friend)
})
# Estimating treatment effects
t.test(dta_m$subscriber_friend ~ dta_m$adopter)
r_sf<-lm(adopter~subscriber_friend, data = dta_m)
summary(r_sf)
------------------------------------------------------------------------------------------
#regression
summary(regression<- glm(adopter~age+male+friend_cnt+avg_friend_age+avg_friend_male+
                           friend_country_cnt+songsListened+lovedTracks+posts+playlists+
                           shouts+tenure+good_country+subscriber_friend_cnt+subscriber_friend,
                 family = binomial(), data = data))
# posts,shouts and avg_friend_male are insignificant. 
summary(regression<- glm(adopter~age+male+friend_cnt+avg_friend_age+friend_country_cnt+
                          songsListened+lovedTracks+playlists+
                          tenure+good_country+subscriber_friend_cnt+subscriber_friend,
                          family = binomial(), data = data))

#Report the correlations among the all variables.
s_data<-subset(data,select=c(age,male,friend_cnt,avg_friend_age,
                             friend_country_cnt,songsListened,lovedTracks,playlists,
                             tenure,good_country,subscriber_friend_cnt,subscriber_friend))
M<-cor(s_data)
library('corrplot') 
corrplot(M, method = "circle") 
#subscriber_friend_cnt, friend_cnt and friend_country_cnt are highly correlated,
#so I only keep subscriber_friend_cnt.
#age and avg_friend_age are highly correlated, so I only keep age. 
#after simplify variables, we do regression again.
regression<- glm(adopter~age+male+songsListened+lovedTracks+playlists+
                           tenure+good_country+subscriber_friend_cnt+subscriber_friend,
                         family = binomial(), data = data)
summary(regression)

odds_ratio<-data.frame(exp(regression$coefficients))
odds_ratio


