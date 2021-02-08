library(stringr)
library(tidyverse)
library(caret)
library(tidyverse)
library(ggthemes)
library(lubridate)
memory.limit(60000)

#taking a glimpse at the Dataset:
head(edx)
head(edx)
glimpse(edx)



#Converting the timestamp to year and Extracting the Realease Year from the Title

edx_A<- edx%>% mutate(year_released=(str_extract(title,"\\s\\(\\d{4}\\)")))%>%
  mutate(year_released= str_replace(year_released,"\\s\\((\\d{4})\\)","\\1")) %>%
  mutate(year_rated=(year(as_datetime(timestamp)))) %>%
  mutate(movie_age_on_rating=as.numeric(year_rated)-as.numeric(year_released))
head(edx_A)

#Doing the same for the Validation set
validation_A<- validation%>% mutate(year_released=(str_extract(title,"\\s\\(\\d{4}\\)")))%>%
  mutate(year_released= str_replace(year_released,"\\s\\((\\d{4})\\)","\\1"))%>%
  mutate(year_rated=(year(as_datetime(timestamp))))%>%
  mutate(movie_age_on_rating=as.numeric(year_rated)-as.numeric(year_released))


head(validation_2)


##TO BE DELETED:
#edx_1 <- edx%>% mutate(year_rated=(year(as_datetime(timestamp))))

#Extracting Year from title 

#edx_2<-edx_1%>% mutate(year_released=(str_extract(title,"\\s\\(\\d{4}\\)")))%>%
# mutate(year_released= str_replace(year_released,"\\s\\((\\d{4})\\)","\\1"))


head(edx_A)

#How sparse is our data set?

users100 <- sample(unique(edx$userId),100)

edx%>%filter(userId %in% users100) %>%
  select(userId,movieId,rating) %>%
  mutate(rating=1) %>%
  spread(movieId,rating) %>% select(sample(ncol(.),100)) %>%
  as.matrix() %>% t(.) %>%
  image(1:100,1:100,.,xlab="Movies",ylab="Users") %>%
  abline (h=0:100+0.5,v=0:100+0.5,col="grey")

#How many RATINGS does each title have?
mean((edx_A%>% count(title))$n)

edx_A%>% count(title)%>%
  ggplot(aes(n))+
  geom_histogram(bins=30,color="black") +geom_vline(xintercept = 843,col='red',linetype="dashed")+
  scale_x_log10()+ ggtitle("Number of Movie Ratings")+theme_economist_white()


#How many ratings does each user give?
mean((edx_A%>% count(userId))$n)
edx_A%>% count(userId)%>% ggplot(aes(n))+
  geom_histogram(bins=30,color="black") + geom_vline(xintercept = 129, col = "red", linetype = "dashed")+
  scale_x_log10()+ ggtitle("Number of User Ratings")+theme_economist_white()

#Movie ratings distribution
ratings_plot<- ggplot(edx_A, aes(rating)) + geom_histogram(color = "black",binwidth = 0.2) + scale_x_continuous(breaks = seq(0.5, 5, 0.5)) + geom_vline(xintercept = mean(edx$rating), col = "red", linetype = "dashed") + labs(title = "Distribution of ratings",  x = "Ratings Scale", y = "Sum Of Rating")+ theme_economist_white()


#Loss Function RMSE= 


RMSE<- function(true_ratings,predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2))
}

#Data partition
set.seed(1,sample.kind="Rounding")

partindex<- createDataPartition(edx_A$rating,p=0.1,list=FALSE)

train_set<-edx_A[-partindex,]
test_set<-edx_A[partindex,]

#we also remove all the movies from the test set that do not appear in the train set
test_set<- test_set %>%  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Method: Just the Average  Y= mu_hat + e
#Computation of Average movie rating
mu <- mean(train_set$rating)
mu

naive_rmse<-RMSE(test_set$rating,mu)

RMSE_by_method <- data_frame(method = "Just the average", RMSE = round(naive_rmse,digits=3))


#Average rating for each movie Y= mu_hat + movie bias + e
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))


qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))+theme_economist_white()

test_set<- test_set %>% 
  left_join(movie_avgs, by='movieId')


predicted_ratings <- mu + test_set %>%
  pull(b_i)



model_1_rmse <- RMSE(test_set$rating,predicted_ratings)
RMSE_by_method<-bind_rows(RMSE_by_method,data_frame(method="Movie Effect Model",RMSE=model_1_rmse))
#####Average User Rating. User Effect.  Y= mu + bi + bu + error
train_set<- train_set%>% mutate(mu= mu) %>% left_join(movie_avgs,by='movieId') 
user_avgs <-train_set %>% group_by(userId)%>%
  summarise(b_u= mean(rating-mu-b_i))

test_set<- test_set %>% 
  left_join(user_avgs, by='userId')

head(test_set)

predicted_ratings <- mu + test_set %>%
  pull(b_i) + test_set %>%
  pull(b_u)


model_2_rmse <- RMSE(test_set$rating,predicted_ratings)
RMSE_by_method<-bind_rows(RMSE_by_method,data_frame(method="Movie Effect & User Effect Model",RMSE=model_2_rmse))

#Exploring the effect of premier date in rating
edx_A%>% group_by(year_released) %>%  summarise(rating=mean(rating))%>% 
  ggplot(aes(year_released,rating)) + geom_point() +theme_economist_white()+theme(axis.text.x=element_text(angle=90,hjust =1))

#It seems that the data follow a pattern, movies after the 80s have received a lower average rating. 
#While, movies released in the 30s and 70s have a better average rating.

train_set<- train_set%>%  left_join(user_avgs,by='userId') 
head(train_set)

release_year_avgs <-train_set %>% group_by(year_released)%>%
  summarise(b_ry= mean(rating-mu-b_i-b_u))

#Plot of the year and the average rating
release_year_avgs%>% ggplot(aes(reorder(year_released,b_ry),b_ry)) + geom_bar(stat = 'identity') +coord_flip()

release_year_avgs%>% ggplot(aes(year(Sys.Date())-year_released,b_ry) + geom_point() +geom_smooth(method='lm',formula=y~x))

#implementing year averages in the test set

test_set<- test_set%>% 
  left_join(release_year_avgs, by='year_released')
head(test_set)

predicted_ratings <- mu + test_set %>%
  pull(b_i) + test_set %>%
  pull(b_u) + test_set %>%pull(b_ry)

model_3_rmse <- RMSE(test_set$rating,predicted_ratings)
RMSE_by_method<-bind_rows(RMSE_by_method,data_frame(method="Movie Effect & User Effect & Release Year Effect",RMSE=model_3_rmse))


###Exploring movie age on rating date effect.
head(train_set) 

train_set<- train_set%>% mutate(mu= mu) %>% left_join(release_year_avgs,by='year_released') 


movie_age_avgs <-train_set %>% group_by(movie_age_on_rating)%>%
  summarise(b_ma= mean(rating-mu-b_i-b_u-b_ry))

head(movie_age_avgs)
movie_age_avgs%>% ggplot(aes(movie_age_on_rating,b_ma))+geom_point()+geom_smooth(method='loess',formula=y~x) +ggtitle('Effect of movie age at the time of rating')
+theme_economist_white()


#implementation on test set
test_set<- test_set %>% 
  left_join(movie_age_avgs, by='movie_age_on_rating')
head(test_set) 



predicted_ratings <- mu + test_set %>%
  pull(b_i) + test_set %>%
  pull(b_u) + test_set%>%pull(b_ry) +test_set%>%pull(b_ma)

model_4_rmse <- RMSE(test_set$rating,predicted_ratings)
RMSE_by_method<-bind_rows(RMSE_by_method,data_frame(method="Movie Effect & User Effect & Release Year Effect  & Movie age on rating Effect",RMSE=model_4_rmse))

train_set<- train_set %>% 
  left_join(movie_age_avgs, by='movie_age_on_rating')
head(train_set)

#Exploring how we can improve our prediction. Where do we have the bigest residual?
test_set%>%
  mutate(residual= rating-(mu+b_i+b_u+b_ry),prediction=mu+b_i+b_u+b_ry) %>%
  arrange(desc(residual)) %>%
  select(title,residual,prediction,rating) %>% slice(1:10) %>% knitr::kable()

#First make a table with the distinct movies and their respective bi from movie avgs
distinct_movies  <- edx %>% 
  select(movieId, title) %>%
  distinct()

movie_avgs%>%left_join(distinct_movies,by='movieId')%>%arrange(desc(b_i)) %>% select(title,b_i) %>% slice(1:10)%>%knitr::kable()

movie_avgs%>%left_join(distinct_movies,by='movieId')%>%arrange(b_i) %>% select(title,b_i) %>% slice(1:10)%>%knitr::kable()

train_set %>% dplyr::count(movieId)%>% left_join(movie_avgs)%>%left_join(distinct_movies,by='movieId')%>%arrange(desc(b_i)) %>% select(title,b_i,n) %>% slice(1:10)%>%knitr::kable()

train_set %>% dplyr::count(movieId)%>% left_join(movie_avgs)%>%left_join(distinct_movies,by='movieId')%>%arrange(b_i) %>% select(title,b_i,n) %>% slice(1:10)%>%knitr::kable()

train_set %>% dplyr::count(movieId)%>% summarise(mean=mean(n))

#Second make a table with the distinct users and their respective bu from user avgs
distinct_users  <- edx %>% 
  select(userId) %>%
  distinct()

train_set %>% dplyr::count(userId)%>% left_join(user_avgs)%>%left_join(distinct_users,by='userId')%>%arrange(b_u) %>% select(userId,b_u,n) %>% slice(1:10)%>%knitr::kable()
train_set %>% dplyr::count(userId)%>% left_join(user_avgs)%>%left_join(distinct_users,by='userId')%>%arrange(desc(b_u)) %>% select(userId,b_u,n) %>% slice(1:10)%>%knitr::kable()

train_set %>% dplyr::count(userId)%>% summarise(mean=mean(n))
# Know that every user gives on average 116 ratings, so the top and worst average user effects come from users with few ratings

#Third make a tabe with the distict years and their respective b_yr avgs
distinct_years  <- edx_A %>% 
  select(year_released) %>%distinct()

train_set %>% dplyr::count(year_released)%>% left_join(release_year_avgs)%>%left_join(distinct_years,by='year_released')%>%arrange(b_ry) %>% select(year_released,b_ry,n) %>% slice(1:10)%>%knitr::kable()
train_set %>% dplyr::count(year_released)%>% left_join(release_year_avgs)%>%left_join(distinct_years,by='year_released')%>%arrange(desc(b_ry)) %>% select(year_released,b_ry,n) %>% slice(1:10)%>%knitr::kable()

#How many ratings is the average for each year?
train_set%>%dplyr::count(year_released)%>% summarise(mean=mean(n))


#Third make a tabe with the distict years and their respective b_ma avgs
distinct_movie_ages  <- edx_A %>% 
  select(movie_age_on_rating) %>%distinct()%>% arrange(movie_age_on_rating)



train_set %>% dplyr::count(movie_age_on_rating)%>% left_join(movie_age_avgs)%>%left_join(distinct_movie_ages,by='movie_age_on_rating')%>%arrange(b_ma) %>% select(movie_age_on_rating,b_ma,n) %>% slice(1:10)%>%knitr::kable()
train_set %>% dplyr::count(movie_age_on_rating)%>% left_join(movie_age_avgs)%>%left_join(distinct_movie_ages,by='movie_age_on_rating')%>%arrange(desc(b_ma)) %>% select(movie_age_on_rating,b_ma,n) %>% slice(1:10)%>%knitr::kable()

#How many ratings is the average for each year?
train_set%>%dplyr::count(movie_age_on_rating)%>% summarise(mean=mean(n))



#For all these explore the top 10 movies according to the respective bias 
#and the number of ratings that occured for these movies. Notice that the ones with the highest ratings have a small number of ratings.
#For this reason we need to regularize, using different lambdas and cross validation to choose the best one


lambda<-1.5
movie_avgs_reg<- train_set %>% group_by(movieId) %>% summarize (b_i_reg = sum(rating-mu)/(n()+lambda),n_i=n())
head(movie_avgs_reg)

data_frame(orig=movie_avgs$b_i,reg=movie_avgs_reg$b_i_reg, n= 
             movie_avgs_reg$n_i) %>%   ggplot(aes(orig, reg, size=sqrt(n))) +  geom_point(shape=1, alpha=0.5)

train_set<- train_set %>% left_join(movie_avgs_reg,by="movieId")

head(train_set)
train_set%>% group_by("title") %>%arrange(desc(b_i_reg)) %>% select(title,b_i_reg,b_i,n_i)%>% distinct()%>% slice(1:10)

test_set <- test_set %>% left_join(movie_avgs_reg, by='movieId') 
predicted_ratings_reg_movie<- mu+test_set%>%pull(b_i_reg)


model_5_rmse <- RMSE(test_set$rating,predicted_ratings_reg_movie)
RMSE_by_method<-bind_rows(RMSE_by_method,data_frame(method="Movie Effect Regularized",RMSE=model_5_rmse))



##Rgularize Movie effects to show that it helps to regularize



#####Regularization for movie, user and release year effects. 
##Cross Validation.

lambdas<- seq (0,10,0.5)
mu <- mean(train_set$rating)

rmses <- sapply(lambdas, function(l){
  b_i_r <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i_r = sum(rating - mu)/(n()+l))
  
  predicted_ratings <- test_set %>% 
    left_join(b_i_r, by='movieId') %>% 
    mutate(pred = mu + b_i_r) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

plot(lambdas,rmses)
data_frame(lambdas=lambdas, rmses=rmses)[which.min(rmses),]
min(rmses)
model_1_rmse


lambdas<- seq (0,10,0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  
  b_i_r <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i_r = sum(rating - mu)/(n()+l))
  
  b_u_r <- train_set %>% left_join(b_i_r,by="movieId")%>%
    group_by(userId) %>% 
    summarize(b_u_r = sum(rating - mu-b_i_r)/(n()+l))
  
  b_ry_r <- train_set %>% left_join(b_u_r,by="userId")%>%
    left_join(b_i_r,by="movieId")%>%
    group_by(year_released) %>% 
    summarize(b_ry_r = sum(rating - mu-b_i_r-b_u_r)/(n()+l))
  
 # b_ma_r <-train_set%>%
  #  left_join(b_u_r,by="userId")%>%
   # left_join(b_i_r,by="movieId")%>%
    #left_join(b_ry_r,by='year_released') %>%
    #group_by(movie_age_on_rating) %>% 
    #summarize(b_ma_r = sum(rating - mu-b_i_r-b_u_r-b_ry_r)/(n()+l))
  
  
  predicted_ratings <- test_set %>% 
    left_join(b_i_r,by="movieId") %>%
    left_join(b_u_r,by="userId") %>% 
    left_join(b_ry_r,by="year_released")%>%  
    #left_join(b_ma_r,by='movie_age_on_rating')%>%
    mutate(pred = mu + b_i_r+b_u_r+b_ry_r)%>% .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})
plot(lambdas,rmses)
data.frame(lambdas,rmses)[which.min(rmses),]



model_5_rmse <- data.frame(rmses)[which.min(rmses),]
RMSE_by_method<-bind_rows(RMSE_by_method,data_frame(method="Regularized Movie Effect & User Effect & Release Year Effect",RMSE=model_5_rmse))
train_set<- train_set%>% mutate(mu= mu) %>% left_join(release_year_avgs,by='year_released') 

RMSE_by_method%>%knitr::kable()
rmse_results
#Once we regularize we implement it to the model

l<- 4.75


mu <- mean(edx_A$rating)

b_i_r <- edx_A %>% 
  group_by(movieId) %>% 
  summarize(b_i_r = sum(rating - mu)/(n()+l))

b_u_r <- edx_A %>% left_join(b_i_r,by="movieId")%>%
  group_by(userId) %>% 
  summarize(b_u_r = sum(rating - mu-b_i_r)/(n()+l))

b_ry_r <- edx_A %>% left_join(b_u_r,by="userId")%>%
  left_join(b_i_r,by="movieId")%>%
  group_by(year_released) %>% 
  summarize(b_ry_r = sum(rating - mu-b_i_r-b_u_r)/(n()+l))

#b_ma_r <-edx_A%>%
#  left_join(b_u_r,by="userId")%>%
#  left_join(b_i_r,by="movieId")%>%
#  left_join(b_ry_r,by='year_released') %>%
#  group_by(movie_age_on_rating) %>% 
#  summarize(b_ma_r = sum(rating - mu-b_i_r-b_u_r-b_ry_r)/(n()+l))


predicted_ratings <- validation_A %>% 
  left_join(b_i_r,by="movieId") %>%
  left_join(b_u_r,by="userId") %>% 
  left_join(b_ry_r,by="year_released")%>%  
#  left_join(b_ma_r,by='movie_age_on_rating')%>%
  mutate(pred = mu + b_i_r+b_u_r+b_ry_r)%>% .$pred



FinalRMSE <- RMSE(validation$rating,predicted_ratings)

#Once it is implemented our model is ready to be used in the validation set and get the final RMSE.


