
#################
#Packages
#################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gt)
library(corrplot)
library(psych)
library(gridExtra)




#################
#Import Data
################
songs <-read_delim("songs.csv")
acoustics <-read_delim("acoustic_features.csv")
pop <-read_delim("song_pop.csv")
charts <-read_delim("song_chart.csv")




#################################
#Dataset Cleaning and Preparation
#################################

#Pop
sum(duplicated(pop[, 1])) #This tells us there are 4791 rows without unique song ids 
charts %>%
  filter(song_id== "00drgzklCYUPHnEDRi7ROJ") 
pop %>%  
  filter(song_id== "00drgzklCYUPHnEDRi7ROJ")
#So what i have understood is that if songs have been in charts over new year then they have been entered twice 
#Choosing select the first year. 
pop <- pop %>% 
  arrange(song_id, year) %>%  
  filter(duplicated(song_id) == FALSE) #Filtered pop songs removing 

pop <-pop[, c(1, 4)]

#songs 
songs <-songs %>% 
  select(song_id, popularity, explicit, song_type)

#Acoustics 
summary(acoustics)
acoustics <-na.omit(acoustics)
acoustics <-acoustics[, -c(3,5)]


#############################
#Binding and Merging Datasets
#############################

total_data <-merge(songs, acoustics, by= "song_id")
total_data <-merge(total_data, pop, by= "song_id")


##########################
#Preparation of Total Data 
##########################

#Recoding of categorical variable answers to 0 and 1 
total_data$explicit <-ifelse(total_data$explicit== TRUE, 1, 0) #If song is explicit is it given value 1 if it is clean it is given value 0
 #If song is popular it has value 1 if it is not it is given value 0
total_data$song_type <-ifelse(total_data$song_type == "Solo", 1, 0) #If song is Solo it has value 1 if it is a collaboration it is given value 0

total_data %>%  
  head(5) %>%  
  gt()


################################
#Creating Test and Training data
#################################

set.seed(1)
total_data <-total_data[sample(1:nrow(total_data)), ] #shuffle rows in data frame 
train_size <-0.7 #Set size of training set
training <- total_data[1:(train_size * nrow(total_data)), ] #Create training set
test <-total_data[(nrow(training)+1):nrow(total_data),] #Create test set




############################
#EDA- Descriptive Analysis 
##########################
continuous_training <- total_data %>% 
  select(popularity, duration_ms, acousticness,
         danceability,
         energy, instrumentalness, liveness, 
         loudness, valence, tempo, 
         year, speechiness)


Summary_stat <-data.frame(describe(continuous_training))
Summary_stat <- Summary_stat %>%  
  select(n, mean, median, min, max, range, skew, kurtosis) %>% 
  round(2)
rownames(Summary_stat) <- c("Popularity", "Duration (ms)", "Acousticness", "Danceability", 
                            "Energy", "Instrumentalness", "Liveness", "Loudness", "Valence", 
                            "Tempo", "Year", "Speechiness")
colnames(Summary_stat) <-c("n", "Mean", "Median", "Minnimum", "Maximum", "Range", "Skew", "Kurtosis")
gt(Summary_stat, 
   rownames_to_stub = TRUE)

(colnames(continuous_training))


p1<-ggplot(continuous_training, aes(x= popularity))+
  geom_histogram(fill= "#A62F32", col= "white")+
  theme_minimal()+
  labs(x= "Popularity Score", 
       y= "Frequency", 
       title= "Popularity Score Data Distribution")


p2 <-ggplot(continuous_training, aes(x= duration_ms))+
  geom_histogram(fill= "#296299", col= "white")+
  theme_minimal()+
  labs(x= "Duration (ms)", 
       y= "Frequency", 
       title= "Song Duration Data Distribution")


p3 <-ggplot(continuous_training, aes(x= acousticness))+
  geom_histogram(fill= "#296299", col= "white")+
  theme_minimal()+
  labs(x= "Acousticness", 
       y= "Frequency", 
       title= "Acousticness Data Distribution")

p4 <-ggplot(continuous_training, aes(x= danceability))+
  geom_histogram(fill= "#296299", col= "white")+
  theme_minimal()+
  labs(x= "Danceability", 
       y= "Frequency", 
       title= "Danceability Data Distribution")

p5 <-ggplot(continuous_training, aes(x= energy))+
  geom_histogram(fill= "#296299", col= "white")+
  theme_minimal()+
  labs(x= "Energy", 
       y= "Frequency", 
       title= "Energy Data Distribution")

p6 <-ggplot(continuous_training, aes(x= instrumentalness))+
  geom_histogram(fill= "#296299", col= "white")+
  theme_minimal()+
  labs(x= "log10 Instrumentallness", 
       y= "Frequency", 
       title= "Instrumentallness Data Distribution")+
  scale_x_log10()

p7 <-ggplot(continuous_training, aes(x= liveness))+
  geom_histogram(fill= "#296299", col= "white")+
  theme_minimal()+
  labs(x= "Liveness", 
       y= "Frequency", 
       title= "Liveness Data Distribution")

p8 <-ggplot(continuous_training, aes(x= loudness))+
  geom_histogram(fill= "#296299", col= "white")+
  theme_minimal()+
  labs(x= "Loudness", 
       y= "Frequency", 
       title= "Loudness Data Distribution")

p9 <-ggplot(continuous_training, aes(x= valence))+
  geom_histogram(fill= "#296299", col= "white")+
  theme_minimal()+
  labs(x= "Valence", 
       y= "Frequency", 
       title= "Valence Data Distribution")

p10 <-ggplot(continuous_training, aes(x= tempo))+
  geom_histogram(fill= "#296299", col= "white")+
  theme_minimal()+
  labs(x= "Tempo", 
       y= "Frequency", 
       title= "Tempo Data Distribution")

p11 <-ggplot(continuous_training, aes(x= year))+
  geom_histogram(fill= "#296299", col= "white")+
  theme_minimal()+
  labs(x= "Year", 
       y= "Frequency", 
       title= "Year Data Distribution")


p12 <-ggplot(continuous_training, aes(x= speechiness))+
  geom_histogram(col= "white", fill= "#296299")+
  theme_minimal()+
  labs(x= "Log10 Speechiness", 
       y= "Frequency", 
       title= "Speechiness Data Distribution")+
  scale_x_log10()
p12

p13 <-ggplot(training, aes(x= factor(explicit)))+
  geom_bar(fill= "#E79E83")+
  labs(x= "", 
       y= "Frequency", 
       title = "Explicitness Frequencies")+
  scale_x_discrete(breaks = c("0", "1"), labels = c("Not Explicit", "Explicit"))+
  theme_minimal()+
  theme(legend.position = "none")

p14<-ggplot(training, aes(x= factor(mode)))+
  geom_bar(fill= "#E79E83")+
  labs(x= "", 
       y= "Frequency", 
       title = "Modality Frequencies")+
  scale_x_discrete(breaks= c("0", "1"), labels= c("Minor", "Major"))+
  theme_minimal()


p16 <-ggplot(training, aes(x= factor(song_type)))+
  geom_bar(fill= "#E79E83")+
  labs(x= "", 
       y= "log10(Frequency)", 
       title = "Song Type Frequencies")+
  theme_minimal()+
  scale_x_discrete(breaks = c("0", "1"), labels = c("Collaboration", "Solo"))



grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p16)


#########################################
#EDA Variable Relationships Continuous 
########################################
cor_matrix <-cor(continuous_training, method= "spearman")
colnames(cor_matrix) <-c("Popularity", "Duration (ms)", "Acousticness","Danceability",
                         "Energy", 
                         "Instrumentalness", "Liveness", "Loudness", "Valence", 
                         "Tempo", "Year", "Speechiness")
rownames(cor_matrix) <-c("Popularity", "Duration (ms)", "Acousticness","Danceability",
                         "Energy", 
                         "Instrumentalness", "Liveness", "Loudness", "Valence", 
                         "Tempo", "Year", "Speechiness")

corrplot(cor_matrix, 
         method = "color", 
         addCoef.col = "black",
         number.cex = 0.9, 
         number.digits = 2,
         diag = FALSE, 
         cl.pos = "b", 
         tl.col = "black", 
         tl.cex = 1.2, 
         type = "lower")

corelation_values <-corr.test(continuous_training, method = "spearman")
corelation_values




#######################################
#EDA Variable Relationships Categorical
#######################################
#Explicitness- **Signifcant
ggplot(training %>% filter(explicit == 1), aes(x = popularity)) +
  geom_histogram(binwidth = 5, alpha= 0.7, colour= "black")+
  theme_bw()+
  labs(x= "Popularity", 
       y= "Frequency", 
       title= "Popularity Scores of Explicit Songs")
ggplot(training %>% filter(explicit == 0), aes(x = popularity)) +
  geom_histogram(binwidth = 5, alpha= 0.7, colour= "black")+
  theme_bw()+
  labs(x= "Popularity", 
       y= "Frequency", 
       title= "Popularity Scores of Non-Explicit Songs")
wilcox.test(popularity ~ factor(explicit), data= training) 

box_1 <-ggplot(training, aes(x= factor(explicit), y= popularity))+
  geom_boxplot(aes(fill = factor(explicit)), alpha= 0.9)+
  theme_bw()+
  labs(x= "Explicitness", 
       y= "Popularity Score", 
       title = "Popularity Score of Explict and Non-Explicit Songs")+
  theme(legend.position = "none")+
  scale_x_discrete(breaks = c("0", "1"), labels = c("Not Explicit", 
                                                    "Explicit"))+
  scale_fill_manual(values = c("1" = "#E79E83", "0" = "#296299"))+
  theme(plot.title = element_text(size = 17),
        axis.title = element_text(size= 14), 
        axis.text = element_text(size= 13))



#Song Types **Significant
ggplot(training %>% filter(song_type == 1), aes(x = popularity)) +
  geom_histogram(binwidth = 5, alpha= 0.7, colour= "black")+
  theme_bw()+
  labs(x= "Popularity", 
       y= "Frequency", 
       title= "Popularity Scores of Solo Songs")
ggplot(training %>% filter(song_type == 0), aes(x = popularity)) +
  geom_histogram(binwidth = 5, alpha= 0.7, colour= "black")+
  theme_bw()+
  labs(x= "Popularity", 
       y= "Frequency", 
       title= "Popularity Scores of Collaboration Songs")

wilcox.test(popularity ~ factor(song_type), data= training)

box_2 <-ggplot(training, aes(x= factor(song_type), y= popularity))+
  geom_boxplot(aes(fill = factor(song_type)), alpha= 0.9)+
  theme_bw()+
  labs(x= "Song Type", 
       y= "Popularity Score", 
       title = "Popularity Score of Solo and Collaboration Song Types")+
  theme(legend.position = "none")+
  scale_x_discrete(breaks = c("0", "1"), labels = c("Collaboration", "Solo"))+
  scale_fill_manual(values = c("1" = "#E79E83", "0" = "#296299"))+
  theme(plot.title = element_text(size = 17),
        axis.title = element_text(size= 14), 
        axis.text = element_text(size= 13))




#Mode **Significant
ggplot(training %>%  
         filter(mode== 1), aes(x= popularity))+
  geom_histogram(binwidth = 5, alpha=0.7, colour= "black")+
  theme_bw()+
  labs(x= "Popularity", 
       y= "Frequency", 
       title= "Popualrity Score Distribution of Major Songs")+

ggplot(training %>%  
         filter(mode== 0), aes(x= popularity))+
  geom_histogram(binwidth = 5, alpha=0.9, colour= "black")+
  theme_bw()+
  labs(x= "Popularity", 
       y= "Frequency", 
       title= "Popualrity Score Distribution of Minor Songs")

wilcox.test(popularity ~ factor(mode), data= training)

box_3 <-ggplot(training, aes(x= factor(mode), y= popularity))+
  geom_boxplot(aes(fill= factor(mode)), alpha= 0.9)+
  theme_bw()+
  labs(x= "Mode",
       y= "Popularity Score", 
       title= "Popularity Scores of Songs with Major and Minor Modes")+
  theme(legend.position = "none")+
  scale_x_discrete(breaks= c("0", "1"), labels= c("Minor", "Major"))+
  scale_fill_manual(values = c("1" = "#E79E83", "0" = "#296299"))+
  theme(plot.title = element_text(size = 17),
        axis.title = element_text(size= 14), 
        axis.text = element_text(size= 13))

box_3
grid.arrange(box_1, box_2, box_3, ncol= 3)

#################################
#Multivariable Linear Regression 
#################################

md <-lm(popularity ~ duration_ms + acousticness + danceability +
          energy + liveness + loudness + valence + tempo + year +
          speechiness + explicit + mode + song_type + instrumentalness, 
        data = training)


#Forward Selection
md_coef_only <-lm(popularity~ 1, data = training)
forward_selection_md <-step(md_coef_only, direction = "forward", 
                            scope= formula(md))
summary(forward_selection_md)
coef(forward_selection_md)

#Backward Selection
backward_selection <-step(md, direction = "backward")

summary(backward_selection)
summary(md)


##################################################
#Multivariable Linear Regression Model Assumptions
##################################################

#Multicolinearity
car::vif(forward_selection_md) 

#Normlaity of residuals and homoscedacity 
md_resid <-forward_selection_md$residuals
md_fitted <- forward_selection_md$fitted.values

residual_plot_values <-cbind(md_resid, md_fitted)
residual_plot_values <-data.frame(residual_plot_values)

esidual_distribition_plot <-ggplot(residual_plot_values, aes(x= md_resid))+
  geom_histogram(aes(y= after_stat(density)), colour= "white", fill= "darkgrey", 
                 bins= 50)+
  theme_minimal()+
  labs(x= "Residuals", 
       y= "Density", 
       title= "Distribution of Residuals", 
       tag= "A")+
  stat_function(fun = dnorm, args = list(mean=mean(residual_plot_values$md_resid), 
                                         sd=sd(residual_plot_values$md_resid)), 
                colour= "black", size= 1.5)+
  theme(plot.title = element_text(size = 15), 
        plot.tag = element_text(size = 13), 
        axis.title = element_text(size= 13), 
        axis.text = element_text(size= 13))
esidual_distribition_plot

residual_vs_fitted_plot <-ggplot(residual_plot_values, aes(x= md_fitted, y= md_resid))+
  geom_point(col= "black", alpha= 0.4)+
  theme_minimal()+
  labs(x= "Fitted values",
       y= "Residual values", 
       title= "Residual vs Fitted Values", 
       tag = "B")+
  theme(plot.title = element_text(size = 15), 
        plot.tag = element_text(size = 13), 
        axis.title = element_text(size= 13), 
        axis.text = element_text(size= 13))
residual_vs_fitted_plot

grid.arrange(esidual_distribition_plot, residual_vs_fitted_plot)

#Oultiers 
training_dependents <- training %>%  
  select(explicit, duration_ms, mode, 
         acousticness, danceability, instrumentalness, 
         liveness, loudness, year, tempo, song_type)
mahalanobis_distances <- mahalanobis(training_dependents, center = colMeans(training_dependents), 
                                     cov = cov(training_dependents))
mahalanobis_distances <-data.frame(mahalanobis_distances)
mahalanobis_distances$p_values <-pchisq(mahalanobis_distances$mahalanobis_distances, df= ncol(training_dependents)-1)
training_with_mahal <-cbind(training, mahalanobis_distances)

training_no_outliers <- training_with_mahal %>%  
  filter(p_values >0.05)
nrow(training)- nrow(training_no_outliers) #1513 outliers 


###################
#Model Evaluation 
###################

stepwise_test <-test
stepwise_test$predicted_values <-predict(forward_selection_md, newdata = stepwise_test)
stepwise_test$residals <-stepwise_test$predicted_values- stepwise_test$popularity
sum(stepwise_test$residals**2) #2,143,535

md_test <-test
md_test$predicted_values <-predict(md, newdata = md_test)
md_test$residuals <-md_test$predicted_values - md_test$popularity
sum(md_test$residuals**2) #2,143,147
