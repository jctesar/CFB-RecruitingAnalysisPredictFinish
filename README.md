# CFB-RecruitingAnalysisPredictFinish
## Test Question
It is well known that college recruiting will make a large impact on the success of a college football team. The best college football programs will sign (meaning the recruit accepts the scholarship and attends the school) the top high school recruits and most-often continue to have the most success on the field. But each and every year there are teams that do not recuit at the highest level and have success on the field, or vice versa where a team recruits at a very high level and does not achieve as much success as expected on the field. This is not a debate of whether high school recruiting matters to on-field succes, but rather HOW MUCH does high school recruiting correlate to on-field success at the college level. 

To conduct this study I am relying on two key data sets that are widely regarded and highly respected for those familiar with college football. The first is 247.com, arguably the leading website in tracking high school recruiting and compiling 'talent levels' of college programs based on their level of high school recruiting. Each season 247.com will post a talent rank for the upcoming season (listed as the variable 'talent_rank' in the data) which will use the roster for the upcoming season and include the evaluation level of each recruit in their high school senior year. The rosters will be ranked using their own formula, which for this study I decided not use the specific number produced from the ranking but rather the ranking itself listed in descending order (1 is the most talented, 2 the second, and so on...). This is not a perfect evaluation..for example since it uses the evaluation from the high school level it does not take into account their production/evaluation at the college level. An incoming freshman may be ranked equally as high as an upcoming senior who is likely to contribute much more to the team in the upcoming season. But I have made the assumption that overall we will proceed that each player has the ability to equally contribute to team success (think of the saying "best players will play").

The second key data set I am using is also highly regarded in the college football world, which is the S&P ratings produced by sports analyst Jeff Sagarin. His rankings will use several unique formulas that go beyond records and standard media rankings, such as including strength of schedule, close wins v large wins, tempo of offense and many more. At the end of the season a final set of S&P ratings are produced that rank all Division 1 football teams using the key formulas. This practice is very well respected in college football and are considered to me far more accurate than the popular coaching or media rankings produced (who are ofter biased to teams with the most coverage, star players, over-empasizing final records, etc...). 

For this study I can look in hindsight on past seasons on the 'talent ranking' of a team going into the season based on 247.com and match it with the S&P rating at the end of the respective season. The current set includes the talent level and S&P finish for the 2018 and 2019 seasons combined (I may add 2017 and beyond if I feel the sample size should increase). If you believe high school recruiting matters a lot, then the most talented teams will likely finish in relative order of the talent ratings. On the other hand, if you believe high school recruiting does not matter so much you would expect the talent and final results to not match up as well. The obvious answer is it will be somewhere in the middle, which is where my study comes into play. I will use several techniques, including linear regression, random forest modeling, machine learning among others to evaluate and provide specific results on the correlation of high school recruiting and on-field success in college football.   


## Load Data 

```{r}
library(readxl)
X2017_19_Team_Talent <- read_excel("C:/Users/Jack Tesar/Desktop/Personal Stats Projects/2017-19 Team Talent.xlsx")

# view for reference
# View(X2017_19_Team_Talent)
```


```{r}
data_orig <- X2017_19_Team_Talent
head(data_orig)

# copy data set to keep original unaltered
data1 <- data_orig
#nrow(data1)
#head(data1)
```

The data is first ranked by year in descending order, which includes the years 2017, 2018 and 2019. And next the variable 'talent_rank' is ordered in ascending order with 75 teams within each year, so we would expect a total amount of 225 rows. 

Since I compiled the data myself in Excel prior to loading into R I am familiar with the variables and some potential outliers that may be present. The first potential outlier I will explore is the 'sp_finish', since the final S&P Rankings include over 200 teams and there is the possibility there are teams that finish far below expected and may be considered extreme outliers. Although there could be a long list of reasons as to why a team finshes far below expected (rash of injuries, coaching changes, etc..), these are not reflected in the variables and can be removed if meeting the threshold of an extreme outlier. Another variable I will investigate is 'roster_size' since roster sizes are not the same for all teams and may have an impact when we manipulate the data with prortions, percentages etc..

## Explore Outliers

```{r}
## explore outliers of 'roster_size'

# first use boxplot as a visual if there are obvious outliers
boxplot(data1$roster_size,horizontal=TRUE,axes=TRUE,outline=FALSE)

# use summary to provide specific values for boxplot
summary(data1$roster_size)
nrow(data1)

# calculate IQR_roster (Q3-Q1)
iqr_roster <- (85-78)

# test if appropriate to remove outlier below Q1
78 - (iqr_roster)*1.5
```

Based on the visual of the boxplot there seems to be at least one exteme outlier on the 'lower' end. Based on our IQR calculations we can proceed with removing any values with a roster size less than 67.5. 

```{r}
data1 <- data1[which(data1$roster_size > 67.5),]

# confirm outlier has been removed
summary(data1$roster_size)
nrow(data1)
```

We successfully removed 1 row of data. We will replicate this process for the variable 'sp_finish'.  

```{r}
## explore outliers of 'sp_finish'

# first use boxplot as a visual if there are obvious outliers
boxplot(data1$sp_finish,horizontal=TRUE,axes=TRUE,outline=FALSE)

# use summary to provide specific values for boxplot
summary(data1$sp_finish)

# calculate IQR_roster (Q3-Q1)*1.5
iqr_sp <- (63.25-19.75)
63.25 + (iqr_sp)*1.5
```

```{r}
data1 <- data1[which(data1$sp_finish < 128.5),]

# confirm outlier has been removed
summary(data1$sp_finish)
nrow(data1)
```

We have removed 2 more rows based on our outlier testing. We can conduct alternate outlier testing by creating a dummy linear model and test if there are extreme outliers or leverage points remaining. 

```{r}
# create dummy linear model of 'talent_rank' vs 'sp_finish'
lm1 <- lm(sp_finish ~ talent_rank, data = data1)
dim(data1)

# create plot for visual 
plot(data1$talent_rank, data1$sp_finish, xlab = "Talent Rank", ylab = "S&P Finish")
abline(lm1)
```

There are no obvious extreme outliers. We can alternatively produce a plot using standardized residuals. 

```{r}
plot(data1$talent_rank, rstandard(lm1), xlab = "Talent Rank", ylab = "Standardized Residuals of S&P Finish")
abline(h=c(-2,2), lty=2)
```

As a general rule, in "smaller" data sets, points outside of +/- 2 standardized residuals may warrant investigation. There are ~ 10 points outside that qualify as outside +2, but I will make the determination that because the points are relatively spread across the X-Axis (Talent Rank) it is reasonable to include these points. We will proceed with the assumption no further action is needed at this time. 


## Add Columns on Key Items of Interest

Currently the data includes raw numbers and categorical entries. Manipulating the data to include proportions may prove to yield significant variables in our analysis. I Will include the percentage of the roster that are 5 star prospects, as well as at least 4 star (includes 4 and 5 star prospects) to provide two new variables and provide as a percentage of the roster. 

```{r}
# load tidyverse package
library(tidyverse)

# copy data 
data2 <- data1

# add new column for percent of five star recruit on roster
data2 <- data2 %>% 
  mutate(school, five_star_perc = (100*(five_stars / roster_size)))

# add new column for percent of roster that is at least 4 star (is 4 star or 5 star recruit)
data2 <- data2 %>% 
  mutate(school, four_star_min_perc = (100*(four_stars + five_stars) / roster_size))

# visual confirmation new columns have been included
head(data2)
```

## Linear regression analysis

Before we can proceed with our testing we must evaluate that a list of assumptions have been met. Critical assumptions include constant variance and a normal distribution (or normality) of the data set. We can evaluate our assumption of constant variance through a residual plot and our assumption of normality through a QQ-plot. We will begin by creating the linear model with all predictors included and evaluating the assumptions listed. 

```{r}
# create linear regression model to test various predictors on the variable 'sp_finish'
lm1.0 <- lm(sp_finish ~ talent_rank + five_stars + four_stars + three_stars + four_star_min_perc + five_star_perc, data = data2)

# residual plot to assess constant variance
plot(predict(lm1.0), resid(lm1.0))
abline(h=0)

# QQ-plot to assess normality
qqnorm(resid(lm1.0))
qqline(resid(lm1.0))
```

As it currently stands our assumptions for normality have not been met. We can see in the QQ-plot the variance is skewed to the right and there is notabe straying from the QQ-line, indicating the distribution is not normal. 

At this stage it would be worthwhile to explore whether to proceed with the study. In other words, is there enough correlation to support further investigating relationships within our explanatory and response variables even with skewed data. We will use a visual reference with a boxplot and analysis with the F-test to test whether there are relevant variables included. 

```{r}
# boxplot for reference
boxplot(sp_finish ~ talent_rank, xlab = "Talent Rank", ylab = "S&P Finish", main = "Boxplot of Talent Rank vs. S&P Finish", data = data2)
```

```{r}
# F-test to see if any predictors are "useful"
lm_null <- lm(sp_finish ~ 1, data = data2)
anova(lm_null, lm1.0)
```

From the boxplot we can see there does appear to be a relationship between the explanatory and response variables, most notably at the "lower" values (i.e. the higher ranking is more associated with a higher finish), while there is more variance and "noise" at the higher end of the data (or lower talent rating). In addition, our F-Test has shown there is at least one significant explanatory variable included in the model. For these reasons we will proceed with the assumptions that the data is "normal enough" and the variance is "constant enough" for our testing.  


```{r}
plot(data2$talent_rank, rstandard(lm1.0), xlab = "Talent Rank", ylab = "Standardized Residuals of S&P Finish")
abline(h=c(-2,2), lty=2)
```

I included a +/- of 2 on the residual plot as it is best-practice to remove values that exceed this amount. We can see from the plot there are several points above the +2 residuals. Since our sample size is sufficiently large we may proceed with removing these points. 

```{r}
# copy data set for residual work
data2_res <- data2

# store residuals as a new column
data2_res$res <- resid(lm1.0)

# calculate 2 standard deviations (i.e. 2 residuals)
sd2 <- 2*sd(resid(lm1.0))
sd2
# 2 standard deviations is ~50.9

# identify values that are above 2 residuals by adding new column and identifying as 1 if above, 0 otherwise
data2_res$outside <- ifelse(abs(data2_res$res)>sd2, 1, 0)

# note number of rows for reference
nrow(data2_res)

# plot 
plot(data2_res$res, col = data2_res$outside + 1, pch=16,  ylim = c(-3, 3))
```

```{r}
# copy dataset and include only values with standard residual < 2
data2_res_rm <- data2_res[!data2_res$outside, ]

# confirm rows have been removed
nrow(data2_res_rm)
# RESULT: 9 rows removed 

# plot 
plot(data2_res_rm$res, col = data2_res_rm$outside + 1, pch=16,  ylim = c(-3, 3))
```

```{r}
# copy lm1.0 using data set with residuals > 2 removed
lm1.0 <- lm(sp_finish ~ talent_rank + five_stars + four_stars + three_stars + four_star_min_perc + five_star_perc, data = data2_res_rm)

# plot for reference
plot(data2_res_rm$talent_rank, rstandard(lm1.0), xlab = "Talent Rank", ylab = "Standardized Residuals of S&P Finish")
abline(h=c(-2,2), lty=2)
```


```{r}
# create linear regression model to test various predictors on the variable 'sp_finish'
summary(lm1.0)
```

The P-values are very high and statistically insignificant, but there is a good chance many of the variables are highly correlated. We can correct this using a Variance Inflation Factor which will provide figures for high correlation between variables. As a rule of thumb a VIF over 5 is concerning and it would be best practice to remove variables from highest VIF to lowest in order (taking into account the respective P-values).

```{r}
# load farawy package
library(faraway)

# VIF on model "lm1" (round to 2 decimals)
round(vif(lm1.0), 2)
```

We will proceed with removing variables that are statistically significant and remove one by one. Since the variable 'five_stars' is highlight insignificant and carries the highest VIF I Will remove this variable first. I will continue this process until only significant variables remain and the VIF values are acceptable.   

```{r}
# duplicate linear model "lm1" but remove variable 'five_stars'
lm1.1 <- lm(sp_finish ~ talent_rank + four_stars + three_stars + four_star_min_perc + five_star_perc, data = data2)
summary(lm1.1)
```

The remaining variables are still insignificant but trending towards potential significance, an encouraging sign there may be significant variables included after removing the correlated and/or insignificant variables. 

```{r}
round(vif(lm1.1), 2)
```

As seen above the most insignificant  variable is talent_rank, but since there are other VIF values much higher, I will not remove this variable (yet). Instead I will remove 'four_star_min_prop' since it is insignificant and carries a very high VIF. 

```{r}
# remove 'four_stars'
lm1.2 <- lm(sp_finish ~ talent_rank + four_stars + three_stars + five_star_perc, data = data2)
summary(lm1.2)
```

```{r}
round(vif(lm1.2), 2)
```

Here we can see the VIF values have become much closer to acceptable, which is an encouraging sign. Now we can follow the process of removing the most insignificant variables based on the p-value knowing our risk of removing correlated variables is much lower. Now we will remove 'talent_rank' since it is the most statistically insignificant variable and there are no obvious violations in VIF. 

```{r}
# remove 'three_stars'
lm1.3 <- lm(sp_finish ~ four_stars + three_stars + five_star_perc, data = data2)
summary(lm1.3)
```

```{r}
round(vif(lm1.3), 2)
```

Alas our VIF values are all under 5, where we can confidently say there is very little correlation between the variables. We will continue to remove based on level of significance where we will remove 'five_star_perc.'

```{r}
# remove 'five_star_prop'
lm1.4 <- lm(sp_finish ~ four_stars + three_stars, data = data2)
summary(lm1.4)
```

Our level of significance for 'three_star' would be acceptable with a 10% Type 1 error level. But since 'four_star' is highly significant (less than 1%) I will remove the 'three_star' variable.  

```{r}
# remove 'three_stars'
lm1.5 <- lm(sp_finish ~ four_stars, data = data2)
summary(lm1.5)
```

After removing the insignificant variables in the model we are left with one highly significant variable of 'four_stars', which is the number of high school recruits rated as a four star on the roster. To interpret the model, an increase in 1 four star recruit on the roster is associated with an DECREASE in final ranking of -1.12. Please note final ranking is listed in descending order with 1 as the "highest" ranked team so an increase in four star players is associated with a higher final ranking. 

I am a little surprised this was the remaining variable, and my suspician is because there were highly correlated variables there is a strong possibility other variables would be highly significant if left in the model as the lone variable. I will test this by creating an alternate linear regression model with a different remaining variable that is likely correlated with 'four star.'

```{r}
## ggplot

# load ggplot library
library(ggplot2)

# ggplot of lm1.5
ggplot(data = data2, aes(four_stars, sp_finish)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(y = "S&P Finish", x = "Number of Four Star Recruits on Roster") + 
  ggtitle("Linear Regression Model: Number of 4-Star Recruits vs. S&P Finish")
```



```{r}
# alternative single linear regression with 'four_star_min_perc' as lone variable
lm1.5_alt_1 <- lm(sp_finish ~ four_star_min_perc, data = data2)
summary(lm1.5_alt_1)
```

Interpretation: an increase in 1 percentage of the roster that is a 4 or 5 star recruit is associated with a DECREASE in final ranking of -0.77. In other words, increasing the percentage of 4 & 5 star players is assocaited with improved S&P final results. 

```{r}
# ggplot of lm1.5_alt_1 using 'four_star_min_perc' as alternate lone variable
ggplot(data = data2, aes(four_star_min_perc, sp_finish)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(y = "S&P Finish", x = "Percentage of 4 & 5 Star Recruits on Roster") + 
  ggtitle("Linear Regression Model: Number of Minimum 4-Star Recruits vs. S&P Finish")
```

```{r}
# alternative single linear regression with 'talent_rank' as lone variable
lm1.5_alt_2 <- lm(sp_finish ~ talent_rank, data = data2)
summary(lm1.5_alt_2)
```

Interpretation: an increase in 1 talent rank is associated with an INCREASE in S&P final ranking of 0.75. Alternatively we could say a decrease in 1 talent rank (the ranking is improved by 1) is associated with a DECREASE in S&P final ranking of 0.75, meaning the final ranking was improved. 

```{r}
# ggplot of lm1.5_alt_2 using 'talent_rank' as alternate lone variable
ggplot(data = data2, aes(talent_rank, sp_finish)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(y = "S&P Finish", x = "Talent Rank of Roster per 247sports.com") + 
  ggtitle("Linear Regression Model: Talent Rank of Roster vs. S&P Finish")
```

As shown in alternate linear models (lm1.5_alt_1 & lm1.5_alt_2) we could have used alternate lone variables to use for our linear regression model with alternate interpretations. The bottom line remains that there are strong correlations present to the level of recruiting and final S&P rankings. 

## Further validation of constant variance & normality of the data 

After we have produced a linear regression model we can reassess the validity of the model. We will do so by re-checking our assumptions of constant variance and normality.  

```{r}
# residual plot to assess constant variance
plot(predict(lm1.5), resid(lm1.5))
abline(h=0)

# QQ-plot to assess normality
qqnorm(resid(lm1.5))
qqline(resid(lm1.5))
```

Based on the predict v residual plot there are certainly concerns about constant variance for our linear model produced. We may consider transformations to improve our model to meet assumptions of constant variance. 

```{r}
# log transformation 
lm1.6_log <- lm(log(sp_finish) ~ log(talent_rank), data = data2)
summary(lm1.6_log)
```

Our p-value for the variable 'talent_rank' is acceptable. 

```{r}
# residual plot to assess constant variance
plot(predict(lm1.6_log), resid(lm1.6_log))
abline(h=0)

# QQ-plot to assess normality
qqnorm(resid(lm1.6_log))
qqline(resid(lm1.6_log))
```

Using a log transformation did not improve our level of variance. Next we will try a square root transformation. 

```{r}
# square root transformation 
lm1.6_sqrt <- lm(sqrt(sp_finish) ~ sqrt(talent_rank), data = data2)
summary(lm1.6_sqrt)
```

```{r}
# residual plot to assess constant variance
plot(predict(lm1.6_sqrt), resid(lm1.6_sqrt))
abline(h=0)

# QQ-plot to assess normality
qqnorm(resid(lm1.6_sqrt))
qqline(resid(lm1.6_sqrt))
```

As shown above the amount of variance has improved greatly. This will slightly alter our interpretation of the model in the way that an increase of 1 talent ranking (square root of 1 = 1) is associated with an increase in sqrt(0.75) = 0.86 in S&P finish. 

## Machine Learning Part 1
## Regression Tree Modeling - Continious 

The next portion of our analysis will include Regression Tree modeling. We will first gauge the level of variance that can be explained by the model. If the amount is high, it is clear there are notable trends in the data set, while if the variance is lower the data is more random. 

```{r}
# for reference
head(data2)
```

```{r}
# load packages 
library(rpart)
library(rpart.plot)

# copy data set 
data2alt <- data2

# create model for decision tree to predict 'sp_finish' from the given variables
decision_tree1 <- rpart(sp_finish ~ talent_rank + roster_size + five_stars + four_stars + three_stars + five_star_perc + four_star_min_perc, data = data2alt, method = "anova")
rpart.plot(decision_tree1, type = 3)
```

```{r}
# summary of decision tree
summary(decision_tree1)
```


Based on the rpart plot we can see there are several trees or "nodes" of interest among the variables included. At the bottom we can see the number of data points that followed each branch sequence. This is a great starting point to visualize the model but we will need to further explore the fit and accuracy of the model and see if we can make predictions. 

```{r}
## if we want to make predictions we can split data into training & test sets
# use 60% of sample size for training set
smp_size <- floor(0.6*nrow(data2alt))

# set seed to keep training & test sets the same rows (use random number)
set.seed(10)

# create training set using ~60% of data
train_rows <- sample(seq_len(nrow(data2alt)), size = smp_size)
rpart_train_1 <- data2alt[train_rows, ]
rpart_test_1 <- data2alt[-train_rows, ]
```

```{r}
library(tree)

# remove character variables (columns 2&3)
data3alt <- data2alt[-c(2,3)]

# create training set
train.set <- sample(1:nrow(data3alt), nrow(data3alt)*0.6)

# create regression tree
tree.data <- tree(sp_finish ~ ., data3alt, subset = train.set)

# create yhat for predicted values
yhat <- predict(tree.data, newdata = data3alt[-train.set,])

# test set on y variable "sp_finish"
data.test <- data3alt[-train.set, "sp_finish"]

data.test.vector <- as.vector(data.test['sp_finish'])
class(data.test.vector)
#head(data.test.vector)

#str(data.test)

# mean square error (MSE)
mean((yhat - data.test.vector)^2)
```

```{r}
str(data3alt)
```



```{r}
# replicate model using only training set
decision_tree1_train <- rpart(sp_finish ~ talent_rank + five_stars + four_stars + three_stars + four_star_min_perc + five_star_perc, data = rpart_train_1)
#decision_tree1_train
```



```{r}
# use analysis from training model to make predictions on test set and store as vector
rpart_predict_1 <- c(predict(decision_tree1_train, rpart_test_1))

# list all predictions on test set
rpart_predict_1

# convert predicted results as numeric in order to create vector
rpart_predict_1 <- as.numeric(rpart_predict_1)

# verify vector is TRUE if length is equal to sample size
length(rpart_predict_1)
```

```{r}
# view specific entries included in the test set
rpart_test_1
```


Now that we have a list of predictions I will work to merge the predicted values into the data frame of the training set to illustrate the predicted finish with the actual finish in a single data frame. 


```{r}
# remove columns so only 'year', 'school' and 'sp_finish' remain
predicted_results_sub <- rpart_test_1[c(1,2,9)]

# for reference
head(predicted_results_sub, 3)
```

```{r}
# merge predicted results into testing set dataframe
predicted_results_merge <- predicted_results_sub %>% 
  mutate(year, pred_finish = rpart_predict_1)

# now we can see side-by-side the predicted finish and sp_finish for each team (by year) included in the test set
predicted_results_merge
```

Based on the dataframe above we can see the predicted finish based on our decision tree model in the far right column, and the actual S&P Finish in the column to the left. We can see some estimates are accurate while others not so much. Could these be outliers? How accurate are these predictions? One metric we can use is the Mean Absolute Error which we can be interpreted as the mean different between the actual and predicted results. 

```{r}
# calculate mean square error (MSE)
mean((predicted_results_merge$pred_finish - predicted_results_merge$sp_finish)^2)
```

The MSE is ~946, which we can interpret as the average of the square of the residuals. Meaning, if we sqaured each residual (difference between the actual and predicted value) it would give us the average. This number is great for telling us the accuracy of our model but not so great to interpret in context. For that reason we will use the Mean Absolute Error. 

```{r}
# load package to calculate MAE
library(Metrics)

# Mean Absolute Error (MAE)
mae(predicted_results_merge$sp_finish, predicted_results_merge$pred_finish)
```

Our MAE is ~ 23, which we can interpret as our decision tree model can predict the final result within 23 (or +/- 11.5) spaces away form the observed value. In context, based on the recruiting criteria included if a team were to finish the season at #40, our model would have predicted their finish between ~28 and ~52. 


```{r}
## ggplot

# create data frame of actual v predicted results
mae_df <- data.frame(predicted_results_merge$sp_finish, predicted_results_merge$pred_finish)

# confirm data frame created
# head(mae_df)

mae_plot <- ggplot(data=mae_df, aes(predicted_results_merge$sp_finish, predicted_results_merge$pred_finish)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(x = "S&P Finish", y = "Decision Tree Predicted Finish") + 
  ggtitle("S&P Finish v. Decision Tree Predicted Finish")

mae_plot
```




## Machine Learning Part 2
## Random Forest Modeling - Classification 

In our next portion we will explore the specific 'decisions' made by the tree and the importance of each, as well as key metrics. Random forest modeling is most widely used for classifying data based on the variables included. To classify the data we will need a categorical dependent variable, which we will utilize the independent variables to make predictions on how to classify the dependent variable. Currently our dependent variable of interest 'sp_finish' is continious, but we can convert this to categorical whether a team finised inside the the Top 25, a widely used measuing stick for college football fans. 

```{r}
## we will convert our data set so the dependent variable is categorical
data3 <- data2

# add new column where "yes" = finish top 25 & "no" = did not finish top 25
data3 <- data3 %>%
  mutate(school, fin_top_25 = cut(sp_finish, breaks = c(0, 25, 117), labels = c("yes", "no")))

# for our decision trees the variables 'year', 'school', & 'conference' are good for reference but will likley add unnecessary noise to a decision tree model. We will remove
data3 <- within(data3, rm(year, school, conference))

# in addition we will remove 'sp_finish' variable since this is directly related to the 'fin_top_25' variable we are attempting to predict with the remaining variables
data3 <- within(data3, rm(sp_finish))

# notice 12th column 'fin_top_25' has been added as categorical variable
head(data3,3)
```

```{r}
## divide data into training & test sets

# use 75% of sample size for training set
smp_size <- floor(0.75*nrow(data3))

# set seed to keep training & test sets the same rows (use random number)
set.seed(55)

# create training set using ~75% of data (112 rows)
train_rows <- sample(seq_len(nrow(data3)), size = smp_size)
top25_train <- data3[train_rows, ]
top25_test <- data3[-train_rows, ]

# prop table for reference
prop.table(table(top25_train$fin_top_25))
prop.table(table(top25_test$fin_top_25))
# if proportions are relatively equal for both sets (within 5% margin) we can proceed
```

The proportion tables are realtively equal. We will proceed with the assumption the testing and training sets are equally representative of the data. 

```{r}
# show classification tree using c5.0 package 
library(C50)

# produce classification tree (note the 11th column is removed since 1-10 are predictors)
tree1 <- C5.0(top25_train[-8], top25_train$fin_top_25)
tree1
```

We can see from our classification tree there were 165 samples included (the training set), 7 predictors utilized, and a tree size of 8. In other words, there were 8 "tree nodes" or decisions. This is an indication several of the variables included are 'useful' in categorizing the dependent variable. 

```{r}
# summary of classification tree for further details
summary(tree1)
```


```{r}
plot(tree1)
```


Based on the table produced from our random forest model, we can see the model used 4 key variables/nodes (equal to 8 brances since there are 2 branches per node). The first node is regarded as the most important, which is 'four_stars', which complements our findings from the linear regression analysis earlier. 

It is worth noting the model was much more effective in classifying the teams that did NOT finish inside the Top 25. This could suggest recruiting will certainly hamper teams from achieving success on the field if their recruiting does not meet a certain threshold, but there are other factors not included in our model that are key to finishing inside the Top 25. 

Next we will explore this same concept but change the classification to finishing inside the Top 10 and see if our model performs better or worse, another widely regarded measuring stick from college football fans. 

```{r}
## we will convert our data set so the dependent variable is categorical
data3alt <- data2

# add new column where "yes" = finish top 10 & "no" = did not finish top 10
data3alt <- data3alt %>%
  mutate(school, fin_top_10 = cut(sp_finish, breaks = c(0, 10, 117), labels = c("yes", "no")))

# for our decision trees the variables 'year', 'school', & 'conference' are good for reference but will likley add unnecessary noise to a decision tree model. We will remove
data3alt <- within(data3alt, rm(year, school, conference))

# in addition we will remove 'sp_finish' variable since this is directly related to the 'fin_top_10' variable we are attempting to predict with the remaining variables
data3alt <- within(data3alt, rm(sp_finish))

# notice 12th column 'fin_top_10' has been added as categorical variable
head(data3alt,3)
```

```{r}
## divide data into training & test sets

# use 75% of sample size for training set
smp_size <- floor(0.75*nrow(data3alt))

# set seed to keep training & test sets the same rows (use random number)
set.seed(55)

# create training set using ~75% of data (112 rows)
train_rows <- sample(seq_len(nrow(data3alt)), size = smp_size)
top10_train <- data3alt[train_rows, ]
top10_test <- data3alt[-train_rows, ]

# prop table for reference
prop.table(table(top10_train$fin_top_10))
prop.table(table(top10_test$fin_top_10))
# if proportions are relatively equal for both sets (within 5% margin) we can proceed
```

The proportion tables of the training and testing data sets are relatively equal. 

```{r}
# produce classification tree (note the 11th column is removed since 1-10 are predictors)
tree2 <- C5.0(top10_train[-8], top10_train$fin_top_10)
tree2
```

The tree will utilize 7 predictors, but interestingly the tree size is only 2 nodes now. This is likely an indicator there is a key variable that stands out above the others as opposed to our tree1 model. 

```{r}
summary(tree2)
```

The summary of our tree2 analysis yields a lower error rate, which is encouraging that we can better trust our results. In addition, the variable 'five_stars' is now the key branch in finishing inside or outside the top 10. Intuitively this makes sense as five star players are rated as the very best high school recruits, so it is reasonable to assume the college teams that accumulate the most five star players would finish at the very top. It is worth noting we have a similar problem as with tree1, where the model is much more effective at predicting teams finishing OUTSIDE the top 10, but all of the variance is from predicting teams INSIDE the top 10. This is further evidence that if a team does not accumulate a certain threshold of five star players (greater than or equal to 8) it will hamper the teams success, but there are likely other varaibles not included in our model that would explain the teams that do finish inside the top 10. 




























































