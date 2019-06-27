#librarys
library(readr)
library(ggplot2)
library(GGally)
library(leaps)

#loading in the dataset, downloaded from basketballreference.
NBA_17_18_Data_Sheet1 <- read_csv("C:/Users/Jeet/Downloads/per game - Sheet1.csv")
#viewing to make sure everything is ok
View(NBA_17_18_Data_Sheet1)
#Now limitting the data set since many players barely played any significant minutes

#only selecting variables I might need removing
final_predictors <- c("Player", "Tm", "G", "MP", "2P%", "3P%", "FG%", "eFG%", "FT%", "PS/G")
removal1 <- NBA_17_18_Data_Sheet1[final_predictors]
#Subsetting my data to only players that played > 20 games and averaged > 12.
removal2 <- subset(removal1, G >= 20 & MP >= 12)
#Also for simplicity sake I will cut out any players that had NA listed in any column. (They never scored a point or FT or 3-pointer)
removal3 <- removal2[complete.cases(removal2),]
#Now I have a much smaller dataset that doesn't have players that only played 1 minute or game affecting my results.
#Firstly I want to make a scatterplot matrix of my predictors to my response which is PTS.
#I will choose the predictors: Age, MP, FG%, 3P%, eFG%, PTS
final_data <- subset(removal3)[final_predictors]
ggpairs(final_data[c(3:10)])

#So now although our correlation is weak I am still going to test the residuals to see that the LINE parameters are met.
#assigning all the cols to a name
player <- final_data$Player
points <- final_data$`PS/G`
team <- final_data$Tm
games_played <- final_data$G
min_played <- final_data$MP
two_point <- final_data$`2P%`
three_point <- final_data$`3P%`
FG <- final_data$`FG%`
eFG <- final_data$`eFG%`
FT <- final_data$`FT%`

#my first model with all 7 predictors: games_played, min_played, two_point, three_point, FG, eFG, FT.
model1 <- lm(points ~ games_played + min_played + two_point + three_point + FG + eFG + FT)
#dataframe with only predictors
predictors <- final_data[,c(3:10)]
#summary
summary(model1)
#checking for variances.
resids_model1 <- fortify(model1)
#plot
ggplot(resids_model1, aes(x = .fitted ,y = .resid)) +
  geom_point()
######It seems the variance is close to being constant but not quite. As a result I will try log transforming y which is points.
#qqplot
ggplot(data = predictors, aes(sample = points)) +
  geom_qq() +
  geom_qq_line()
######this seems to be close to normality but not quite.

#So now lets work on making this linear and with a constant variance
#new model with log of our response (points)
model2 <- lm(log(points) ~ games_played + min_played + two_point + three_point + FG + eFG + FT)
resid_model2 <- fortify(model2)
ggplot(resid_model2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)
####As we can see the variance is now constant throughout. As a result this model fits the E in "LINE".
ggplot(data = predictors, aes(sample = points)) +
  geom_qq() +
  geom_qq_line()
#So this plot shows us that our data might not follow normal distribution but we can run a shapiro wilke hypothesis test to be sure.
#####The shapiro-wilke test tells us that the null Hypothesis = Data is normally distributed is in fact true since our p-value is greather then 0.05 which means we fail to reject our null meaning that our null is true.
shapiro.test(model2$residuals)
#####Just to be safe lets plot the histogram of the residuals and do an eye test. As we can see the error terms (or residuals is normally distributed)
hist(model2$residuals)


#####Now that we have our model lets do some analysis on how good our predictors are.
#####problem 5 hw 3 use all 3 types
####AIC METHOD
#Now using AIC Method lets see our best predictors.
#reduced model with no predictors
mod_reduced <- lm(log(points) ~ 1)
#full model with all potential predictors
mod_full <- lm(log(points) ~ games_played + min_played + two_point + three_point + FG + eFG + FT)
#calculating the AIC for the best predictors step by step
step(mod_reduced, scope = list(lower = mod_reduced, upper = mod_full))
#####According to the AIC the model with the smallest AIC is preffered which in this case is the model: lm(log(points) ~ min_played + FT + FG + two_point + games_played + three_point)


#Now using the adjusted rsquared model
n <- nrow(predictors)
fit <- regsubsets(cbind(games_played, min_played, two_point, three_point, FG, eFG, FT), log(points))
summary_fit <- summary(fit)
rss <- summary_fit$rss
mse <- c(rss[1]/(n - 2), rss[2]/(n - 3), rss[3]/(n - 4), rss[4]/(n - 5), rss[5]/(n - 6), rss[6]/(n - 7), rss[7]/(n - 8))
mse
#####according to the adjusted R^2^ we should pick the one with the highest value and lowest MSE which in this case since all values of MSE and adjusted R^2^ method are close we will choose another method to pick our model.

#Now using Cp Criterion to find the best model
summary_fit$cp
##According to C~p~ Criterion the best model is the one where C~p~ = p, since our p = 8 (intercept + 7 potential predictors) our best fitting model is the second to last model ie: lm(log(points) ~ games_played + min_played + three_point + FG + eFG + FT)


#####check for influential points.
#using hatvalues function to find high leverage points
full_model <- lm(log(points) ~ min_played + FT + FG + two_point + games_played + three_point)
hatvalues(full_model)

which(hatvalues(full_model) >= 0.05)

###Since 3(7/408) = 0.05. We want can clasify anyone higher this value as a leverage point and possibly an influential point.
###In this case we will keep a close eye on rows: 16, 23, 126, 134, 189, 237, 238, 315, 328, 394, 404, 407.

#####Finding influential points:

#DFFITS (Difference in Fits)

which(dffits(full_model) > 0.28)
dffits(full_model)[c(159, 295, 354, 381)]


#Studentized Residuals
which(rstandard(full_model) == max(rstandard(full_model)))
rstandard(full_model)[354]
#354 Is a potential outlier
####Now using the Cooks Distance to calculate influential points
cooks_dis <- cooks.distance(full_model)
plot(mod_full, which = 4)
#According to this rows 16 238 and 328 are influential points. Which just so happen to correspond to some of our high leverage points.

