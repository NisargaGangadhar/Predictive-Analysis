#NISARGA.G
#19203753

#loading the dataset.
dataset=read.csv(file.choose())
head(dataset)
summary(dataset)

#EXPLORATORY DATA ANALYSIS:
#1. Using a boxplot, histogram and summary. 
#Describe the distribution of the sales price of the houses.
summary(dataset$Price)
par(mfrow=c(1,2))
boxplot(dataset$Price,ylab="SalesPrice",main="Boxplot")
hist(dataset$Price, freq=FALSE, xlab="Sales Price", breaks="FD",main="Histogram and density estimates")
lines(density(dataset$Price), lwd=2, col="blue")

#2. Convert all the categorical variables to factors. Using the summary and a boxplot describe 
#how sales prices vary with respect to the number of bedrooms, bathrooms, garage size and school.
dataset$Bath <- factor(dataset$Bath)
dataset$Bed <- factor(dataset$Bed)
dataset$Garage <-factor(dataset$Garage)
dataset$School <-factor(dataset$School)
par(mfrow=c(2,2))
boxplot(dataset$Price ~ dataset$School)
boxplot(dataset$Price ~ dataset$Bed)
boxplot(dataset$Price ~ dataset$Garage)
boxplot(dataset$Price ~ dataset$Bath)
by(dataset$Price,dataset$School,summary)
by(dataset$Price,dataset$Bed,summary)
by(dataset$Price,dataset$Garage,summary)
by(dataset$Price,dataset$Bath,summary)

#3. Using the summary, correlation and the pairs plots discuss the relationship between the response
#sales price and each of the numeric predictor variables.
cor(dataset[,c(1,2,3,6)])
data <- dataset[,c(1,2,3,6)]
pairs(data)
by(dataset$Price,dataset$Size,summary)
by(dataset$Price,dataset$Lot,summary)
by(dataset$Price,dataset$Year,summary)
########################################################################################################## 
#REGRESSION MODEL
#1. Fit a multiple linear regression model to the data with sales price as the
#response and size, lot, bath, bed, year, garage and school as the predictor
#variables. Write down the equation for this model
###rescaling
size=dataset$Size - mean(dataset$Size)
lot=dataset$Lot-mean(dataset$Lot)
year=dataset$Year-mean(dataset$Year)
mod=lm(Price~lot+size+year+Bath+Bed+Garage+School,data=dataset)
summary(mod)
#Question 9 - Residual Plot
library(car)
residualPlot(mod)
##########################################################################################################
#ANOVA
#1. Compute the type 1 anova table.
anova(mod)
#3. Compute a type 2 anova table comparing the full model with all predictor
#variables to the the reduced model with the suggested predictor variable
#identified in the previous question removed
mod1=lm(Price~lot+size+Bath+Bed+Garage+School,data=dataset)
anova(mod,mod1)
#########################################################################################################
#DIAGNOSTICS
#1. added variable plots and component residual plot.
library(car)
library("GGally")
avPlots(mod)
crPlots(mod)
#2.dwt test
dwt(mod)
#3.vif test
vif(mod)
#4.zero conditional mean and homoscedasticity
plot(fitted(mod),rstudent(mod))
abline(h=0)
par(mfrow=c(1,2))
plot(dataset$Lot,rstudent(mod))
plot(dataset$Size,rstudent(mod))
plot(dataset$Year,rstudent(mod))
#5.histogram and quantilequantile plot of the studentized residuals.
hist(rstudent(mod))
qqnorm(rstudent(mod))
qqline(rstudent(mod),col=2)
#################################################################################################
#Leverage, Influence and Outliers:
###q1
leveragePlots(mod)                           
###q2
influencePlot(mod)
###q3
library("MASS")
outlierTest(mod)
################################################################################################
#Expected Value, CI and PI:

confi=predict(mod,level=0.95,interval='confidence')
pred=predict(mod,level=0.95,interval='prediction')

ggplot(dataset,aes(y=dataset$Price,x=fitted(mod)))+geom_point()+

stat_smooth(aes(y=confi[,"upr"]),method=lm,se=FALSE)+
stat_smooth(aes(y=confi[,"lwr"]),method=lm,se=FALSE)+
stat_smooth(aes(y=pred[,"upr"]),method=lm,se=FALSE,col="red")+
stat_smooth(aes(y=pred[,"lwr"]),method=lm,se=FALSE,col="red")+
geom_line(aes(y=confi[,"fit"]),col='black')
  

