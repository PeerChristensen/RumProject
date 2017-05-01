#Rum project: main script (all data)
#Fall 2016
#Peer Christensen

# This script reads the final data file with all the needed information
# and explores different statistical models for predicting rum ratings 
# based on price pre bottle in USD and sugar content in g/L
#---------------------------------------------------
### INSTALL REQUIRED PACKAGES ###
#install.packages("Hmisc")
#install.packages("pastecs")
#install.packages("QuantPsyc")
#install.packages("relaimpo")
#install.packages("car")
#install.packages("caret")
#install.packages("broom")
#install.packages("GGally")
#install.packages("GGally")
#install.packages("corrgram")
#install.packages("MASS")
#install.packages("ggplots")
#install.packages("plotly")
#install.packages("robust")
#install.packages("robustbase")
#install.packages("RColorBrewer")
#install.packages("corrplot")
#install.packages("gvlma")
#---------------------------------------------------
#SET WORKING DIRECTORY, LOAD DATAFILE
setwd("/Users/peerchristensen/Desktop/rum project")
rum = read.csv2("rum_to_R.csv",header=T,stringsAsFactors = T,sep=";")
#---------------------------------------------------
#CLEAN AND/OR SUBSET DATA
#subsetting rums classed as "Aged"
aged=subset(rum,Category=="Aged")
aged=droplevels(aged)
aged=na.omit(aged)
#---------------------------------------------------
#EXPLORING THE DATA
str(aged)
summary(aged)
head(aged)
pairs(aged[,c(4,6:7)],panel = panel.smooth)
library(caret)
featurePlot(x=aged[,c("Price","Sugar")],y=aged$Rating,plot="pairs")
library(ggplot2,GGally)
ggpairs(aged[c(6:7,4)],lower=list(continuous="smooth_loess"))
#checking for collinearity using correlations
library(Hmisc)
rcorr(as.matrix(aged[,c(4,6:7)]))
#Correlogram
library(corrgram)
library(RColorBrewer)
col=brewer.pal(11,"RdBu")
corrgram(aged[,4:7],col.regions=colorRampPalette(col[c(2:5,7:10)]))
library(corrplot)
aged_cor=cor(aged[,c(4,6:7)])
corrplot(aged_cor)
#---------------------------------------------------
#EXPLORING THE VARIABLES, REMOVING OUTLIERS
library(pastecs)
#Price
boxplot(aged$Price)
boxplot.stats(aged$Price) #$out = outliers
q_three=summary(aged$Price)[5] #3rd quartile
iqr_price=IQR(aged$Price) #interquartile range
high_val_price = (iqr_price * 1.5) + q_three #equation giving the recommended high cut-off point for outliers
aged=subset(aged,Price <= high_val_price) #remove rows with values above cut-off point
#Sugar: same procedure as for the Price variable
boxplot(aged$Sugar)
boxplot.stats(aged$Sugar)
q_three=summary(aged$Sugar)[5]
iqr_sugar=IQR(aged$Sugar,na.rm=T)
high_val_sugar = (iqr_sugar * 1.5) + q_three
aged=subset(aged, Sugar <= high_val_sugar)
#histogram: Price
hist(aged$Price,breaks=15,col="red",freq=F)
curve(dnorm(x,mean=mean(aged$Price),sd=sd(aged$Price)),add=TRUE,lwd=3)
#hist of log of Price
hist(log(aged$Price),breaks=15,col="red",freq=F)
curve(dnorm(x,mean=mean(log(aged$Price)),sd=sd(log(aged$Price))),add=TRUE,lwd=3)
#histogram: Sugar
hist(aged$Sugar,breaks=15,col="goldenrod1",freq=F)
curve(dnorm(x,mean=mean(aged$Sugar),sd=sd(aged$Sugar)),add=TRUE,lwd=3)
#histogram: Rating
hist(aged$Rating,breaks=10,col="lightgreen",freq=F,ylim=c(0,0.5))
curve(dnorm(x,mean=mean(aged$Rating),sd=sd(aged$Rating)),add=TRUE,lwd=3)
#distribution stats (see Field, mainly for small sample sizes)
round(stat.desc(data.frame(aged$Price,aged$Sugar,aged$Rating),basic=F,norm=T),2)
#output:
#skewness: should be 0, positive val indicate pile-up on the left side
#skew.2SE: skew / 2*SE, if absolute val greater than 1, skew is significant at p < .05
#kurtosis: should be 0, positive val indicate pointy (leptokurtic) shape
#kurt.2SE: kurtosis / 2*SE, if absolute val greater than 1, skew is significant at p < .05
#normtest.W: Shapiro–Wilk test of normality
#normtest.p: p-val for normality test, should be above .05
#-----------------------------------------------
#SCATTERPLOTS
#Rating ~ Price
p_price = aged$Rating~aged$Price
plot(p_price)
abline(lm(p_price),lwd=2,col="blue")
p_dat=subset(aged,Price>=0)
lines(lowess(p_dat$Price,p_dat$Rating), col="red",lwd=2)
#Rating ~Sugar
p_sugar = aged$Rating~aged$Sugar
plot(p_sugar)
abline(lm(p_sugar),lwd=2,col="blue")
s_dat=subset(aged,Sugar>=0)
lines(lowess(s_dat$Sugar,s_dat$Rating), col="red",lwd=2)
#all variables in a matrix plot
scatterplotMatrix(~Rating+Price+Sugar,aged)
#--------------------------------------------------------------
# REGRESSION MODELS
#Price
mod1 <- lm(Rating ~ Price,weights=Raters,data=aged)
summary(mod1)
#Sugar
mod2 <- lm(Rating ~ Sugar,weights=Raters,data=aged)
summary(mod2)
#Price + Sugar 
mod3 <- lm(Rating ~ Price+Sugar,weights=Raters,data=aged)
summary(mod3)
#mod1 vs. mod3
#in hierarchical regression, in which we add predictor variables one by one, we can compare
#models at each step to check if the addition of a given variable significantly improves the model 
anova(mod1,mod3)
#We can also compare models using Akaike's Information Criterion, a measure of fit
#that penalizes the model for including more variables. The model with the smallest AIC is the best fit
AIC(mod1,mod3)
#log(Price) +  Sugar
mod4 <- lm(Rating ~ log(Price)+Sugar,weights=Raters,data=aged)
summary(mod4) 
#interpret log(Price) coef: we expect about 0.1 unit increase in Rating when Price increases by 10 %
AIC(mod3,mod4)
library(QuantPsyc)
lm.beta(mod4) #standardized coefficients (sd units, relative importance)
library(relaimpo)
impo = calc.relimp(mod4,type=c("lmg","last","first","pratt"),rela=TRUE) #relative importance
plot(impo)
confint(mod4) #confidence intervals
#summarizing models using "broom":
library(broom)
tidy(mod4)
glance(mod4)
round(head(augment(mod4)[,2:11]),2) #augment() returns a data frame with added columns of useful info
#----------------------------------------------------
# DIAGNOSTICS & PLOTS
library(car)
residualPlots(mod4) #plus lack-of-fit tests and Tukey's test for non-additivity
plot(mod4)
marginalModelPlots(mod4)
crPlots(mod4) #check for non-linearity, how to interpret?
#Cook's distance: a measure of the overall influence of an observation on the whole model
#Values should be smaller than 1
plot(cooks.distance(mod4),ylim=c(0,1))
plot(mod4, which=4) #Another plot of cook's distance, but the y-axis scale seems off
#histogram - check for non-normality
stud.resid=rstudent(mod4)
hist(stud.resid,freq=F,col = "lightblue",breaks=15)
curve(dnorm(x,mean=mean(stud.resid),sd=sd(stud.resid)),add=TRUE,lwd=3)
#variance inflation factor - test of multicollinearity (see Field)
vif(mod4)
#tolerance statistic - reciprocal of vif (see Field)
1/vif(mod4)
#test the assumption of independent errors using the Durbin–Watson test
dwt(mod4) # D-W should be close to 2 and p > .05
#global validation of linear model assumprions
library(gvlma)
gvlma(mod4)
#----------------------------------------------------
# CROSS-VALIDATION

#5x5-folds validation
set.seed(621)
library(caret)
# Shuffle row indices: rows
rows <- sample(nrow(aged))
# Randomly order data
x <- aged[rows, ]
# Determine row to split on: split
split = round(nrow(x) * .8)
# Create train
train = x[1:split,]
# Create test
test=x[(split+1):nrow(x),]
#2b. predict on test set
# Fit lm model on train: model
model=lm(Rating~log(Price)+Sugar,weights=Raters,train)
#RMSE on train set:
sqrt(mean((model$fitted-train$Rating)^2))
# Predict on test: p
p=predict(model,test)
# Compute errors: error
error=p-test[["Rating"]] 
# Calculate RMSE on test set
sqrt(mean(error^2)) 
# Fit lm model using 5 x 5-fold CV: model
model <- train(Rating ~ log(Price)+Sugar, train,
        preProcess=c("center","scale"),
        method = "lm", 
        trControl = trainControl(
                method = "cv", 
                number = 5,
                repeats = 5, 
                verboseIter = TRUE
        )
)
model
# Predict on dataset
predict(model,x)
#predicted RMSE "by hand"
x$pred_rat=predict(model,x)
err=x$pred_rat-x$Rating
sqrt(mean(err^2))

#10-folds cross-validation
model2 <- train(
        Rating ~ log(Price)+Sugar, x,
        method = "lm",
        trControl = trainControl(
                method = "cv", number = 10,
                verboseIter = TRUE
        )
)
model2

#-------------------------------------------------------------------
#ROBUST REGRESSION
#with rlm()
library(MASS)
rmod4a=rlm(Rating~log(Price)+Sugar,weights=Raters,aged)
summary(rmod4a) #p vals and R-squared missing
Price_pval=2*pt(13.1,72,lower=F)
Sugar_pval=2*pt(5.7,72,lower=F)
a=sum((predict(rmod4a)-mean(aged$Rating))^2)
b=sum((aged$Rating-mean(aged$Rating))^2)
rmod4a_rsq = a/b # (multiple r-squared) 0.769
rmod4a_adj_rsq = 1 - ((1-rmod4a_rsq) * (nrow(aged) - 1) / (nrow(aged) - 2 - 1)) #0.762
#with lmRob(), !! *mult-R^2=0.7634, weights look odd (see below)
library(robust)
rmod4b=lmRob(Rating~log(Price)+Sugar,weights=Raters,aged)
#with lmrob(), mult-R^2=0.7302
library(robustbase)
rmod4c=lmrob(Rating~log(Price)+Sugar,weights=Raters,aged)
#When comparing these models we see that rmod4a and rmod4c give similar results
#The dissimilarity of rmod4b appears to be due to a different method for identifying
#and weighing outliers...
df=data.frame(aged,rmod4a$w,rmod4b$w,rmod4c$rweights)
head(df[,8:10])
#inspecting the weight, we see that lmRob() just takes the weights from the Raters variable
#leaving out the "weights argument...
rmod4b=update(rmod4b,.~.-weights)
#------------------------------------------------------
### PLOTLY PLOTS
library(plotly)
plot_2d=plot_ly(aged, type="scatter", x = ~log(Price), y = ~aged$Rating,
           size = ~Sugar,sizes=c(300,1500), color=~Sugar,
                text = ~paste(Label),mode="markers") %>%
        layout(title="Relationship Between Rum Price and Average Rating")
plot_2d

plot_3d = plot_ly(x=log(aged$Price),y=aged$Sugar,z=aged$Rating,
                  type="scatter3d",mode='markers',size=aged$Raters,
                  color=aged$Sugar,text=aged$Label,sizes=c(600,2500)) %>%
                layout(scene=list(
                        xaxis=list(title="Price"),
                        yaxis=list(title="Sugar"),
                        zaxis=list(title="Rating")))
plot_3d

#for looking at the robust regr weights
robust_plot1=qplot(log(Price), Rating, size = 1-rmod4a.w, color=rmod4a.w,data=df)
robust_plot2=qplot(log(Price), Rating, size = 1-rmod4c.rweights, color=rmod4a.w,data=df)

px2=plot_ly(df, type="scatter", x = ~log(Price), y = ~Rating,
            size = ~rmod4b.w, color=~rmod4,
            text = ~paste(Label),mode="markers") %>% 
        layout(title="lmRob")
px3=plot_ly(df, type="scatter", x = ~log(Price), y = ~Rating,
            size = ~rmod4c.rweights,sizes=c(300,1500), color=~Sugar,
            text = ~paste(Label),mode="markers") %>%
        layout(title="lmrob")
px1;px2;px3
#-----
m <- loess(aged$Rating ~ log(aged$Price))
p2 <- plot_ly(aged, x = ~log(aged$Price), color = I("black")) %>%
        add_markers(y = ~aged$Rating, text = ~paste(aged$Label), showlegend = FALSE) %>%
        add_lines(y = ~fitted(loess(aged$Rating ~ log(aged$Price))),
                  line = list(color = 'rgba(7, 164, 181, 1)'),
                  name = "Loess Smoother") %>%
        add_ribbons(data = augment(m),
                    ymin = ~.fitted - 1.96 * .se.fit,
                    ymax = ~.fitted + 1.96 * .se.fit,
                    line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)',
                    name = "Standard Error") %>% 
        layout(title="Relationship Between Rum Price and Average Rating")
p2