###################### 
# Exam 2
# Grant Gasser
# Fall 2018
#####################

#Load and view
attach(mtcars)
names(mtcars)
head(mtcars)

#Look at relationship among variables
library(GGally)
ggpairs(mtcars)

#May be some issues with linearity and there is obvious multicollinearity
#Disp, hp, wt seem to have negative linear relationship
#drat and maybe qsec have a positive linear relationship, rest are essentially indicators

#Predicting Y (mpg), create model
fit <- lm(mpg ~ ., data=mtcars)
summary(fit)

#Look at residuals v. fitted, can see issue with linearity more clearly here
plot(fit, which = 1)

#Keep an eye on Chrysler Imperial, Fiat 128, and Toyota Corolla

#Look at multicollinearity in X's
library(car)
vif(fit) 

#Can see lots of multicollinearity, indicating redundance and too many variables

#Iterative process: To see which X's are collinear, remove X with highest VIF (disp at 21.6)
fit.tmp <- lm(mpg~.-disp, data=mtcars)
vif(fit.tmp)

#VIF of wt and carb decreased significantly, implying they are collinear with disp

#Remove next highest VIF: cyl at 14.3
fit.tmp <- lm(mpg~.-disp-cyl, data=mtcars)
vif(fit.tmp)

#VIF of hp and qsec both decreased by about 1, implying they have some collinearity with cyl

#All VIFs are now below 10, more specifically, about <= 6


#Now use regsubsets with the BIC/SBC criterion to get a final model
library(leaps)
regsubsets.out <- regsubsets(mpg ~ .,
                            data = mtcars,
                            nbest = 3,       
                            method = "exhaustive")
mat = summary(regsubsets.out)
cbind(mat$outmat, mat$bic)[order(mat$bic),]
plot(regsubsets.out, scale = "bic", main = expression(BIC))

#These visualizations show the best model according to BIC is the one with wt, qsec, and am
#Next best is model with cyl and wt

#Try model with wt, qsec, and am
fit.bic <- lm(mpg~wt+qsec+am, data=mtcars)
vif(fit.bic)

#Low VIFs for this fit, we will choose this as the prediction model

#Fit
fit <- lm(mpg~wt+qsec+am, data=mtcars)

#As weight goes up, mpg goes down (makes sense)
#As qsec goes up, mpg goes up (faster cars tend to be nicer, more expensive, often have better mpg than bad old cars)
#As am goes up (is one), mpg goes up. Automatic cars tend to be newer, newer cars tend to have better mpg
summary(fit) 

#Begin outlier analysis
plot(fit, which=1)

#As we saw with the initial fit, Chrysler Imperial, Fiat 128, and Toyota Corolla stand out

#Outliers wrp to Y: Chrysler Imperial, rstudent = 2.32
outlierTest(fit)

dat.tmp = data.frame(fitted.values = fit$fitted.values, residuals = rstudent(fit))

n = nrow(dat.tmp)
p = length(coef(fit))

#Outliers wrp to X: Merc 230 and Lincoln Continental
hii <- hatvalues(fit)
names(which(hii>2*p/n))


#Influential Points: Merc 230 (X outlier), Chrysler Imperial (Y outlier), Fiat 128, Toyota Corolla
influ <- dffits(fit)
names(which(abs(influ) >  2*sqrt(p/n)))

#Cook's Distance: Chrysler Imperial has much more influence than any other points
#About 2x as much influence as Merc 230 and Fiat 128
plot(fit, which = 4)

#Look at influence on Betas
dfbetasPlots(fit)

#Plot doesn't show variable names, looks like 17 is influential on all 3 betas b1, b2, b3
#17 is beyond cutoff for beta1
which(dfbetas(fit)[,2] > 1)

#This is the Chrysler Imperial, an outlier wrp to Y and the most influential point

#Now check assumptions
#Normality
qqPlot(rstudent(fit)) #Chrysler Imperial and Fiat 128 challenging the assumption, close to bounds

shapiro.test(rstudent(fit))

#The points (including Chrysler Imperial) are concerning, but not enough evidence to say
#the residuals are not normal at level alpha = .05

#Just curious: What if we removed Chrysler Imperial?
mtcars.2 <- mtcars[-17,]

fit.2 <- lm(mpg ~ wt+qsec+am, data=mtcars.2)

#See how much it affects normality: pval went from about .08 to .4
shapiro.test(rstudent(fit.2))

#Normality assumption much safer without 17


#Test independence of residuals
library(lmtest)
dwtest(fit) 
bgtest(fit)

#Both tests: Not enough evidence to say the residuals are not independent

#Test whether residuals have constant variance
bptest(fit)

#Not enough evidence to say residuals don't have constant variance

#90% Confidence Interval
#Weight is 2750 pounds
#1/4 mile time is 15.9 seconds
#Automatic transmission
newX <- data.frame(wt=2750/1000, qsec=15.9, am=0)
predict(fit, newX, interval="confidence", level=.90)

#So a 90% CI for these values is: (18.34, 20.58)



#### JUST FOR FUN: TRY LASSO ####
#set up the X and Y
Y = mpg
X = mtcars[,-1]
X = as.matrix(X)

library(glmnet)

#alpha = 1 for lasso
fit.lasso <- cv.glmnet(x=X,y=Y, alpha=1, nfolds=5)

plot(fit.lasso) #looks like model with 3 variables will be best

#looks like minimum will be best
coef(fit.lasso,s='lambda.min',exact=TRUE)

#This model includes cyl, hp, and wt (NOTE: all with negative coefficients)

y.hat <-  predict(fit.lasso,X)

res <-  as.numeric(Y-y.hat)
names(res) <-  row.names(mtcars)

#Normality assumption is violated
qqPlot(res)

#Now test normality (shapiro), enough evidence to suggest residuals not normally distributed
shapiro.test(res)

#Not sure how to measure other assumptions with glm object

#90% Confidence Interval, not sure how, prediction did not work..
#newX <- data.frame(cyl=6, hp=130, wt=2750/1000)
#newX <- as.matrix(newX)
#predict(fit.lasso, newX, interval="confidence", level=0.90)
#predict.cv.glmnet(fit.lasso, newX, s="lambda.min")
