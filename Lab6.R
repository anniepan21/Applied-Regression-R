### Lab 6 ###
library(faraway)
par(mfrow=c(2,1))
data(savings)
g <- lm(sr~pop15 + pop75 + dpi + ddpi, savings)

#Raw residuals; Standardized residuals
res <- g$residuals #raw residuals
sres<- rstandard(g) #standardized residuals
plot(res); plot(sres) #notice the 2 plots

X <- model.matrix(g) # Design matrix for g
Xt_X_inv <- solve(t(X) %*% X) #inverse of t(X)* X
H <- X %*% Xt_X_inv %*% t(X) # Hat matrix for g
y <- savings$sr
res <- y - H %*% y #residuals as seen similar to those given from model.

# 2. Outliers- Cook distances, Leverages, Halfnorms- Plots
# 2a. Leverages
lev <- hatvalues(g)
# the leverages = the diagonal entries of the Hat matrix (n by n);
lev <- diag(H) # the same as the previous command
plot(lev, main="Index plot of Leverages") #plot the leverages
abline(h=2*5/50) #cutoff h=2(p+1)/n

# Criterion for large leverage: Which countries have "large" leverage? We use a criterion which is a
# horizontal line at 2(p+1)/n
countries <- row.names(savings) #assign row names to "countries"
names(lev) <- countries
lev[lev > 0.2]

# 2b. Cooks distances
cook <- cooks.distance(g) #cooks.distance() gives the Cooks distance
plot(cook,ylab="Cooks distances") #plot Cooks distance
identify(1:50, cook,countries) #identify() useful for point device, click on points then the row.names will appear

# 2c. Halfnorm
dev.off()
halfnorm(cook, 4) #4 countries

# There are tests/plots to check ei ~ iid N(0, sigma^2)
# Assumption of independent and identically normal distributed errors with 0 mean and constant variance.

# 3a. Check Normality: QQ Plot; histogram plot; Shapiro-Wilk test
hist(g$residuals) #check for bell shape
qqnorm(g$residuals) #check for pattern of dots form closely around the line
qqline(g$residuals)

shapiro.test(g$residuals) # TEST: large p-value ( > .05) , support NULL: Normal distributed

# 3b. Check Zero Mean and Constant Variance: Residuals vs fitted values plot
plot(g$fitted, g$residuals); abline(h=0)

# 3c. Checking Correlations among residuals: Durbin-Watson test
install.packages("lmtest"); library(lmtest)
dwtest(g, alt="two.sided")

# What is the Null hypothesis and the p-value


# A. PRACTICE
# 1. POISSON REGRESSION : when the response is an integer count.
data(warpbreaks)
attach(warpbreaks)
boxplot(breaks ~ wool)
g_pois <- glm(breaks ~ wool+tension, family=poisson)
summary(g_pois)

waldtest(g, test="Chisq") # to test the overall fit of model
# Fitted values
fitted<- g_pois$fitted.values # the fitted values are just the averages.

# Prediction
pred <- predict(g_pois, type="response") # prediction values are the same as fitted values from model
pred

newdata <- data.frame(wool="A", tension="L")
predict(g_pois, newdata, type="response")


# PART 2. LOGISTIC REGRESSION -------------------

# A. Practice --------------------------
# 1. POISSON REGRESSION : when the response is an integer count.
data(warpbreaks) #install package datasets
head(warpbreaks) # type of wool (A or B) ; tension (L, M, H)

attach(warpbreaks)
boxplot(breaks ~ wool)
g_pois <- glm(breaks ~ wool+tension, family=poisson)
summary(g_pois)

waldtest(g_pois, test="Chisq") # to test the overall fit of model
# Fitted values
fitted<- g_pois$fitted.values # the fitted values are just the averages.

# Prediction
pred <- predict(g_pois, type="response") # prediction values are the same as fitted values from model
pred

newdata <- data.frame(wool="A", tension="L")
predict(g_pois, newdata, type="response")


# 2.LOGISTIC REGRESSION -------------------------------------
# Work for case where the response has 2 values: {0, 1}
data(pima)
head(pima)
attach(pima)
g <- glm( test ~., data=pima, family=binomial(link="logit"))
summary(g)

# Prediction: What is the chance a 60 years old person with high glucose and high bmi , to have diabetes?
library(RcmdrMisc)
stepwise(g) # reduce the model to less predictors
g_red <- glm(test~ pregnant+glucose+bmi+diabetes,data=pima, family=binomial(link="logit"))

newdata<- data.frame(pregnant=mean(pregnant), glucose=200, bmi=70, diabetes=mean(diabetes))
pred <- predict(g_red, newdata, se=TRUE, interval="prediction")
pred

a <- exp(4.787219) ; a
prob <- a/(a+1) # as in formula for p in the Note above
prob
#high probability the person will get diabetes