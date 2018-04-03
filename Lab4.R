### Lab4
###   Contents: 
###     Orthogonality among Predictors
###     Principal Component Analysis
###     Multicollinarity
###     Power and Log Transformation
###     Polynomial Regression

### A. PCA - The matrix of predictors Y, eigen system of YtY, after clustering and scaling ------------------------------
library(faraway)
data("gala")
str(gala)

### take out the Species, all predictors remain
Y <- gala[,-1]

### covariance matrix
covar <- cov(Y)

### correlation matrix
corr <- cor(Y)

### gives the eigenvalues
eigen(corr)

gala.pca <- prcomp(Y, center=TRUE, scale=TRUE) 
names(gala.pca)
gala.pca
summary(gala.pca)
plot(gala.pca, type="l")

### B. Multicollinearity: problem with the (XtX) matrix ----------------------------
g <- lm(Species ~ ., data=gala)
X <- model.matrix(g)
e <- eigen(t(X)%*%X)
signif(e$vectors,3)
plot(e$values,type="l")

### check the condition number k
k <- sqrt(3.66e+07 / 9.44e+00)  # the condition number
k

### check the VIF's of each predictor
vif(g) # the Variance Inflation Factor VIF
plot(vif(g))


### C. Power transformations - Log transformation ----------------------------------
install.packages("MASS")
library(MASS)
install.packages("alr3")
library(alr3)

defective <- read.table(file.choose(),header=TRUE)
attach(defective); head(defective)
pairs(defective,pch=16) # the scatterplot

### C1. Transformation the response only, using boxcox()
defective.g <- lm(Defective~Temperature+Density+Rate, data=defective)
summary(defective.g)
bc <- boxcox(defective.g, lambda = seq(0,1,0.1), plotit = TRUE)

### C2. Transformation of the response only, using inverse response plot
inverse.response.plot(defective.g, key=TRUE)

defective.g_new <- lm(sqrt(Defective) ~ Temperature+Density+Rate, data=defective)
summary(defective.g_new)
plot(x=defective.g$fitted.values, y=sqrt(Defective), pch=16)
abline(lsfit(defective.g$fitted.values, sqrt(Defective)))

### C3. Transformation of BOTH response and predictors
library(car)
trans <- powerTransform(cbind(Defective,Density,Rate,Temperature)~1, data=defective)
summary(trans)

### D. Polynomial regression ----------------------------------------------------------------
attach(savings)
pairs(savings, pch=16)

savings.g <- lm(sr~., data=savings) # linear regression
savings.g1 <- lm(sr~ pop15+poly(pop75,2)+poly(dpi,2)+ddpi, savings) #polynomial regression, linear and squared term for pop75 and dpi.

summary(savings.g)
summary(savings.g1)

### Power and Log transformation
savings.trans <- powerTransform(cbind(sr,pop15,pop75,dpi,ddpi)~1, savings)
summary(savings.trans)

# pop75 needs square root; dpi and ddpi need log transform
savings.g2 <- lm(sr~pop15+sqrt(pop75)+log(dpi)+log(ddpi), savings)
summary(savings.g2)


### EXERCISE -----------------------------------------------------------------------------------------------------

### 1. Data: cars04.csv
cars <- read.csv(file.choose(), header=TRUE)

### (a) Perform scatterplot
pairs(cars,pch=16)
C <- cars[,-1]

### (b) Build a linear model, check ANOVA F-test
cars.g <- lm(SuggestedRetailPrice ~., data=C)
summary(cars.g)
anova(cars.g)
### All bi are equal vs Not all bi = 0
### p-value < 2.2e-16
### If p < alpha, we reject H0
### We reject H0

### R^2 = 0.9989

### Horsepower, CityMPG, HighwayMPG, WheelBase p-value > .05
### These predictors are not significant


### (c) --------------------------------------------------------------
### Peform a PCA
C.pca <- prcomp(C, center=TRUE, scale=TRUE) 
C.pca
summary(C.pca)
plot(C.pca, type="l")

### PC5-PC12 can be ignored.


### Perform powerTransform() 
library(car)
cars.trans <- powerTransform(cbind(SuggestedRetailPrice, DealerCost, EngineSize, Cylinders, Horsepower, CityMPG, HighwayMPG, Weight, WheelBase, Length, Width)~1, data=C)
summary(cars.trans)

### Perform boxcox()
bc <- boxcox(cars.g, lambda = seq(0,1,0.1), plotit = TRUE)
### EngineSize need square root; Cylinders, Horsepower needs log transformation.

### 2. Data: magazines.csv
mag <- read.csv(file.choose(), header = TRUE)
attach(mag)
pairs(mag, pch=16)
str(mag)
M <- mag[,-1]


### (a) Build a linear model using AdRevenue vs others
g <- lm(AdRevenue~., data=M)
summary(g)

### (b) Perform Power Transformation of BOTH response and predictors
mag.trans <- powerTransform(cbind(AdRevenue,AdPages,SubRevenue,NewsRevenue)~1, M)
summary(mag.trans)
### AdRevenue, AdPages, SubRevenue, and NewsRevenue needs log transformation

### (c) Perform a boxcox() procedure
mag.bc <- boxcox(g, lambda = seq(0,1,0.1), plotit = TRUE)

### (d) Build a new model using these transformations
g2 <- lm(log(AdRevenue) ~ log(AdPages) + log(SubRevenue)+ log(NewsRevenue), data=M)
summary(g2)

### (e) Plot Response and Fitted
plot(g2$fitted.values, log(AdRevenue), pch=16)
abline(lsfit(g2$fitted.values, log(AdRevenue)))
