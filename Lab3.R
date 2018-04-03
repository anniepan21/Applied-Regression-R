# Lab 3
# Contents: 
#   - Multiple Linear Regression
#   - Model matrix 
#   - Regression coefficients 
#   - Inference on coefficients 
#   - ANOVA - Reduced Models
#   - Prediction.


library(faraway)

# A. PRACTICE
# A1. Multiple linear regression model

data("gala")
dim(gala)
head(gala)
names(gala) # variables (or column headings)
row.names(gala) # the individual name in each row, without a column heading
g <- lm(Species ~ Endemics+ Area + Elevation + Nearest + Scruz + Adjacent, data=gala)
summary(g)


# A2. Model matrix X -Coefficient estimates

gala_1 <- gala[,-1]  # gala taken out the 1st column, Species
ones <- rep(1,30)  # column vector of 1's
X <- cbind(ones, gala_1)  # add column of 1's to gala_1 to form X. X is the design matrix of g
X <- as.matrix(X) # convert X into a matrix to do matrix operations

y <- gala$Species # species the response variable, is saved as y
XtX <- t(X)%*%X  ## t(): transpose of x ; %*% does matrix multiplication:
XtX
XtX_inv <- solve(t(X)%*%X)  # solve() gives the inverse matrix
XtX_inv
coefs <- XtX_inv %*% t(X) %*% y #coefficients estimates
coefs

g$coefficients #same as in the g-output

# Coefficient standard errors
diag <- diag(XtX_inv)  # the diagonal entries of XtX_inv
sigma <- 28.96  # Residual standard error
coef_st_errors <- sigma* sqrt(diag)
coef_st_errors

confint(g)  # 95% CI's for coefficients

# 1. Residuals standard error
names(g) #all components of g; pay attention to coefficients, residuals and fitted.values
SSE<- sum(g$residuals^2) # Sum Square Error
res_standerror <- sqrt(SSE/(30-7)) # df of SSE is (n-p-1); p =6

# 2. ANOVA: SST, SSE, SSR; Coefficient of determination: R^2 
SST <- sum((y - mean(y))^2) # SS Total
R_square <- 1-(SSE/SST) #Coefficient of Determination R^2
R_square
# SSR <- SST - SSE


# 3. ANOVA F-statistics and P-value
df1 <- 6 #df(SSR) = p
df2 <- 23 #df(SSE)= n-p-1
F <- (SSR/df1)/(SSE/df2) #F-statistics, F-dist(df=p , df=n-p-1)
F
1 -pf(F, df1, df2) #P-value

# 4. REDUCED MODELS; anova()
g_red <- lm(Species ~ . -Nearest-Scruz-Adjacent, data=gala) #a reduced model
summary(g_red)
anova(g, g_red) #compare the 2 models, using ANOVA

SSE_g <- sum(g_red$residuals^2) #Sum Squares Residuals of g_red
df_1 <- 23 #df (SSE) of full model
SSE_g_red <- sum(g_red$residuals^2) #Sum Squares Residuals of g_red

df_2<- 26 #df(SSE) of reduced model
F_stat <- ( (SSE_g_red -SSE_g)/(df_2 -df_1) )/(SSE_g/df_1) #F_stat
pf <- pf(F_stat, df_2-df_1, df_1)
pvalue <- 1-pf
pvalue


-----------------------------------------
#  Note: Hypothesis.
#  Ho: g and g_red are the significantly the same.[ That means: g_red is better since less variables]
#  Ha: g and g_red are significantly different
#  (b) Read the P-value of the F-statistics. Notice the remaining coefficients not changing much, and s and R^2 are reduced.
  

#  Notes:
#  (1) By taking out all variables except Endemics, R^2 does not change much. 
#      We say Area, Elevation, Nearest, Scruz Adjacent account for a small percentage to "explain" Species.
#  (2) Since the coefficient estimates do not change much between the 2 models, 
#      we say that the variable vectors are almost ORTHOGONAL to one another. 
#      Taking out some variables does not change much the work of the remaining variables.
#      If the predictors are not orthogonal (there exist some correlations), there are considerable changes in the coefficient estimates between the 2 models.


# 5. Predictions and CI's for predictions: predict()
newdata <- data.frame(Endemics= 100,Area=mean(gala$Area),Elevation=mean(gala$Elevation),Nearest=mean(gala$Nearest),Scruz=mean(gala$Scruz),Adjacent=mean(gala$Adjacent)) #from new data for prediction
predict(g, newdata ,se=T)
