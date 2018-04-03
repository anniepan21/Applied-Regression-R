### Lab 5 Indicator variables - Factors - ANOVA

# A. PRACTICE -----------------------------------------------------------------------------
# 1. Example - data(cathedral) ---------------------------------------
# (1 factor with two levels and 1 continuous variable)
library(faraway)
data("cathedral")
attach(cathedral)
str(cathedral)

class(style)
levels(style)
lapply(split(cathedral, style),summary) # a summary wrt style factor

tapply(x,style, mean) #means of x wrt to style factor
tapply(x,style, sd) #standard deviation of x,wrt to style
tapply(y,style, mean)

boxplot(x~style)
boxplot(y~style)

pairs(cathedral)

g <- lm(y ~ x+style+x:style, cathedral) # using interaction term x:style
summary(g)

#   **Note: (a) styler has 2 values {0, 1}. x:styler is continous   
#   (b) x:styler (the interaction term ) is not significant.**


# ANOVA with anova()
model.matrix(g)
g_red <- lm(y ~ x+style, cathedral) # no interaction term
summary(g_red)
anova(g,g_red)

# no intercept model
g_cat <- lm(y~.-1, cathedral)
summary(g_cat)


# 2. Example - data(twins) ----------------------------------
# 1 factor with three levels and 1 continuous variable
data(twins)
attach(twins)
class(Social)
levels(Social)

g<- lm(Foster ~ Biological * Social, twins) #linear model with Biological(continuous) + Social (3 levels) + interaction between Biological and Social (3 levels)
summary(g)

##   **(a) Social High is not listed= the reference(Sociallow=0; Socialmiddle=0 )
##   (b) Some coefficients are non-significant, especially the interaction terms.**

# no intercept model
g_twins <- lm(Foster ~ . -1, twins)
summary(g_twins)

##   **Interaction term: To check if the parallel lines model (no interaction) is sufficient,
##    we build a reduced model without the interaction term and compare


g_red <- lm(Foster ~ Biological +Social -1 , data=twins) # drop the interaction term
summary(g_red)

anova(g, g_red)

### 3. Example (2 factors, each with levels, no continuous variable)
data("ToothGrowth")
attach(ToothGrowth)
str(ToothGrowth)
class(supp)
levels(supp)

dose <- factor(dose)
levels(dose)

lapply(split(ToothGrowth, supp, dose), summary) #summary of len, wrt to factors supp and dose
tapply(len, supp, mean)
tapply(len, dose, mean)

g <- lm(len ~ supp + dose) # model with no interaction term
summary(g)
#Note: (a) dose has 3 levels { .5, 1 , 2}. (b) supp has 2 level: OJ and VC.

# Or use No intercept model:
g_tooth <- lm( len ~. -1, ToothGrowth)
summary(g_tooth)

# Check Interaction between factors: interaction.plot()
interaction.plot(supp, dose, len, ToothGrowth)
interaction.plot(dose, supp, len, ToothGrowth)

# There is some interactions between dose and supp. So build a linear model with interaction
g_int <- lm( len ~ (dose + supp)^2, ToothGrowth)
summary(g_int)

# t-test of 2 samples [can think about data under 2 different treatments]
y1 <- c(16.85,16.40,17.21,16.35,16.52,17.04,16.96,17.15,16.59,16.57)
y2 <- c(16.62,16.75,17.37,17.12,16.98,16.87,17.34,17.02,17.08,17.27)
y <- data.frame(y1,y2)
names(y) <-c("Modified", "Unmodified")
y.means <- as.numeric(apply(y,2,mean))
t.test(mod,unmod,var.equal=TRUE) # with equal variance assumption)
t.test(mod, unmod,paired=T)

### B. EXERCISE -----------------------------------------------------------------------------
library(faraway)
data("coagulation")
head(coagulation)
attach(coagulation)
levels(diet)

# 1. Make a summary wrt to factor diet (treatments) 
lapply(split(coagulation, diet),summary)

# 2. Find the means of the groups under treatments. [ use tapply() ]
tapply(coag, diet, mean)

# 3.  Find the standard deviation of the groups under treatments [use tapply() ]
tapply(coag, diet, sd)

# 4. Perform a boxplot
boxplot(coag ~ diet)

# 5. Build a linear model : coag ~ diet, without the Intercept
g <- lm(coag~ diet-1, coagulation)
summary(g)

#(a) Write the regression equation.
#y (coag) = 61 (DietA) +66 (DietB) + 68 (DietC) + 61 (DietD) 
#(b) Explain variables: dietA, dietB, dietC, dietD
#(c) How to find fitted values from the model for : Fitted(diet A) = 61; Fitted(dietB)=66; Fitted(dietC)=68,fitted(diet D)=61.
fitted <- g$fitted
fitted