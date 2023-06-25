## Import dataset Nations2.dta using package ‘haven’
library(haven)
data <- read_stata("C:/Users/Isha/Downloads/Nations2.dta")

## Exploratory analysis
# 1. explore data and its format
head(data)

#2. explore the datatypes of variables
sapply(data, FUN=class)

#3. explore correlation between the variables
nums <- unlist(lapply(data, is.numeric))
data[ , nums]
cormat <- cor(data[, nums],use='pairwise.complete.obs')
library(corrplot)
corrplot(cormat,type = 'lower')

## Running regression taking life expectancy as dependent variable and region, gdp, school, adfert,
#chldmort, life, pop, urban, femlab, literacy, co2 and gini as independent variables.
#1. Running regression and obtaining results
m1<-lm(life~region+ gdp+ school+ adfert+ chldmort+ pop+ urban+ femlab+ literacy+ co2+ gini,
data=data)
summary(m1)

#2. Plotting the regression of life expectancy against each of the independent variables
plot(life~region+ gdp+ school+ adfert+ chldmort+ pop+ urban+ femlab+ literacy+ co2+ gini, data=data)
abline(m1)

## Testing for assumptions of linear regression
#1. checking heteroscedasticity by plotting residual vs fitted values
plot(m1$resid~m1$fitted.values)
abline(h=0,lty=2)

#2. checking normality by checking if quantile to quantile plot is diagonal
qqnorm(m1$resid)
qqline(m1$resid)

#3. checking for multicollinearity
avPlots(m1)
car::vif(m1)

#4. checking linearity
residualPlots(m1)
