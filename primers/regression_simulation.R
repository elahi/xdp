library(tidyverse)

##### Simulate the relationship between grade and income

## Simulate grades
n_students <- 30
grade_mean <- 86
grade_sd <- 6
set.seed(22)
grades <- rnorm(n = n_students, mean = grade_mean, sd = grade_sd)
hist(grades)
grades[grades > 100] <- 100

## True (population) regression 
alpha <- 5000
beta <- 500
set.seed(12)
error <- rnorm(n = n_students, mean = 0, sd = 2000)
incomes <- alpha + beta * grades + error

##### Plot regression

fit1 <- lm(incomes ~ grades)
fit1

qplot(x = grades, y = incomes) + 
  geom_smooth(method = "lm", color = "red") + 
  geom_abline(slope = beta, intercept = alpha, linetype = "dashed")

qplot(x = grades, y = incomes) + 
  geom_smooth(method = "lm") + 
  geom_abline(slope = beta, intercept = alpha, linetype = "dashed") + 
  scale_x_continuous(limits = c(-10, 100)) + 
  scale_y_continuous(limits = c(-10000, 60000)) + 
  geom_abline(slope = coef(fit1)[2], 
              intercept = coef(fit1)[1], 
              linetype = "dashed", color = "red") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

summary(fit1)
anova(fit1)

##### Estimate a and b

## empirical mean of the grades
xbar <- mean(grades)
ybar <- mean(incomes)

## sum of products
sum_products <- sum((grades - xbar) * (incomes - ybar))

## sum of squares of grades
ss_grades <- sum((grades - xbar)^2)

## estimate b
b <- sum_products / ss_grades

## estimate a
a <- ybar - b * xbar

##### Estimate SEb

## sum of squares of residuals
resids <- as.numeric(fit1$residuals)
ss_resids <- sum(resids^2)

## mean square residuals (i.e., mean square error)
ms_resids <- ss_resids / (n_students - 2)
ms_resids

## standard error of slope
se_slope <- sqrt(ms_resids / ss_grades)
se_slope

summary(fit1)

##### Estimate residual standard error and R2

## residual standard error
se_resids <- sqrt((1 / (n_students - 2)) * ss_resids) 
se_resids

## sum of squares of incomes (i.e., total sum of squares)
ss_incomes <- sum((incomes - ybar)^2)

## R2
r_squared <- (ss_incomes - ss_resids) / (ss_incomes)
r_squared

summary(fit1)


                 