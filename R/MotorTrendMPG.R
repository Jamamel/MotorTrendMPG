library(GGally)
library(ggplot2)

data(mtcars)
sapply(mtcars, class)
View(mtcars)

mtcars$am <- factor(mtcars$am, levels = c(0,1), labels = c('automatic', 'manual'))
mtcars$vs <- factor(mtcars$vs, levels = c(0,1), labels = c('v', 'straight'))
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$cwt <- scale(mtcars$wt, scale = FALSE)
mtcars$cdisp <- scale(mtcars$disp, scale = FALSE)
mtcars$cmpg <- scale(mtcars$mpg, scale = FALSE)
mtcars$toy <- ifelse(rownames(mtcars) == 'Toyota Corolla',1,0)
mtcars$fiat <- ifelse(rownames(mtcars) == 'Fiat 128',1,0)


ggpairs(mtcars,
        colour = 'am',
        columns = c(1,3:7,9))

ggpairs(mtcars,
        colour = 'am',
        columns = c(1:2,8:11))

# First model reveals a possible relationship of wt and hp when all other effects present. 
# Good fit but mostly driven by large number of parameters. 
fit0 <- lm(mpg ~ ., mtcars)
summary(fit0)

# Large residuals and the assumption of Normality in the regressions error terms appears not to be accurate.
par(mfrow = c(2,2))
plot(fit0)

# By defining a model where MPG is explained by factor effect of transmition type and weight along with their interaction. Number of cylinders is also introduced as factor. Weight is centred to improve interpretability.
mtcars$cwt <- scale(mtcars$wt, scale = FALSE)
mtcars$toy <- ifelse(rownames(mtcars) == 'Toyota Corolla',1,0)
mtcars$fiat <- ifelse(rownames(mtcars) == 'Fiat 128',1,0)


# fit <- lm(mpg ~ am * cwt + cyl, mtcars)
fit <- lm(cmpg ~ am : cwt + cyl:cwt + am + cyl + toy + fiat, mtcars)
summary(fit)

# Adjustment is not much worse than when all variables (and its high number of parameters) were used. In addition, all coefficients have low enough standard errors to be acceptable for the author (especially given the low sample size given the complex relationships present and the limitation of tools allowed in this exercise). 

# Residual variance is lower and the assumption of Normality, while not perfect, is better founded. Four cars have a similar patterns that aren't easily isolated with simple linear regression.  However, fit and parsimony of the model were deemed acceptable.
par(mfrow = c(2,2))
plot(fit)

# Lower fit, greater residual variance, less nice QQ plot. General lower accuracy observed.
fit2 <- lm(mpg ~ am + cyl, mtcars)
summary(fit2)
par(mfrow = c(2,2))
plot(fit2)


# Still a mediocre model with slightly lower residual variance than fit2. The rogue group of cars is larger, meaning we don't describe enough patterns with as much consistency.
fit3 <- lm(mpg ~ am + cyl + wt, mtcars)
summary(fit3)
par(mfrow = c(2,2))
plot(fit3)



mtcars %>%
  group_by(am) %>%
  summarise(mpg = round(mean(mpg), 0),
            weight = round(mean(wt), 1),
            cylinders = names(table(cyl))[match(max(table(cyl)), table(cyl))])


