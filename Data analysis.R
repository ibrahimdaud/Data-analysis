# install packages
# install.packages("HSAUR")
# install.packages("ggplot2")

# initialising dataset 
data("water", package = "HSAUR")
my_data <- water

# split data into locations: north and south
split_location <- split(my_data, my_data$location)

north_data <- split_location$North

south_data <- split_location$South

# Scatter graph for hardness vs. mortality
library(ggpubr)
plot <- ggplot(my_data, aes(x=hardness, y=mortality, shape=as.factor(location), colour=as.factor(location))) + geom_point()
plot

# Scatter graph with Line of best fit
lm(my_data$mortality ~ my_data$hardness)

Call:
lm(formula = my_data$mortality ~ my_data$hardness)

Coefficients:
(Intercept)  my_data$hardness
1676.356          -3.226

plot(my_data$hardness, my_data$mortality, main = "Scatter graph with Line of Best Fit" , col = "brown")
abline(a=1676.356, b=-3.226, col ="green", lwd=1)

# Pearson's correlation coefficient
cor.test(my_data$hardness, my_data$mortality, method="pearson", data =datacorrelation)


# Scatter graph comparing hardness vs. mortality in North only
plot(north_data$hardness, north_data$mortality, xlab="hardness", ylab="mortality", main="Hardness vs. Mortality in North", col = "brown")
# Scatter graph comparing hardness vs.  mortality in South only
plot(south_data$hardness, south_data$mortality, xlab="hardness", ylab="mortality", main="Hardness vs. Mortality in South", col = "brown")

# Scatter graph with Line of best fit comparing hardness vs. mortality in North only
lm(north_data$mortality ~ north_data$hardness)
plot(north_data$hardness, north_data$mortality, main = "Hardness vs. Mortality in North", col = "brown", xlab = "hardness", ylab = "mortality")
abline(a=1692.313, b=-1.931, col ="green", lwd=1)
# Scatter graph with Line of best fit comparing hardness vs. mortality in South only
lm(south_data$mortality ~ south_data$hardness)
plot(south_data$hardness, south_data$mortality, main = "Hardness vs. Mortality in South" , col = "brown", xlab = "hardness", ylab = "mortality")
abline(a=1522.815, b=-2.093, col ="green", lwd=1)

# Pearson's correlation coefficient for North only
cor(north_data$hardness, north_data$mortality, method = c("pearson"))
# Pearson's correlation coefficient for South only
cor(south_data$hardness, south_data$mortality, method = c("pearson"))

# summary for hardness in north
summary(north_data$hardness)

# summary for mortality in north
summary(north_data$mortality)

# summary for hardness in south
summary(south_data$hardness)

# summary for mortality in south
summary(south_data$mortality)

library(ggpubr)
# Box plots
# Box plot for hardness in north
ggboxplot(north_data$hardness, ylab="hardness", xlab=FALSE, main="Hardness in the North")

# Box plot for mortality in north
ggboxplot(north_data$mortality, ylab="mortality", xlab=FALSE, main="Mortality in the North")

# Box plot for hardness in south
ggboxplot(south_data$hardness, ylab="hardness", xlab=FALSE, main="Hardness in the South")

# Box plot for mortality in south
ggboxplot(south_data$mortality, ylab="mortality", xlab=FALSE, main="Mortality in the South")

# Shapiro-Wilk tests 
library(ggpubr)
# Shapiro-Wilk test for hardness in entire dataset
shapiro.test(my_data$hardness)

# Shapiro-Wilk test for mortality in entire dataset
shapiro.test(my_data$mortality)

# Shapiro-Wilk tests 
library(ggpubr)
# Shapiro-Wilk test for hardness in north only
shapiro.test(north_data$hardness)

# Shapiro-Wilk test for mortality in north only
shapiro.test(north_data$mortality)

# Shapiro-Wilk test for hardness in south only
shapiro.test(south_data$hardness)

# Shapiro-Wilk test for mortality in south only
shapiro.test(south_data$mortality)

# QQ plots
library(ggpubr) 
# QQ plots for hardness in north
ggqqplot(north_data$hardness, ylab="hardness", main="Hardness in the North")

# QQ plots for mortality in north
ggqqplot(north_data$mortality, ylab="mortality", main="Mortality in the North")

# QQ plots for hardness in south
ggqqplot(south_data$hardness, ylab="hardness", main="Hardness in the South")

# QQ plots for mortality in south
ggqqplot(south_data$mortality, ylab="mortality", main="Mortality in the South")

# t-tests for mortality in North and South
t.test(north_data$mortality, south_data$mortality)