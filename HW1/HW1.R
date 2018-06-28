x = rnorm(50)
mean(x)
sd(x)
median(x)
e = rnorm(50, sd = 0.05)
y = 10 * x + e
plot(x, y, main  = "scatter plot for x, y", xlab = "x", ylab = "y")
hist(y, main = "histogram of y")
boxplot(y, main = "boxplot of y")