#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# A Practical Introduction to Regression Discontinuity Designs
# Authors: Matias D. Cattaneo, Nicolas Idrobo and Rocio Titiunik
#------------------------------------------------------------------------------#
# SOFTWARE WEBSITE: https://rdpackages.github.io/
#------------------------------------------------------------------------------#
# TO INSTALL/DOWNLOAD R PACKAGES/FUNCTIONS:
# FOREIGN: install.packages('foreign')
# GGPLOT2: install.packages('ggplot2')
# GRID: install.packages('grid')
# LPDENSITY: install.packages('lpdensity')
# RDDENSITY: install.packages('rddensity')
# RDLOCRAND: install.packages('rdlocrand')
# RDROBUST: install.packages('rdrobust')
# TEACHINGDEMOS: install.packages('TeachingDemos')
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# NOTE: if you are using RDROBUST version 2020 or newer, the option 
# masspoints="off" may be needed to replicate the results in the monograph.
# For example:
#    out = rdrobust(Y,X)
# should be replaced by:
#    out = rdrobust(Y,X,masspoints="off",stdvars="on")
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# NOTE: if you are using RDDENSITY version 2020 or newer, the option 
# masspoints=FALSE may be needed to replicate the results in the monograph.
# For example:
#    out = rddensity(X)
# should be replaced by:
#    out = rddensity(X,masspoints=FALSE)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
rm(list = ls())

library(foreign)
library(ggplot2)
library(lpdensity)
library(rddensity)
library(rdrobust)
library(rdlocrand)
library(TeachingDemos)

options(width = 300)
par(mar = rep(2, 4))

data = read.dta("CIT_2020_CUP_senate.dta")
Y = data$demvoteshfor2
X = data$demmv
T = (X >= 0)

#-------------------------------#
# Section 2                     #
# The Canonical Sharp RD Design #
#-------------------------------#
# Figure 3a
# Raw comparison of means
rdplot(Y, X, nbins = c(2500, 500), p = 0, col.lines = "red", col.dots = "lightgray", title = "", 
       y.lim = c(0,100))

# Figure 3b
# Local comparison of means
rdplot(Y[abs(X) <= 50], X[abs(X) <= 50], nbins = c(2500, 500), p = 4, col.lines = "red", col.dots = "lightgray", title = "", 
       y.lim = c(0,100))

#-----------#
# Section 3 #
# RD Plots  #
#-----------#
# R Snippet 1 (Figure 5)
# Scatter plot
plot(X, Y, xlab = "Score", ylab = "Outcome", col = 1, pch = 20)
abline(v = 0)

# Figure 6
# RD plot using 40 bins of equal length
out <- rdplot(Y, X, nbins = c(20,20), binselect = 'esmv')
summary(out)

# R Snippet 2 (Figure 7a)
# 40 Evenly-spaced bins
out <- rdplot(Y, X, nbins = c(20,20), binselect = 'es')
summary(out)

# R Snippet 3 (Figure 7b)
# 40 Quantile-spaced bins
out <- rdplot(Y, X, nbins = c(20,20), binselect = 'qs', x.lim = c(-100,100))
summary(out)

# R Snippet 4 (Figure 8)
# IMSE RD plot with evenly-spaced bins
out <- rdplot(Y, X,  binselect = 'es', x.lim = c(-100,100))
summary(out)

# R Snippet 5 (Figure 9)
# IMSE RD plot with quantile-spaced bins
out <- rdplot(Y, X,  binselect = 'qs', x.lim = c(-100,100))
summary(out)

# R Snippet 6 (Figure 10)
# Mimicking variance RD plot with evenly-spaced bins
out <- rdplot(Y, X,  binselect = 'esmv')
summary(out)

# R Snippet 7 (Figure 11)
# Mimicking variance RD plot with quantile-spaced bins
out <- rdplot(Y, X,  binselect = 'qsmv', x.lim = c(-100,100))
summary(out)

#----------------------------------------------#
# Section 4                                    #
# The Continuity-Based Approach to RD Analysis #
#----------------------------------------------#
# R Snippet 8
# Using two regressions to estimate
out <- lm(Y[X < 0 & X >= -10] ~ X[X < 0 & X >= -10])
left_intercept = out$coefficients[1]
print(left_intercept)
out <- lm(Y[X >= 0 & X <= 10] ~ X[X >= 0 & X <= 10])
right_intercept = out$coefficients[1]
print(right_intercept)
difference = right_intercept - left_intercept
print(paste("The RD estimator is", difference, sep = " "))

# R Snippet 9
# Using one regression to estimate
T_X <- X * T
out <- lm(Y[X >= -10 & X <= 10] ~ X[X >= -10 & X <= 10] + T[X >= -10 & X <= 10] + T_X[X >= -10 & X <= 10])
summary(out)

# R Snippet 10
# Generating triangular weights
w <- NA
w[X < 0 & X >= -10] <- 1 - abs(X[X < 0 & X >= -10] / 10)
w[X >= 0 & X <= 10] <- 1 - abs(X[X >= 0 & X <= 10] / 10)

# R Snippet 11
# Using two regressions and weights to estimate
out <- lm(Y[X < 0] ~ X[X < 0], weights = w[X < 0])
left_intercept <- out$coefficients[1]
out <- lm(Y[X >= 0] ~ X[X >= 0], weights = w[X >= 0])
right_intercept <- out$coefficients[1]
difference <- right_intercept - left_intercept
print(paste("The RD estimator is", difference, sep = " "))

# R Snippet 12
# Using rdrobust with uniform weights
out <- rdrobust(Y, X, kernel = 'uniform',  p = 1, h = 10)
summary(out)

# R Snippet 13
# Using rdrobust with triangular weights
out <- rdrobust(Y, X, kernel = 'triangular',  p = 1, h = 10)
summary(out)

# R Snippet 14
# Using rdrobust with triangular weights and p = 2
out <- rdrobust(Y, X, kernel = 'triangular',  p = 2, h = 10)
summary(out)

# R Snippet 15
# Using rdbwselect with mserd bandwidth
out <- rdbwselect(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)

# R Snippet 16
# Using rdbwselect with msetwo bandwidth
out <- rdbwselect(Y, X, kernel = 'triangular',  p = 1, bwselect = 'msetwo')
summary(out)

# R Snippet 17
# Using rdrobust with mserd bandwidth
out <- rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)

# R Snippet 18
# Using rdrobust to show the objects it returns
rdout <- rdrobust(Y, X, kernel = 'triangular', p = 1, bwselect = 'mserd')
print(names(rdout))
print(rdout$beta_Y_p_r)
print(rdout$beta_Y_p_l)

# R Snippet 19 (Figure 15)
# Using rdrobust and showing the associated rdplot
bandwidth <- rdrobust(Y, X, kernel = 'triangular', p = 1, bwselect = 'mserd')$bws[1,1]
out <- rdplot(Y[abs(X) <= bandwidth], X[abs(X) <= bandwidth], p = 1, kernel = 'triangular')
summary(out)

# R Snippet 20
# Using rdrobust without regularization term
out <- rdrobust(Y, X, kernel = 'triangular', scaleregul = 0,  p = 1, bwselect = 'mserd')
summary(out)

# R Snippet 21
# Using rdrobust with default options
out <- rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)

# R Snippet 22
# Using rdrobust with default options and showing all the output
out <- rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd', all = TRUE)
summary(out)

# R Snippet 23
# Using rdrobust with cerrd bandwidth
out <- rdrobust(Y, X, kernel = 'triangular', p = 1, bwselect = 'cerrd')
summary(out)

# R Snippet 24
# Using rdbwselect with all the bandwidths
out <- rdbwselect(Y, X, kernel = 'triangular', p = 1, all = TRUE)
summary(out)

# R Snippet 25
# Using rdbwselect with covariates
Z <- cbind(data$presdemvoteshlag1, data$demvoteshlag1, data$demvoteshlag2, 
          data$demwinprv1, data$demwinprv2, data$dmidterm, data$dpresdem, data$dopen)
colnames(Z) <- c("presdemvoteshlag1", "demvoteshlag1", "demvoteshlag2",
                "demwinprv1", "demwinprv2", "dmidterm", "dpresdem", "dopen")
out <- rdbwselect(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd')
summary(out)

# R Snippet 26
# Using rdrobust with covariates
Z <- cbind(data$presdemvoteshlag1, data$demvoteshlag1, data$demvoteshlag2, 
          data$demwinprv1, data$demwinprv2, data$dmidterm, data$dpresdem, data$dopen)
colnames(Z) <- c("presdemvoteshlag1", "demvoteshlag1", "demvoteshlag2",
                "demwinprv1", "demwinprv2", "dmidterm", "dpresdem", "dopen")
out <- rdrobust(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd')
summary(out)

#-----------------------------------------------#
# Section 5                                     #
# Validation and Falsification of the RD Design #
#-----------------------------------------------#
# Figure 16
# RD plots for predetermined covariates
rdplot(data$presdemvoteshlag1, X,
       x.label = "Score", y.label = "", title = "")

rdplot(data$demvoteshlag1, X,
       x.label = "Score", y.label = "", title = "")

rdplot(data$demvoteshlag2, X,
       x.label = "Score", y.label = "", title = "")

rdplot(data$demwinprv1, X,
       x.label = "Score", y.label = "", title = "")

rdplot(data$demwinprv2, X,
       x.label = "Score", y.label = "", title = "")

rdplot(data$dmidterm, X,
       x.label = "Score", y.label = "", title = "")

rdplot(data$dpresdem, X,
       x.label = "Score", y.label = "", title = "")

rdplot(data$dopen, X,
       x.label = "Score", y.label = "", title = "")

# R Snippet 29
# Using rdrobust on demvoteshlag1
out <- rdrobust(data$demvoteshlag1, X)
summary(out)

# Formal continuity-based analysis for covariates using CER-optimal bandwidth (not reported in the text)
summary(rdrobust(data$presdemvoteshlag1, X, bwselect = 'cerrd'))
summary(rdrobust(data$demvoteshlag1, X, bwselect = 'cerrd'))
summary(rdrobust(data$demvoteshlag2, X, bwselect = 'cerrd'))
summary(rdrobust(data$demwinprv1, X, bwselect = 'cerrd'))
summary(rdrobust(data$demwinprv2, X, bwselect = 'cerrd'))
summary(rdrobust(data$dmidterm, X, bwselect = 'cerrd'))
summary(rdrobust(data$dpresdem, X, bwselect = 'cerrd'))
summary(rdrobust(data$dopen, X, bwselect = 'cerrd'))

# R Snippet 30
# Using rdplot to show the rdrobust effect for demvoteshlag1
bandwidth <- rdrobust(data$demvoteshlag1, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$demvoteshlag1[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

# Figure 17
# Graphical illustration of local linear RD effects for predetermined covariates
bandwidth <- rdrobust(data$presdemvoteshlag1, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$presdemvoteshlag1[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

bandwidth <- rdrobust(data$demvoteshlag1, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$demvoteshlag1[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

bandwidth <- rdrobust(data$demvoteshlag2, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$demvoteshlag2[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

bandwidth <- rdrobust(data$demwinprv1, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$demwinprv1[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

bandwidth <- rdrobust(data$demwinprv2, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$demwinprv2[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

bandwidth <- rdrobust(data$dmidterm, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$dmidterm[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

bandwidth <- rdrobust(data$dpresdem, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$dpresdem[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

bandwidth <- rdrobust(data$dopen, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$dopen[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

# R Snippet 31
# Binomial test
binom.test(52, 102, 1/2)

# R Snippet 32
# Using rddensity
out <- rddensity(X)
summary(out)

# Figure 19a
# Histogram
bw_left <- as.numeric(rddensity(X)$h[1]); bw_right = as.numeric(rddensity(X)$h[2]);
tempdata <- as.data.frame(X); colnames(tempdata) = c("v1");
plot2 <- ggplot(data=tempdata, aes(tempdata$v1)) +
  geom_histogram(data = tempdata, aes(x = v1, y= ..count..), breaks = seq(-bw_left, 0, 1), fill = "blue", col = "black", alpha = 1) +
  geom_histogram(data = tempdata, aes(x = v1, y= ..count..), breaks = seq(0, bw_right, 1), fill = "red", col = "black", alpha = 1) +
  labs(x = "Score", y = "Number of Observations") + geom_vline(xintercept = 0, color = "black") +
  theme_bw()
plot2

# Figure 19b
# Estimated Density
est1 <- lpdensity(data = X[X < 0 & X >= -bw_left], grid = seq(-bw_left, 0, 0.1), bwselect = "IMSE",
                 scale = sum(X < 0 & X >= -bw_left) / length(X))
est2 <- lpdensity(data = X[X >= 0 & X <= bw_right], grid = seq(0, bw_right, 0.1), bwselect = "IMSE",
                 scale = sum(X >= 0 & X <= bw_right) / length(X))
plot1 <- lpdensity.plot(est1, est2, CIshade = 0.2, lcol = c(4, 2), CIcol = c(4, 2), legendGroups = c("Control", "Treatment"))+
  labs(x = "Score", y = "Density") + geom_vline(xintercept = 0, color = "black") +
  theme_bw()
plot1

# R Snippet 33
# Using rdrobust with the cutoff equal to 1
out <- rdrobust(Y[X >= 0], X[X >= 0], c = 1)
summary(out)

# R Snippet 34
# Using rdrobust for the donut-hole approach
out <- rdrobust(Y[abs(X) >= 0.3], X[abs(X) >= 0.3])
summary(out)