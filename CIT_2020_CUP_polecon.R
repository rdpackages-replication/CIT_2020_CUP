#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# A Practical Introduction to Regression Discontinuity Designs: Foundations
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
#    out = rddensity(X,massPoints=FALSE)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
rm(list=ls())

library(foreign)
library(ggplot2)
library(lpdensity)
library(rddensity)
library(rdrobust)
library(rdlocrand)

# Tables 1, 2, 4, 5, and 6 are only constructed in STATA
# Figures 20, 21, and 22 are only constructed in STATA

# Loading the data and defining the main variables
data <- read.dta("CIT_2020_CUP_polecon.dta")
Y <- data$Y
X <- data$X
T <- data$T
T_X <- T*X

#---------------------#
# Section 2           #
# The Sharp RD Design #
#---------------------#
# Figure 3a
# Raw comparison of means
rdplot(Y, X, nbins = c(2500, 500), p = 0, col.lines = "red", col.dots = "black", title = "", 
       x.label = "Islamic Margin of Victory", y.label = "Female High School Percentage", y.lim = c(0,70))

# Figure 3b
# Local comparison of means
rdplot(Y[abs(X) <= 50], X[abs(X) <= 50], nbins = c(2500, 500), p = 4, col.lines = "red", col.dots = "black", title = "", 
       x.label = "Islamic Margin of Victory", y.label = "Female High School Percentage", y.lim = c(0,70))

#-----------#
# Section 3 #
# RD Plots  #
#-----------#
# R Snippet 1 (Figure 5)
# Scatter plot
plot(X, Y, xlab = "Score", ylab = "Outcome", col = 1, pch = 20)
abline(v=0)

# Figure 6
# RD plot using 40 bins of equal length
out <- rdplot(Y, X, nbins = c(20,20), binselect = 'esmv', y.lim = c(0,25))
summary(out)

# R Snippet 2 (Figure 7a)
# 40 Evenly-spaced bins
out <- rdplot(Y, X, nbins = c(20,20), binselect = 'es', y.lim = c(0,25))
summary(out)

# R Snippet 3 (Figure 7b)
# 40 Quantile-spaced bins
out <- rdplot(Y, X, nbins = c(20,20), binselect = 'qs', x.lim = c(-100,100), y.lim = c(0,25))
summary(out)

# R Snippet 4 (Figure 8)
# IMSE RD plot with evenly-spaced bins
out <- rdplot(Y, X,  binselect = 'es', x.lim = c(-100,100), y.lim = c(0,25))
summary(out)

# R Snippet 5 (Figure 9)
# IMSE RD plot with quantile-spaced bins
out <- rdplot(Y, X,  binselect = 'qs', x.lim = c(-100,100), y.lim = c(0,25))
summary(out)

# R Snippet 6 (Figure 10)
# Mimicking variance RD plot with evenly-spaced bins
out <- rdplot(Y, X,  binselect = 'esmv')
summary(out)

# R Snippet 7 (Figure 11)
# Mimicking variance RD plot with quantile-spaced bins
out <- rdplot(Y, X,  binselect = 'qsmv', x.lim = c(-100,100), y.lim = c(0,25))
summary(out)

#----------------------------------------------#
# Section 4                                    #
# The Continuity-Based Approach to RD Analysis #
#----------------------------------------------#
# R Snippet 8
# Using two regressions to estimate
out <- lm(Y[X < 0 & X >= -20] ~ X[X < 0 & X >= -20])
left_intercept = out$coefficients[1]
print(left_intercept)
out <- lm(Y[X >= 0 & X <= 20] ~ X[X >= 0 & X <= 20])
right_intercept <- out$coefficients[1]
print(right_intercept)
difference <- right_intercept - left_intercept
print(paste("The RD estimator is", difference, sep = " "))

# R Snippet 9
# Using one regression to estimate
T_X <- X * T
out <- lm(Y[X >= -20 & X <= 20] ~ X[X >= -20 & X <= 20] + T[X >= -20 & X <= 20] + T_X[X >= -20 & X <= 20])
summary(out)

# R Snippet 10
# Generating triangular weights
w <- NA
w[X < 0 & X >= -20] <- 1 - abs(X[X < 0 & X >= -20] / 20)
w[X >= 0 & X <= 20] <- 1 - abs(X[X >= 0 & X <= 20] / 20)

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
out <- rdrobust(Y, X, kernel = 'uniform',  p = 1, h = 20)
summary(out)

# R Snippet 13
# Using rdrobust with triangular weights
out <- rdrobust(Y, X, kernel = 'triangular',  p = 1, h = 20)
summary(out)

# R Snippet 14
# Using rdrobust with triangular weights and p = 2
out <- rdrobust(Y, X, kernel = 'triangular',  p = 2, h = 20)
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
print(names(rdout)[1:7])
print(names(rdout)[8:15])
print(names(rdout)[16:23])
print(names(rdout)[24:27])
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
Z <- cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
colnames(Z) <- c("vshr_islam1994", "partycount", "lpop1994",
                "merkezi", "merkezp", "subbuyuk", "buyuk")
out <- rdbwselect(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd')
summary(out)

# R Snippet 26
# Using rdrobust with covariates
Z <- cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
colnames(Z) <- c("vshr_islam1994", "partycount", "lpop1994",
                "merkezi", "merkezp", "subbuyuk", "buyuk")
out <- rdrobust(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd')
summary(out)

# R Snippet 27
# Using rdrobust with clusters
out <- rdrobust(Y, X, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd', cluster = data$prov_num)
summary(out)

# R Snippet 28
# Using rdrobust with clusters and covariates
Z <- cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
colnames(Z) <- c("vshr_islam1994", "partycount", "lpop1994",
                "merkezi", "merkezp", "subbuyuk", "buyuk")
out <- rdrobust(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, 
                bwselect = 'mserd', cluster = data$prov_num)
summary(out)

#-----------------------------------------------#
# Section 5                                     #
# Validation and Falsification of the RD Design #
#-----------------------------------------------#
# Figure 16
# RD plots for predetermined covariates
rdplot(data$lpop1994, X,
       x.label = "Score", y.label = "", title = "")

rdplot(data$partycount, X,
       x.label = "Score", y.label = "", title = "")

rdplot(data$vshr_islam1994, X,
       x.label = "Score", y.label = "", title = "")

rdplot(data$i89, X,
       x.label = "Score", y.label = "", title = "", x.lim = c(-100,100))

rdplot(data$merkezp, X,
       x.label = "Score", y.label = "", title = "")

rdplot(data$merkezi, X,
       x.label = "Score", y.label = "", title = "")

# R Snippet 29
# Using rdrobust on lpop1994
out <- rdrobust(data$lpop1994, X)
summary(out)

# Formal continuity-based analysis for covariates using CER-optimal bandwidth (not reported in the text)
summary(rdrobust(data$hischshr1520m, X, bwselect = 'cerrd'))
summary(rdrobust(data$i89, X, bwselect = 'cerrd'))
summary(rdrobust(data$vshr_islam1994, X, bwselect = 'cerrd'))
summary(rdrobust(data$partycount, X, bwselect = 'cerrd'))
summary(rdrobust(data$lpop1994, X, bwselect = 'cerrd'))
summary(rdrobust(data$merkezi, X, bwselect = 'cerrd'))
summary(rdrobust(data$merkezp, X, bwselect = 'cerrd'))
summary(rdrobust(data$subbuyuk, X, bwselect = 'cerrd'))
summary(rdrobust(data$buyuk, X, bwselect = 'cerrd'))
        
# R Snippet 30
# Using rdplot to show the rdrobust effect for lpop1994
bandwidth <- rdrobust(data$lpop1994, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$lpop1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

# Figure 17
# Graphical illustration of local linear RD effects for predetermined covariates
bandwidth <- rdrobust(data$lpop1994, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$lpop1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

bandwidth <- rdrobust(data$partycount, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$partycount[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

bandwidth <- rdrobust(data$vshr_islam1994, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$vshr_islam1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

bandwidth <- rdrobust(data$i89, X)$bws[1,1]
xlim <- ceiling(bandwidth)
rdplot(data$i89[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

# R Snippet 31
# Binomial test
binom.test(53, 100, 1/2)

# R Snippet 32
# Using rddensity
out <- rddensity(X)
summary(out)

# Figure 19a
# Histogram
bw_left <- as.numeric(rddensity(X)$h[1]); bw_right = as.numeric(rddensity(X)$h[2]);
tempdata <- as.data.frame(X); colnames(tempdata) = c("v1");
plot2 <- ggplot(data=tempdata, aes(tempdata$v1)) + theme_bw(base_size = 17) +
  geom_histogram(data = tempdata, aes(x = v1, y= ..count..), breaks = seq(-bw_left, 0, 1), fill = "blue", col = "black", alpha = 1) +
  geom_histogram(data = tempdata, aes(x = v1, y= ..count..), breaks = seq(0, bw_right, 1), fill = "red", col = "black", alpha = 1) +
  labs(x = "Score", y = "Number of Observations") + geom_vline(xintercept = 0, color = "black")
plot2

# Figure 19b
# Estimated Density
est1 <- lpdensity(data = X[X < 0 & X >= -bw_left], grid = seq(-bw_left, 0, 0.1), bwselect = "IMSE",
                 scale = sum(X < 0 & X >= -bw_left) / length(X))
est2 <- lpdensity(data = X[X >= 0 & X <= bw_right], grid = seq(0, bw_right, 0.1), bwselect = "IMSE",
                 scale = sum(X >= 0 & X <= bw_right) / length(X))
plot1 <- lpdensity.plot(est1, est2, CIshade = 0.2, lcol = c(4, 2), CIcol = c(4, 2), legendGroups = c("Control", "Treatment"))+
  labs(x = "Score", y = "Density") + geom_vline(xintercept = 0, color = "black") +
  theme_bw(base_size = 17)+theme(legend.position = c(0.8, 0.85))
plot1

# R Snippet 33
# Using rdrobust with the cutoff equal to 1
out <- rdrobust(Y[X >= 0], X[X >= 0], c = 1)
summary(out)

# R Snippet 34
# Using rdrobust for the donut-hole approach
out <- rdrobust(Y[abs(X) >= 0.3], X[abs(X) >= 0.3])
summary(out)
