#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# A Practical Introduction to Regression Discontinuity Designs
# Authors: Matias D. Cattaneo, Nicolas Idrobo and Rocio Titiunik
# Last update: 03-APR-2020
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
rm(list=ls())

library(foreign)
library(ggplot2)
library(lpdensity)
library(rddensity)
library(rdrobust)
library(rdlocrand)
library(TeachingDemos)

options(width=300)
par(mar = rep(2, 4))

data = read.dta("CIT_2018_Cambridge_senate.dta")
Y = data$demvoteshfor2
X = data$demmv
T = (X>=0)

#-------------------------------#
# Section 2                     #
# The Canonical Sharp RD Design #
#-------------------------------#
# Figure 2.3a
# Raw comparison of means
rdplot(Y, X, nbins = c(2500, 500), p = 0, col.lines = "red", col.dots = "lightgray", title = "", 
       y.lim = c(0,100))

# Figure 2.3b
# Local comparison of means
rdplot(Y[abs(X) <= 50], X[abs(X) <= 50], nbins = c(2500, 500), p = 4, col.lines = "red", col.dots = "lightgray", title = "", 
       y.lim = c(0,100))

#-----------#
# Section 3 #
# RD Plots  #
#-----------#
# Code snippet 1 (Figure 3.1)
# Scatter plot
plot(X, Y, xlab = "Score", ylab = "Outcome", col = 1, pch = 20)
abline(v = 0)

# Code snippet 2 (Figure 3.2)
# RD plot using 40 bins of equal length
out = rdplot(Y, X, nbins = c(20,20), binselect = 'esmv')
summary(out)

# Code snippet 3 (Figure 3.3)
# 40 Evenly-spaced bins
out = rdplot(Y, X, nbins = c(20,20), binselect = 'es')
summary(out)

# Code snippet 4 (Figure 3.4)
# 40 Quantile-spaced bins
out = rdplot(Y, X, nbins = c(20,20), binselect = 'qs', x.lim = c(-100,100))
summary(out)

# Code snippet 5 (Figure 3.6)
# IMSE RD plot with evenly-spaced bins
out = rdplot(Y, X,  binselect = 'es', x.lim = c(-100,100))
summary(out)

# Code snippet 6 (Figure 3.7)
# IMSE RD plot with quantile-spaced bins
out = rdplot(Y, X,  binselect = 'qs', x.lim = c(-100,100))
summary(out)

# Code snippet 7 (Figure 3.8)
# Mimicking variance RD plot with evenly-spaced bins
out = rdplot(Y, X,  binselect = 'esmv')
summary(out)

# Code snippet 8 (Figure 3.9)
# Mimicking variance RD plot with quantile-spaced bins
out = rdplot(Y, X,  binselect = 'qsmv', x.lim = c(-100,100))
summary(out)

#----------------------------------------------#
# Section 4                                    #
# The Continuity-Based Approach to RD Analysis #
#----------------------------------------------#
# Code snippet 1
# Using two regressions to estimate
out = lm(Y[X < 0 & X >= -10] ~ X[X < 0 & X >= -10])
left_intercept = out$coefficients[1]
print(left_intercept)
out = lm(Y[X >= 0 & X <= 10] ~ X[X >= 0 & X <= 10])
right_intercept = out$coefficients[1]
print(right_intercept)
difference = right_intercept - left_intercept
print(paste("The RD estimator is", difference, sep = " "))

# Code snippet 2
# Using one regression to estimate
T_X = X * T
out = lm(Y[X >= -10 & X <= 10] ~ X[X >= -10 & X <= 10] + T[X >= -10 & X <= 10] + T_X[X >= -10 & X <= 10])
summary(out)

# Code snippet 3
# Generating triangular weights
w = NA
w[X < 0 & X >= -10] = 1 - abs(X[X < 0 & X >= -10] / 10)
w[X >= 0 & X <= 10] = 1 - abs(X[X >= 0 & X <= 10] / 10)

# Code snippet 4
# Using two regressions and weights to estimate
out = lm(Y[X < 0] ~ X[X < 0], weights = w[X < 0])
left_intercept = out$coefficients[1]
out = lm(Y[X >= 0] ~ X[X >= 0], weights = w[X >= 0])
right_intercept = out$coefficients[1]
difference = right_intercept - left_intercept
print(paste("The RD estimator is", difference, sep = " "))

# Code snippet 5
# Using rdrobust with uniform weights
out = rdrobust(Y, X, kernel = 'uniform',  p = 1, h = 10)
summary(out)

# Code snippet 6
# Using rdrobust with triangular weights
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, h = 10)
summary(out)

# Code snippet 7
# Using rdrobust with triangular weights and p = 2
out = rdrobust(Y, X, kernel = 'triangular',  p = 2, h = 10)
summary(out)

# Code snippet 8
# Using rdbwselect with mserd bandwidth
out = rdbwselect(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)

# Code snippet 9
# Using rdbwselect with msetwo bandwidth
out = rdbwselect(Y, X, kernel = 'triangular',  p = 1, bwselect = 'msetwo')
summary(out)

# Code snippet 10
# Using rdrobust with mserd bandwidth
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)

# Code snippet 11
# Using rdrobust to show the objects it returns
rdout = rdrobust(Y, X, kernel = 'triangular', p = 1, bwselect = 'mserd')
print(names(rdout))
print(rdout$beta_p_r)
print(rdout$beta_p_l)

# Code snippet 12
# Using rdrobust and showing the associated rdplot
bandwidth = rdrobust(Y, X, kernel = 'triangular', p = 1, bwselect = 'mserd')$h_l
out = rdplot(Y[abs(X) <= bandwidth], X[abs(X) <= bandwidth], p = 1, kernel = 'triangular')
summary(out)

# Figure 4.4
# Local polynomial RD effect illustrated with rdplot-Senate data
rdplot(Y[abs(X) <= bandwidth], X[abs(X) <= bandwidth], p = 1, kernel = 'triangular',
       x.label = 'Score', y.label = 'Outcome', title = '')

# Code snippet 13
# Using rdrobust without regularization term
out = rdrobust(Y, X, kernel = 'triangular', scaleregul = 0,  p = 1, bwselect = 'mserd')
summary(out)

# Code snippet 14
# Using rdrobust with default options
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)

# Code snippet 15
# Using rdrobust with default options and showing all the output
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out, all = TRUE)

# Code snippet 16
# Using rdrobust with cerrd bandwidth
out = rdrobust(Y, X, kernel = 'triangular', p = 1, bwselect = 'cerrd')
summary(out)

# Code snippet 17
# Using rdbwselect with all the bandwidths
out = rdbwselect(Y, X, kernel = 'triangular', p = 1, all = TRUE)
summary(out)

# Code snippet 18
# Using rdbwselect with covariates
Z = cbind(data$presdemvoteshlag1, data$demvoteshlag1, data$demvoteshlag2, 
          data$demwinprv1, data$demwinprv2, data$dmidterm, data$dpresdem, data$dopen)
colnames(Z) = c("presdemvoteshlag1", "demvoteshlag1", "demvoteshlag2",
                "demwinprv1", "demwinprv2", "dmidterm", "dpresdem", "dopen")
out = rdbwselect(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd')
summary(out)

# Code snippet 19
# Using rdrobust with covariates
Z = cbind(data$presdemvoteshlag1, data$demvoteshlag1, data$demvoteshlag2, 
          data$demwinprv1, data$demwinprv2, data$dmidterm, data$dpresdem, data$dopen)
colnames(Z) = c("presdemvoteshlag1", "demvoteshlag1", "demvoteshlag2",
                "demwinprv1", "demwinprv2", "dmidterm", "dpresdem", "dopen")
out = rdrobust(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd')
summary(out)

#-----------------------------------------------#
# Section 5                                     #
# Validation and Falsification of the RD Design #
#-----------------------------------------------#
# Figure 5.1
# RD plots for predetermined covariates
rdplot(data$presdemvoteshlag1, X,
       x.label = "Score", y.label = "", title="")

rdplot(data$demvoteshlag1, X,
       x.label = "Score", y.label = "", title="")

rdplot(data$demvoteshlag2, X,
       x.label = "Score", y.label = "", title="")

rdplot(data$demwinprv1, X,
       x.label = "Score", y.label = "", title="")

rdplot(data$demwinprv2, X,
       x.label = "Score", y.label = "", title="")

rdplot(data$dmidterm, X,
       x.label = "Score", y.label = "", title="")

rdplot(data$dpresdem, X,
       x.label = "Score", y.label = "", title="")

rdplot(data$dopen, X,
       x.label = "Score", y.label = "", title="")

# Code snippet 1
# Using rdrobust on demvoteshlag1
out = rdrobust(data$demvoteshlag1, X)
summary(out)

# Code snippet 2
# Using rdplot to show the rdrobust effect for demvoteshlag1
bandwidth = rdrobust(data$demvoteshlag1, X)$h_l
xlim = ceiling(bandwidth)
rdplot(data$demvoteshlag1[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "")

# Figure 5.2
# Graphical illustration of local linear RD effects for predetermined covariates
bandwidth = rdrobust(data$presdemvoteshlag1, X)$h_l
xlim = ceiling(bandwidth)
rdplot(data$presdemvoteshlag1[abs(X)<=bandwidth], X[abs(X)<=bandwidth],
       p=1, kernel='triangular', x.lim=c(-xlim,xlim), x.label = "Score",
       y.label = "", title="")

bandwidth = rdrobust(data$demvoteshlag1, X)$h_l
xlim = ceiling(bandwidth)
rdplot(data$demvoteshlag1[abs(X)<=bandwidth], X[abs(X)<=bandwidth],
       p=1, kernel='triangular', x.lim=c(-xlim,xlim), x.label = "Score",
       y.label = "", title="")

bandwidth = rdrobust(data$demvoteshlag2, X)$h_l
xlim = ceiling(bandwidth)
rdplot(data$demvoteshlag2[abs(X)<=bandwidth], X[abs(X)<=bandwidth],
       p=1, kernel='triangular', x.lim=c(-xlim,xlim), x.label = "Score",
       y.label = "", title="")

bandwidth = rdrobust(data$demwinprv1, X)$h_l
xlim = ceiling(bandwidth)
rdplot(data$demwinprv1[abs(X)<=bandwidth], X[abs(X)<=bandwidth],
       p=1, kernel='triangular', x.lim=c(-xlim,xlim), x.label = "Score",
       y.label = "", title="")

bandwidth = rdrobust(data$demwinprv2, X)$h_l
xlim = ceiling(bandwidth)
rdplot(data$demwinprv2[abs(X)<=bandwidth], X[abs(X)<=bandwidth],
       p=1, kernel='triangular', x.lim=c(-xlim,xlim), x.label = "Score",
       y.label = "", title="")

bandwidth = rdrobust(data$dmidterm, X)$h_l
xlim = ceiling(bandwidth)
rdplot(data$dmidterm[abs(X)<=bandwidth], X[abs(X)<=bandwidth],
       p=1, kernel='triangular', x.lim=c(-xlim,xlim), x.label = "Score",
       y.label = "", title="")

bandwidth = rdrobust(data$dpresdem, X)$h_l
xlim = ceiling(bandwidth)
rdplot(data$dpresdem[abs(X)<=bandwidth], X[abs(X)<=bandwidth],
       p=1, kernel='triangular', x.lim=c(-xlim,xlim), x.label = "Score",
       y.label = "", title="")

bandwidth = rdrobust(data$dopen, X)$h_l
xlim = ceiling(bandwidth)
rdplot(data$dopen[abs(X)<=bandwidth], X[abs(X)<=bandwidth],
       p=1, kernel='triangular', x.lim=c(-xlim,xlim), x.label = "Score",
       y.label = "", title="")

# Code snippet 3
# Using rddensity
out = rddensity(X)
summary(out)

# Figure 5.4a
# Histogram
bw_left = as.numeric(rddensity(X)$h[1]); bw_right = as.numeric(rddensity(X)$h[2]);
tempdata = as.data.frame(X); colnames(tempdata) = c("v1");
plot2 = ggplot(data=tempdata, aes(tempdata$v1)) +
  geom_histogram(data = tempdata, aes(x = v1, y= ..count..), breaks = seq(-bw_left, 0, 1), fill = "blue", col = "black", alpha = 1) +
  geom_histogram(data = tempdata, aes(x = v1, y= ..count..), breaks = seq(0, bw_right, 1), fill = "red", col = "black", alpha = 1) +
  labs(x = "Score", y = "Number of Observations") + geom_vline(xintercept = 0, color = "black") +
  theme_bw()
plot2

# Figure 5.4b
# Estimated Density
est1 = lpdensity(data = X[X < 0 & X >= -bw_left], grid = seq(-bw_left, 0, 0.1), bwselect = "IMSE",
                 scale = sum(X < 0 & X >= -bw_left) / length(X))
est2 = lpdensity(data = X[X >= 0 & X <= bw_right], grid = seq(0, bw_right, 0.1), bwselect = "IMSE",
                 scale = sum(X >= 0 & X <= bw_right) / length(X))
plot1 = lpdensity.plot(est1, est2, CIshade = 0.2, lcol = c(4, 2), CIcol = c(4, 2), legendGroups = c("Control", "Treatment"))+
  labs(x = "Score", y = "Density") + geom_vline(xintercept = 0, color = "black") +
  theme_bw()
plot1

# Code snippet 4
# Using rdrobust with the cutoff equal to 1
out = rdrobust(Y[X >= 0], X[X >= 0], c = 1)
summary(out)

# Table 5.2 (no output)
# Continuity-based analysis for alternative cutoffs
for(k in -3:-1) {
  out = rdrobust(Y[X < 0], X[X < 0], c = k)
  summary(out)
}
out = rdrobust(Y, X, c = 0)
summary(out)
for(k in 1:3) {
  out = rdrobust(Y[X >= 0], X[X >= 0], c = k)
  summary(out)
}

# Code snippet 5
# Using rdrobust for the donut-hole approach
out = rdrobust(Y[abs(X) >= 0.3], X[abs(X) >= 0.3])
summary(out)
