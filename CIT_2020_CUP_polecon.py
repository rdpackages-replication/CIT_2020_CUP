#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# A Practical Introduction to Regression Discontinuity Designs: Foundations
# Authors: Matias D. Cattaneo, Nicolás Idrobo and Rocío Titiunik
#-----------------------------------------------------------------------------#
# SOFTWARE WEBSITE: https://rdpackages.github.io/
#-----------------------------------------------------------------------------#
# TO INSTALL STATA PACKAGES:
# RDROBUST: pip install rdrobust
# RDDENSITY: pip install rddensity
# RDLOCRAND: pip install rdlocrand
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Loading packages
from rdrobust import rdrobust, rdbwselect, rdplot
from scipy.stats import binomtest
from sklearn.linear_model import LinearRegression
import rddensity
import pandas as pd
import matplotlib.pyplot as plt
import math
import statsmodels.api as sm
import numpy as np

#------------------#
# Loading the data #
#------------------#
# Loading the data and defining the main variables
data = pd.read_csv("CIT_2020_CUP_polecon.csv")

#---------------------#
# Section 2           #
# The Sharp RD Design #
#---------------------#
# Figure 3a
# Raw comparison of means
out = rdplot(y = data.Y, x = data.X, p = 0, nbins = (2500,500), 
             x_label = "Islamic Margin of Victory", y_lim = (0, 70),
             y_label = "Female High School Percentage", title = "")

# Figure 3b
# Local comparison of means
subset = ((-50 <= data.X) & (data.X <= 50)).values
rdplot(y = data.Y, x = data.X, subset = subset, p = 4, nbins = (2500,500), 
       x_label = "Islamic Margin of Victory", y_lim = (0, 70),
       y_label = "Female High School Percentage", title = "")

#-----------#
# Section 3 #
# RD Plots  #
#-----------#
# Snippet 1 (Figure 5)
# Scatter plot
plt.scatter(data.X, data.Y, color = "black", s = 5)
plt.axvline(x = 0, color = 'blue')
plt.show()

# Figure 6
# RD plot using 40 bins of equal length
out = rdplot(data.Y, data.X, nbins = 20, binselect = "esmv", y_lim = (0, 25), 
             x_label = "Score", y_label = "Outcome", title = "")
print(out)

# Snippet 2 (Figure 7a)
# 40 Evenly-spaced bins
out = rdplot(data.Y, data.X, nbins = 20, binselect = 'es', y_lim = (0, 25), 
             x_label = "Score", y_label = "Outcome", title = "")
print(out)

# Snippet 3 (Figure 7b)
# 40 Quantile-spaced bins
out = rdplot(data.Y, data.X, nbins = 20, binselect = 'qs', x_lim = (-100, 100), 
             y_lim = (0, 25), x_label = "Score", y_label = "Outcome", title = "")
print(out)

# Snippet 4 (Figure 8)
# IMSE RD plot with evenly-spaced bins
out = rdplot(data.Y, data.X,  binselect = 'es', x_lim = (-100, 100), y_lim = (0, 25), 
             x_label = "Score", y_label = "Outcome", title = "")
print(out)

# Snippet 5 (Figure 9)
# IMSE RD plot with quantile-spaced bins
out = rdplot(data.Y, data.X,  binselect = 'qs', x_lim = (-100, 100), y_lim = (0, 25), 
             x_label = "Score", y_label = "Outcome", title = "")
print(out)

# Snippet 6 (Figure 10)
# Mimicking variance RD plot with evenly-spaced bins
out = rdplot(data.Y, data.X,  binselect = 'esmv', 
             x_label = "Score", y_label = "Outcome", title = "")
print(out)

# Snippet 7 (Figure 11)
# Mimicking variance RD plot with quantile-spaced bins
out = rdplot(data.Y, data.X, binselect = 'qsmv', x_lim = (-100, 100), y_lim = (0, 25), 
             x_label = "Score", y_label = "Outcome", title = "")
print(out)

#----------------------------------------------#
# Section 4                                    #
# The Continuity-Based Approach to RD Analysis #
#----------------------------------------------#
# Snippet 8
# Using two regressions to estimate
subset_left = data[(data['X'] < 0) & (data['X'] >= -20)].copy()
X_subset_left = subset_left[['X']]
Y_subset_left = subset_left['Y']
model_left = LinearRegression().fit(X_subset_left, Y_subset_left)
left_intercept = model_left.intercept_
#---#
subset_right = data[(data['X'] >= 0) & (data['X'] <= 20)].copy()
X_subset_right = subset_right[['X']]
Y_subset_right = subset_right['Y']
model_right = LinearRegression().fit(X_subset_right, Y_subset_right)
right_intercept = model_right.intercept_
#---#
difference = right_intercept - left_intercept
print("The RD estimator is:", difference)

# Snippet 9
# Using one regression to estimate
subset = data[(data['X'] >= -20) & (data['X'] <= 20)].copy()
#---#
T_subset = subset['T']
X_subset = subset['X']
Y_subset = subset['Y']
#---#
df = pd.DataFrame({
    'X': X_subset, 
    'Y': Y_subset, 
    'T': T_subset
})
#---#
df['T_X'] = df['T'] * df['X']
#---#
X_subset = df.drop(columns=['Y'])
Y_subset = df['Y']
X_subset = sm.add_constant(X_subset)
#---#
ols_model = sm.OLS(Y_subset, X_subset)
ols_results = ols_model.fit()
#---#
print(ols_results.summary())

# Python Snippet 10
# Generating triangular weights
data['w'] = 1 - np.abs(data['X'] / 20)
data['w'] = np.maximum(data['w'], 0)

# Python Snippet 11
# Using two regressions and weights to estimate
subset_left = data[data['X'] < 0].copy()
subset_right = data[data['X'] >= 0].copy()
#---#
X_subset_left = subset_left[['X']]
Y_subset_left = subset_left['Y']
weights_left = subset_left['w']
#---#
model_left = LinearRegression().fit(X_subset_left, Y_subset_left, sample_weight=weights_left)
left_intercept = model_left.intercept_
#---#
X_subset_right = subset_right[['X']]
Y_subset_right = subset_right['Y']
weights_right = subset_right['w']
#---#
model_right = LinearRegression().fit(X_subset_right, Y_subset_right, sample_weight=weights_right)
right_intercept = model_right.intercept_
#---#
difference = right_intercept - left_intercept
print("The RD estimator is:", difference)

# Snippet 12
# Using rdrobust with uniform weights
out = rdrobust(data.Y, data.X, kernel = 'uniform', p = 1, h = 20)
print(out)

# Snippet 13
# Using rdrobust with triangular weights
out = rdrobust(data.Y, data.X, kernel = 'triangular', p = 1, h = 20)
print(out)

# Snippet 14
# Using rdrobust with triangular weights and p = 2
out = rdrobust(data.Y, data.X, kernel = 'triangular', p = 2, h = 20)
print(out)

# Snippet 15
# Using rdbwselect with mserd bandwidth
out = rdbwselect(data.Y, data.X, kernel = 'triangular', p = 1, bwselect = 'mserd')
print(out)

# Snippet 16
# Using rdbwselect with msetwo bandwidth
out = rdbwselect(data.Y, data.X, kernel = 'triangular', p = 1, bwselect = 'msetwo')
print(out)

# Snippet 17
# Using rdrobust with mserd bandwidth
out = rdrobust(data.Y, data.X, kernel='triangular', p=1, bwselect='mserd')
print(out)

# Snippet 18
# Using rdrobust to show the objects it returns
rdout = rdrobust(data.Y, data.X, kernel = 'triangular', p = 1, bwselect = 'mserd')
dir(rdout)
print(rdout.beta_p_r)
print(rdout.beta_p_l)

# Snippet 19 (Figure 15)
# Using rdrobust and showing the associated rdplot
est = rdrobust(data.Y, data.X, kernel = 'triangular', p = 1, bwselect = 'mserd')
h_l, h_r = est.bws.loc['h', :].values
subset = ((-h_l <= data.X) & (data.X <= h_r)).values
out = rdplot(data.Y, data.X, subset = subset, p = 1, kernel = 'triangular',
             x_label = "Score", y_label = "Outcome", title = "")
print(out)

# Snippet 20
# Using rdrobust without regularization term
out = rdrobust(data.Y, data.X, kernel = 'triangular', scaleregul = 0,
               p = 1, bwselect = 'mserd')
print(out)

# Snippet 21
# Using rdrobust with default options
out = rdrobust(data.Y, data.X, kernel = 'triangular', p = 1, bwselect = 'mserd')
print(out)

# Snippet 22
# Using rdrobust with default options and showing all the output
out = rdrobust(data.Y, data.X, kernel = 'triangular', p = 1, 
               bwselect = 'mserd', all = True)
print(out)

# Snippet 23
# Using rdrobust with cerrd bandwidth
out = rdrobust(data.Y, data.X, kernel = 'triangular', p = 1, bwselect = 'cerrd')
print(out)

# Snippet 24
# Using rdbwselect with all the bandwidths
out = rdbwselect(data.Y, data.X, kernel = 'triangular', p = 1, all = True)
print(out)

# Snippet 25
# Using rdbwselect with covariates
Z = data[['vshr_islam1994', 'partycount', 'lpop1994', 'merkezi', 'merkezp',
          'subbuyuk', 'buyuk']]
out = rdbwselect(data.Y, data.X, covs = Z, kernel = 'triangular', 
                 scaleregul = 1, p = 1, bwselect = 'mserd')
print(out)

# Snippet 26
# Using rdrobust with covariates
Z = data[['vshr_islam1994', 'partycount', 'lpop1994', 'merkezi', 'merkezp',
          'subbuyuk', 'buyuk']]
out = rdrobust(data.Y, data.X, covs = Z, kernel = 'triangular',
               scaleregul = 1, p = 1, bwselect = 'mserd')
print(out)

# Snippet 27
# Using rdrobust with clusters
prov_num = data.prov_num
out = rdrobust(data.Y, data.X, kernel = 'triangular', scaleregul = 1, p = 1,
               bwselect = 'mserd', cluster = prov_num)
print(out)

# Snippet 28
# Using rdrobust with clusters and covariates
Z = data[['vshr_islam1994', 'partycount', 'lpop1994', 'merkezi', 'merkezp', 
          'subbuyuk', 'buyuk']]
prov_num = data.prov_num
out = rdrobust(data.Y, data.X, covs = Z, kernel = 'triangular', scaleregul = 1,
               p = 1, bwselect = 'mserd', cluster = prov_num)
print(out)

#-----------------------------------------------#
# Section 5                                     #
# Validation and Falsification of the RD Design #
#-----------------------------------------------#
# Figure 16
# RD plots for predetermined covariates
rdplot(data.lpop1994, data.X, x_label = "Score", y_label = "", title = "")
rdplot(data.partycount, data.X, x_label = "Score", y_label = "", title = "")
rdplot(data.vshr_islam1994, data.X, x_label = "Score", y_label = "", title = "")
rdplot(data.i89, data.X, x_label = "Score", y_label = "", title = "", x_lim = (-100, 100))
rdplot(data.merkezp, data.X, x_label = "Score", y_label = "", title = "")
rdplot(data.merkezi, data.X, x_label = "Score", y_label = "", title = "")

# Snippet 29
# Using rdrobust on lpop1994
out = rdrobust(data.lpop1994, data.X)
print(out)

# Formal continuity-based analysis for covariates using CER-optimal bandwidth (not reported in the text)
print(rdrobust(data.hischshr1520m, data.X, bwselect = 'cerrd'))
print(rdrobust(data.i89, data.X, bwselect = 'cerrd'))
print(rdrobust(data.vshr_islam1994, data.X, bwselect = 'cerrd'))
print(rdrobust(data.partycount, data.X, bwselect = 'cerrd'))
print(rdrobust(data.lpop1994, data.X, bwselect = 'cerrd'))
print(rdrobust(data.merkezi, data.X, bwselect = 'cerrd'))
print(rdrobust(data.merkezp, data.X, bwselect = 'cerrd'))
print(rdrobust(data.subbuyuk, data.X, bwselect = 'cerrd'))
print(rdrobust(data.buyuk, data.X, bwselect = 'cerrd'))

# Snippet 30
# Using rdplot to show the rdrobust effect for lpop1994
est = rdrobust(data.lpop1994, data.X, kernel = 'triangular', p = 1, 
               bwselect = 'mserd')
h_l, h_r = est.bws.loc['h', :].values
xlim = math.ceil(h_l)
subset = ((-h_l <= data.X) & (data.X <= h_r)).values
rdplot(data.lpop1994, data.X, subset = subset, p = 1, kernel = 'triangular', 
        x_lim = (-xlim, xlim), x_label = "Score", y_label = "", title = "")

# Figure 17
# Graphical illustration of local linear RD effects for predetermined covariates
Z = data[['vshr_islam1994', 'partycount', 'merkezi', 'merkezp', 'subbuyuk', 'buyuk']]
for i in Z.columns:
    est = rdrobust(Z[i], data.X, kernel = 'triangular', p = 1, 
                   bwselect = 'mserd')
    h_l, h_r = est.bws.loc['h', :].values
    xlim = math.ceil(h_l)
    subset = ((-h_l <= data.X) & (data.X <= h_r)).values
    rdplot(Z[i], data.X, subset = subset, p = 1, kernel = 'triangular', 
           x_lim = (-xlim, xlim), x_label = "Score", y_label = "", title = "")

# Snippet 31
# Binomial test
result = binomtest(53, n = 100, p = 0.5, alternative = 'two-sided')
print(result.pvalue)

# Snippet 32
# Using rddensity
print(repr(rddensity.rddensity(X = data.X)))

# Figure 19a
# Histogram
out = rddensity.rddensity(X = data.X)
#---#
subset_data = data[(data['X'] > -out.h[0]) & (data['X'] < out.h[1])]
#---#
plt.hist(x=subset_data[subset_data['X'] < 0]['X'], bins=30, color='blue', alpha=0.7)
plt.hist(x=subset_data[subset_data['X'] >= 0]['X'], bins=30, color='red', alpha=0.7)
#---#
plt.axvline(x=0, color='black', linestyle='--')
#---#
plt.xlabel('Score')
plt.ylabel('Number of observations')
plt.title('')
plt.show()

# Figure 19b
# Estimated Density
rdd = rddensity.rddensity(X = data.X)
rddensity.rdplotdensity(rdd, data.X)

# Snippet 33
# Using rdrobust with the cutoff equal to 1
subset = (data.X >= 0).values
out = rdrobust(data.Y, data.X, c = 1, subset = subset)
print(out)

# Snippet 34
# Using rdrobust for the donut-hole approach
subset = (abs(data.X) >= 0.3).values
out = rdrobust(data.Y, data.X, subset = subset)
print(out)