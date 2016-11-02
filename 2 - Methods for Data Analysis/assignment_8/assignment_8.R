# #########################################################################
# Assignment 8
# Method for Data Analysis
# 
# Author: Josh Jensen
# #########################################################################



#
### Set up
#

library(LearnBayes)


beta_prior <- beta.select(list(p=.5, x=.1), list(p=.75, x=.3))
beta_prior


# Now observe 2 texting out of 20 drivers
triplot(beta_prior, c(2, 18), where = 'topright')
beta_prior + c(2, 18)

# Now observe 4 texting out of 20 drivers
triplot(beta_prior, c(2 + 4, 18 + 16), where = 'topright')
beta_prior + c(2 + 4, 18 + 16)

# Now observe 1 texting out of 20 drivers
triplot(beta_prior, c(2 + 4 + 1, 18 + 16 + 19), where = 'topright')
beta_prior + c(2 + 4 + 1, 18 + 16 + 19)


# calc posterior beta distribution
beta_post <- beta_prior + c(2 + 4 + 1, 18 + 16 + 19)

# random sample 100,000 obs
post_sample <- rbeta(100000, beta_post[1], beta_post[2])

# plot
par(mfrow = c(1, 2))
quants <- quantile(post_sample, c(0.05, 0.95))
breaks <- seq(min(post_sample), max(post_sample), length.out = 50)
hist(post_sample, breaks = breaks,
     main = "Distribution of sample \n with 90% HDI"
     , xlab = "sample value"
     , ylab = "density")
abline(v = quants[1], lty = 3, col = "red", lwd = 3)
abline(v = quants[2], lty = 3, col = "red", lwd = 3)
qqnorm(scale(post_sample))
abline(a=0, b=1, col = 'red')
par(mfrow = c(1, 1))

# 90% HDI limits
quants

# compare to national figures (initial quants)
pbeta(c(.1, .3), beta_post[1], beta_post[2])
triplot(beta_prior, c(2 + 4 + 1, 18 + 16 + 19), where = 'topright')