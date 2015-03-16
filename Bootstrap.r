# This program is designed to form a bootstrap confidence interval
# assuming a Poisson process

sample = c(1, 2, 3, 6, 5, 3, 8)
n = length(sample)
lamHat = mean(sample)
count = 1000
means = rep(0, count)
bootMeans = data.frame(means)


for (i in 1:count)
  {
    bootMeans[i, 1] = mean(rpois(n, lamHat))
  }

head(bootMeans)

# Plot Histogram of bootstrap means
plot1 = ggplot(data = bootMeans, aes(x = means)) + geom_histogram( color = "black", fill = hcl(240, 60, 60), alpha = .5)
plot1 = plot1 + xlab("Bootsrap Mean") + ylab("Frequency") + ggtitle("Histogram of Boostrap Means")
plot1

# Constrcut confidence interval for boostrapped mean
pivot = bootMeans - lamHat

sorted = sort(pivot[ , 1])

deltaLow = sorted[25]
deltaHigh = sorted[975]

lower = lamHat - deltaHigh
upper = lamHat - deltaLow

confidence = c(lower, upper)
confidence