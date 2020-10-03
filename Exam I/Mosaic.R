library(mosaic)

df <- data.frame(processed.cleveland)
exercise = df$V8

trellis.par.set(theme=col.mosaic())
options(digits=3)

###
#mu = 500
#sigma = 100
#x = rnorm(500, mean=mu, sd=sigma)
#favstats(x)
#plot(density(x))

mean_exercise = mean(exercise)
std_exercise = sd(exercise)

meanconfint = function (x, sigma, level = 0.95, ...) {
  se = sigma / sqrt(length(x))
  mu = mean(x)
  z = qnorm(1 - (1 - level)/2)
  out = c(mu, mu - z * se, mu + z * se)
  names(out) = c("mean", "lower", "upper")
  return(out)
}

bootstrap1 = do(10000) * mean(resample(exercise))
head(bootstrap1)
bootstrap2 = do(10000) * sd(resample(exercise))
head(bootstrap2)
bootstrap3 = do(10000) * meanconfint(resample(exercise), sd(resample(exercise)))
head(bootstrap3)

densityplot(~mean, data=bootstrap1)
densityplot(~mean, data=bootstrap2)
densityplot(~mean, data=bootstrap3)