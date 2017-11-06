#### set up ####
rm(list=ls())
packages <- c("twitteR", 
              "dplyr",
              "ptproc",
              "hawkes")
lapply(packages, require, character.only = TRUE)

# add your own twitter authorization here
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#### search twitter ####
subject <- c("#badhorrortitles") # replace this with the actual string
number <- 1000 # replace with maximum number of tweets to return
tweets <- searchTwitter(subject, n=number)
tweets <- strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE)
tweets <- twListToDF(tweets)
tweets$time <- as.numeric(tweets$created)
head(tweets)

min(tweets$created); max(tweets$created) # check the dates of the first and last tweets included

#### plot the cumulative number of tweets ####
tweets <- tweets %>%
  select(text, created, id, time)
breaks <- seq(min(tweets$time), max(tweets$time), by=10)
tweets.cut <- cut(tweets$time, breaks, right=FALSE)
tweets.freq <- table(tweets.cut)
cum.freq <- c(0, cumsum(tweets.freq))
time <- seq(min(tweets$created), max(tweets$created), by=10)
plot(time, cum.freq, type="l",
     xlab="Time", ylab="Cumulative Tweets")


#### hawkes model and conditional intensity ####
fit <- function(rate, data) {
  pstart <- c(mu = rate, C = 1, a = 0.5)
  ppm <- ptproc(pts = data, cond.int = hawkes.cond.int, params = pstart)
  condition(ppm) <- penalty(code = NULL, condition = "any(params<0)")
  f <- ptproc.fit(ppm, optim.control = list(trace = 2), alpha = 1e+5, hessian = TRUE)
  
  return (f)
}

tweet.fit <- fit(0.1, tweets$time)

# plot the conditional intensity
x <- seq(min(tweets$created), max(tweets$created), by=1)
est.fit <- evalCIF(tweet.fit, xpts = x)

plot(x, est.fit, type="l",
     xlab = "Times", 
     ylab = "Conditional intensity")

res <- residuals(tweet.fit, type = "approx", K = 99)
plot(res)

#### using ptproc instead for sim ####
model.fit <- summary(tweet.fit)
obj <- model.fit$ppobj
pt.sim <- ptproc.sim(obj, 300)
pt.sim <- as.data.frame(pt.sim)
colnames(pt.sim) <- c("time")

# add in plotting the cumulative history here
head(pt.sim)
pt.sim$newtime <- as.POSIXct(pt.sim$time, origin="1970-01-01", tz="UTC")
ptsim.breaks <- seq(min(pt.sim$time), max(pt.sim$time), by=10)
ptsim.cut <- cut(pt.sim$time, ptsim.breaks, right=FALSE)
ptsim.freq <- table(ptsim.cut)
ptsim.cum.freq <- c(0, cumsum(ptsim.freq))
ptsim.time <- seq(min(pt.sim$newtime), max(pt.sim$newtime), by=10)
plot(ptsim.time, ptsim.cum.freq, type="l",
     xlab="Time", ylab="Cumulative Tweets (ptsim)")

pt.sim.fit <- fit(0.1, pt.sim$time)

# plot the conditional intensity
pt.sim.x <- seq(min(pt.sim$time), max(pt.sim$time), by=1)
pt.sim.est.fit <- evalCIF(pt.sim.fit, xpts = pt.sim.x)

plot(pt.sim.x, pt.sim.est.fit, type="l",
     xlab = "Times", 
     ylab = "ptsim Conditional intensity")

pt.sim.res <- residuals(pt.sim.fit, type="approx", K=25)
plot(pt.sim.res)
