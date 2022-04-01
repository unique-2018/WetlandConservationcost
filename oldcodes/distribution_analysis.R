
df <- data.frame(
  proportion = c(0.25, 0.25, 0.5),
  num_fields = 10
)
landscape_df <- function(df, num){
  out = NULL
  for (i in 1:nrow(df)){
    a <- df[i,]
    b <- a[, "proportion"]
    d <- as.integer(b*num)
    e <- a %>% slice(rep(1:n(), each = d))
    out = rbind(out, e)
  }
  return(out)
}

landscape_df(df, 5)


library(fitdistrplus)
library(logspline)

  canola_price <- read_csv("data/ Canola (including rapeseed) [113111] .csv") %>% drop_na() %>% mutate(value = VALUE *0.03)
  wheat_price <- read_csv("data/ Wheat (except durum wheat) [1121111] .csv") %>% drop_na() %>% mutate(value = VALUE *0.03)
  peas_price <- read_csv("data/ Dry peas [114314] .csv") %>% drop_na() %>% mutate(value = VALUE *0.03)
  flax_price <- read_csv("data/ Flaxseed [115122111] .csv") %>% drop_na() %>% mutate(value = VALUE *0.03)
  #corn_price <- read_csv("data/ Corn for grain [1151111] .csv") %>% drop_na()
  barley_price <- read_csv("data/ Barley [1151141] .csv") %>% drop_na() %>% mutate(value = VALUE *0.03)
  oats_price <- read_csv("data/ Oats [115113111] .csv") %>% drop_na() %>% mutate(value = VALUE *0.03)
  
source("functions_dist.R")
price_dist_fit(df=flax_price)
price_summary_plots(df=flax_price)
  


price_df(df=prices, province = "Alberta")

canola_price <- read_csv("data/ Canola (including rapeseed) [113111] .csv") %>% drop_na() %>% mutate(value = VALUE *0.03)
view(canola_price)
wheat_price <- read_csv("data/ Wheat (except durum wheat) [1121111] .csv") %>% drop_na() %>% mutate(value = VALUE *0.03)
peas_price <- read_csv("data/ Dry peas [114314] .csv") %>% drop_na() %>% mutate(value = VALUE *0.03)
flax_price <- read_csv("data/ Flaxseed [115122111] .csv") %>% drop_na() %>% mutate(value = VALUE *0.03)
#corn_price <- read_csv("data/ Corn for grain [1151111] .csv") %>% drop_na()
barley_price <- read_csv("data/ Barley [1151141] .csv") %>% drop_na() %>% mutate(value = VALUE *0.03)
oats_price <- read_csv("data/ Oats [115113111] .csv") %>% drop_na() %>% mutate(value = VALUE *0.03)

hist()

hist(oats_price$VALUE)
ggplot(oats_price, aes(VALUE)) + geom_histogram(aes(y=..density..)) +
           geom_density(alpha=.2, fill="#FF6666") + theme_bw()
#Exploring
s <- descdist(oats_price$VALUE, discrete = FALSE)
str(s)
descdist(rnorm(300, 5, 24), discrete = FALSE)

#seems the distribution is beta
fit.weibull <- fitdist(oats_price$value, "weibull")
fit.norm <- fitdist(oats_price$value, "norm")
fit.uniform <- fitdist(oats_price$value, "unif")
fit.exp <- fitdist(oats_price$value, "exp")
fit.lnorm <- fitdist(oats_price$value, "lnorm")
fit.gamma <- fitdist(oats_price$value, "gamma")
fit.logistic <- fitdist(oats_price$value, "logis")







plot(fit.norm)
plot(fit.weibull)
plot(fit.uniform)
plot(fit.exp)



fit.weibull$aic
fit.norm$aic
fit.lnorm$aic
fit.gamma$aic

#automatic detection
library(gamlss)
library(gamlss.dist)
library(gamlss.add)

fit <- fitDist(canola_price$VALUE, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)

summary(fit)


n.sims <- 5e4

stats <- replicate(n.sims, {      
  r <- rweibull(n = length(canola_price$VALUE)
                , shape= fit.weibull$estimate["shape"]
                , scale = fit.weibull$estimate["scale"]
  )
  estfit.weibull <- fitdist(r, "weibull") # added to account for the estimated parameters
  as.numeric(ks.test(r
                     , "pweibull"
                     , shape= estfit.weibull$estimate["shape"]
                     , scale = estfit.weibull$estimate["scale"])$statistic
  )      
})

plot(ecdf(stats), las = 1, main = "KS-test statistic simulation (CDF)", col = "darkorange", lwd = 1.7)
grid()

