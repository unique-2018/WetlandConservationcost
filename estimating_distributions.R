#https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=3210035901
#https://r-forge.r-project.org/scm/viewvc.php/*checkout*/JSS/fitdistrplus/paper2JSS.pdf?revision=236&root=riskassessment&pathrev=236
source("functions_yield_price_data.R")
source("functions_dist.R")

#_)__________________________________________Crop yield
yield <- read_csv("data/yield.csv")
yield_df(df=yield, province ="Saskatchewan")


#yield distribution analysis :   crops <- c("Barley","Canola","Corn","Flaxseed","Oats","Peas","Soybeans","Wheat" )
#1. peas
pea_yd <- read_csv("data/yield/ Peas _yield_ Saskatchewan .csv") %>% mutate(value = as.numeric(Peas)) # %>% drop_na()
view(pea_yd)
dist_fit(df = pea_yd)     #uniform distribution is the best fit since it has the lowest AIC value
summary_plots(df=pea_yd)
#fit.peas_yd <- fitdist(pea_yd$value, "weibull")


#2. canola
can_yd <- read_csv("data/yield/ Canola _yield_ Saskatchewan .csv") %>% mutate(value = as.numeric(Canola)) %>% drop_na()
dist_fit(df = can_yd)     #gamma
summary_plots(df=can_yd)
fit.can_yd <- fitdist(can_yd$value, "gamma") #shape(se) = 7.34(1.14)  rate(se()) = 0.34(0.05)
rgamma(n=1, shape=7.34, rate=0.34)

#3. Barley
bar_yd <- read_csv("data/yield/ Barley _yield_ Saskatchewan .csv") %>% mutate(value = as.numeric(Barley)) %>% drop_na()
dist_fit(df = bar_yd)     #weibul
summary_plots(df=bar_yd)
fit.bar_yd <- fitdist(bar_yd$value, "weibull") #shape(se) = 2.53(0.19)  rate(se()) = 039.72(1.55)
rweibull(n=1, shape=2.53, scale =40)


#4. Flaxseed
flax_yd <- read_csv("data/yield/ Flaxseed _yield_ Saskatchewan .csv") %>% mutate(value = as.numeric(Flaxseed)) %>% drop_na()
dist_fit(df = flax_yd)     #weibul
summary_plots(df=flax_yd)
fit.flax_yd <- fitdist(flax_yd$value, "weibull") #shape(se) = 2.18(0.16)  rate(se()) = 14.47(0.63)
rweibull(n=1, shape=2.18, scale = 14)


#5.Oats
oats_yd <- read_csv("data/yield/ Oats _yield_ Saskatchewan .csv") %>% mutate(value = as.numeric(Oats)) %>% drop_na()
dist_fit(df = oats_yd)     #gamma
summary_plots(df=oats_yd)
fit.oats_yd  <- fitdist(oats_yd $value, "gamma") #shape(se) = 4.59(059)  rate(se()) = 0.10(0.01)
rgamma(n=1, shape=4.59, rate=0.10)


#6.Soybeans,"Wheat" 
soy_yd <- read_csv("data/yield/ Soybeans _yield_ Saskatchewan .csv") %>% mutate(value = as.numeric(Soybeans)) %>% drop_na()
dist_fit(df = soy_yd)     #uniform distribution is the best fit since it has the lowest AIC value
summary_plots(df=soy_yd)
fit.soy_yd  <- fitdist(soy_yd$value, "unif") #min=20  max=32.3
runif(1, 20, 32.3)

#7.Wheat 
wheat_yd <- read_csv("data/yield/ Wheat _yield_ Saskatchewan .csv") %>% mutate(value = as.numeric(Wheat)) %>% drop_na()
#view(wheat_yd)
dist_fit(df = wheat_yd)     #weibul
summary_plots(df=wheat_yd)
fit.wheat_yd  <- fitdist(wheat_yd$value, "weibull") #shape(se) = 2.48(0.18)  rate(se()) = 26.34(1.05)
rweibull(n=1, shape= 2.48, scale=26)

# __________________________________________________Crop prices
price_new <- read_csv("data/price/price_new.csv") 
view(price_new)
price_df(df=price_new, province = "Saskatchewan")

#price distribution analysis :   crops <- c("Barley","Canola","Corn","Flaxseed","Oats","Peas","Soybeans","Wheat" )
#1. peas
pea_price <- read_csv("data/price/ Dry peas [114314] Saskatchewan .csv")   %>% drop_na()
view(pea_price)
dist_fit(df = pea_price)     #lognormal
summary_plots(df=pea_yd)
fit.pea_price <- fitdist(pea_price$value, "lnorm") #shape(se) = 2.48(0.18)  rate(se()) = 26.34(1.05)
rlnorm(1, 1.85, 0.30)

#2. canola
can_price <- read_csv("data/price/ Canola (including rapeseed) [113111] Saskatchewan .csv")   %>% drop_na()
dist_fit(df = can_price)     #gamma
summary_plots(df=can_yd)
fit.can_price <- fitdist(can_price$value, "gamma") #
rgamma(n=1, shape=10.5, rate=0.91)


#3. Barley
bar_price <- read_csv("data/price/ Barley [1151141] Saskatchewan .csv")   %>% drop_na()
dist_fit(df = bar_price)     #gamma
summary_plots(df=bar_yd)
fit.bar_price <- fitdist(bar_price$value, "gamma") #
rgamma(n=1, shape=7.54, rate=1.65)

#4. Flaxseed
flax_price <- read_csv("data/price/ Flaxseed [115122111] Saskatchewan .csv")   %>% drop_na()
dist_fit(df = flax_price)     #log_normal
summary_plots(df=flax_yd)
fit.flax_price  <- fitdist(flax_price$value, "lnorm") #
rlnorm(n=1, 2.33, 0.42)

#5.Oats
oats_price <- read_csv("data/price/ Oats [115113111] Saskatchewan .csv")   %>% drop_na()
dist_fit(df = oats_price)     #gamma
summary_plots(df=oats_yd)
fit.oats_price  <- fitdist(oats_price$value, "gamma") #
rgamma(n=1, shape=8.52, rate=1.86)


#6.Soybeans,"Wheat" 
soy_price <- read_csv("data/price/ Soybeans [1151211] Saskatchewan .csv")   %>% drop_na()
dist_fit(df = soy_price)     #gamma
summary_plots(df=soy_yd)
fit.soy_price  <- fitdist(soy_price$value, "gamma") #
rgamma(n=1, shape=15.34, rate=1.34)

#7.Wheat 
wheat_yd <- read_csv("data/price/ Wheat (except durum wheat), milling Saskatchewan .csv")   %>% drop_na()
#view(wheat_yd)
dist_fit(df = wheat_yd)     #lnorm
summary_plots(df=wheat_yd)
fit.wheat_yd  <- fitdist(wheat_yd$value, "lnorm") #
rlnorm(n=1, 2, 0.13)



