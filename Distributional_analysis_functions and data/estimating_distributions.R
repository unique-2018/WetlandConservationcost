#https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=3210035901
#https://r-forge.r-project.org/scm/viewvc.php/*checkout*/JSS/fitdistrplus/paper2JSS.pdf?revision=236&root=riskassessment&pathrev=236
source("functions_yield_price_data.R")
source("functions_dist.R")

#_)__________________________________________Crop yield
yield <- read_csv("data/yield.csv")
yield_df(df=yield, province ="Alberta")


#yield distribution analysis :   crops <- c("Barley","Canola","Corn","Flaxseed","Oats","Peas","Soybeans","Wheat" )
#1. peas
pea_yd <- read_csv("data/yield/ Peas _yield_ Alberta .csv") %>% mutate(value = as.numeric(Peas)) %>% drop_na()
view(pea_yd)

dist_fit(df = pea_yd)     #uniform distribution is the best fit since it has the lowest AIC value
summary_plots(df=pea_yd)
min(pea_yd$value)
max(pea_yd$value)


#2. canola
can_yd <- read_csv("data/yield/ Canola _yield_ Alberta .csv") %>% mutate(value = as.numeric(Canola)) %>% drop_na()
dist_fit(df = can_yd)     #log-norm

summary_plots(df=can_yd)
fit.can_yd <- fitdist(can_yd$value, "lnorm") #shape(se) = 7.34(1.14)  rate(se()) = 0.34(0.05)
rlnorm(n=10, 3.15, 0.35)

#3. Barley
bar_yd <- read_csv("data/yield/ Barley _yield_ Alberta .csv") %>% mutate(value = as.numeric(Barley)) %>% drop_na()
dist_fit(df = bar_yd)     #uniform
summary_plots(df=bar_yd)
min(bar_yd$value)
max(bar_yd$value)

#5.Oats
oats_yd <- read_csv("data/yield/ Oats _yield_ Alberta .csv") %>% mutate(value = as.numeric(Oats)) %>% drop_na()
dist_fit(df = oats_yd)     #uniform
summary_plots(df=oats_yd)
min(oats_yd$value)
max(oats_yd$value)


#7.Wheat 
wheat_yd <- read_csv("data/yield/ Wheat _yield_ Alberta .csv") %>% mutate(value = as.numeric(Wheat)) %>% drop_na()
#view(wheat_yd)
dist_fit(df = wheat_yd)     #gamma
summary_plots(df=wheat_yd)
fit.wheat_yd  <- fitdist(wheat_yd$value, "gamma") #shape(se) = 2.48(0.18)  rate(se()) = 26.34(1.05)
rgamma(n=10, shape= 5.14, rate=0.18)

# __________________________________________________Crop prices

# loading the required packages
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

# Load Packages
p_load(cansim)


#  >>>>>>>>>>>>>>>>> Consumer price index <<<<<<<<<<<<<<<<<<<<<
cpi_canada <- get_cansim(1810000401) %>%
  filter(`Products and product groups` == "All-items",
         GEO == "Canada") %>%
  dplyr::select(REF_DATE, VALUE) %>%
  separate(REF_DATE, into = c('year', 'month'))%>% 
  select(year, VALUE) %>%
  write_csv("data/cpi.csv")


cpi_df <- read.csv("data/cpi.csv") %>% 
  dplyr::group_by(year) %>%
  dplyr::summarize(cpi_annual = mean(VALUE))

cpi <- cpi_df %>% column_to_rownames("year") 
cpi_2022 <- cpi["2022", "cpi_annual"]
cpi_series <- cpi_df %>% mutate(rel_cpi = round(cpi_2022/cpi_annual,2))
view(cpi_series)


#_______________________ price distributions___________
price_new <- read_csv("data/price_new.csv") 
view(price_new)
unique(price_new$`Farm products`)
source("functions_yield_price_data.R")
source("functions_dist.R")

price_df(df=price_new, prov = "Alberta")

#price distribution analysis :   crops <- c("Barley","Canola","Corn","Flaxseed","Oats","Peas","Soybeans","Wheat" )
#1. peas
pea_price <- read_csv("data/price/ Dry peas [114314] Alberta .csv") %>% mutate(value = value *0.03) %>% drop_na()
min_year_pea <- min(pea_price$year)
max_year_pea <- max(pea_price$year)
cpi_series_pea <- cpi_series %>% filter(year>= min_year_pea & year<=max_year_pea) %>% select(rel_cpi)

nrow(cpi_series_pea)
nrow(pea_price)
pea_price_df <- pea_price %>% cbind(cpi_series_pea) %>% mutate(value = value*rel_cpi) %>% select(year, value)


dist_fit(df = pea_price_df)     #uniform
summary_plots(df=pea_price_df)
min(pea_price_df$value)
max(pea_price_df$value)


#2. canola
can_price <- read_csv("data/price/ Canola (including rapeseed) [113111] Alberta .csv") %>% 
  mutate(value = value *0.03) %>% drop_na()
view(can_price)

min_year_can <- min(can_price$year)
max_year_can <- max(can_price$year)
cpi_series_can <- cpi_series %>% filter(year>= min_year_can & year<=max_year_can) %>% dplyr::select(rel_cpi)

nrow(cpi_series_can)
nrow(can_price)
can_price_df <- can_price %>% cbind(cpi_series_can) %>% mutate(value = value*rel_cpi) %>% dplyr::select(year, value)
view(can_price_df)

dist_fit(df = can_price_df)     #gamma
summary_plots(df=can_price_df)
fit.can_price <- fitdist(can_price_df$value, "gamma") #
rgamma(n=10, shape=32, rate=2)


#3. Barley
bar_price <- read_csv("data/price/ Barley [1151141] Alberta .csv")   %>% 
  mutate(value = value *0.03) %>% drop_na()

min_year_bar<- min(bar_price$year)
max_year_bar <- max(bar_price$year)
cpi_series_bar <- cpi_series %>% filter(year>= min_year_bar & year<=max_year_bar) %>% dplyr::select(rel_cpi)

nrow(cpi_series_bar)
nrow(bar_price)
bar_price_df <- bar_price %>% cbind(cpi_series_bar) %>% mutate(value = value*rel_cpi) %>% dplyr::select(year, value)
view(bar_price_df)

dist_fit(df = bar_price_df)     #uniform
summary_plots(df=bar_yd)
min(bar_price_df$value)
max(bar_price_df$value)

#4.Oats
oats_price <- read_csv("data/price/ Oats [115113111] Alberta .csv")  %>% 
  mutate(value = value *0.03) %>% drop_na()

min_year_oat<- min(oats_price$year)
max_year_oat <- max(oats_price$year)
cpi_series_oats <- cpi_series %>% filter(year>= min_year_oat & year<=max_year_oat) %>% dplyr::select(rel_cpi)

nrow(cpi_series_oats)
nrow(bar_price)
oats_price_df <- oats_price %>% cbind(cpi_series_oats) %>% mutate(value = value*rel_cpi) %>% dplyr::select(year, value)
view(oats_price_df)

dist_fit(df = oats_price_df)     #uniform
summary_plots(df=oats_price_df)
min(oats_price_df$value)
max(oats_price_df$value)

#5.Wheat 
wh_price <- read_csv("data/price/ Wheat (except durum wheat), milling Alberta .csv") %>% 
  mutate(value = as.numeric(value) *0.03) %>% drop_na()
view(wh_price)
min_year_wh<- min(wh_price$year)
max_year_wh <- max(wh_price$year)
cpi_series_wh <- cpi_series %>% filter(year>= min_year_wh & year<=max_year_wh) %>% dplyr::select(rel_cpi)

nrow(cpi_series_wh)
nrow(wh_price)
wh_price_df <- wh_price %>% cbind(cpi_series_wh) %>% mutate(value = value*rel_cpi) %>% dplyr::select(year, value)
view(wh_price_df)

#view(wheat_yd)
dist_fit(df = wh_price_df)     #lnorm
summary_plots(df=wheat_yd)
min(wh_price_df$value)
max(wh_price_df$value)



