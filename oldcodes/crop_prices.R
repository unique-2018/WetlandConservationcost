library(tidyverse)
prices <- read_csv("data/prices.csv")
view(prices)
yield <- read_csv("data/yield.csv")
unique(prices$`Farm products`)
unique(prices$year)
unique(prices$province)
view(yield)

price1 <- prices %>% separate(REF_DATE, sep = "-", into = c("year", "month"), extra = "merge") %>% 
  mutate(crop = case_when(
    `Farm products` == "Wheat (except durum wheat), milling" ~ "Wheat",
    `Farm products` == "Dry peas [114314]" ~ "Peas",
    `Farm products` == "Flaxseed [115122111]" ~ "Flax",
    `Farm products` == "Canola (including rapeseed) [113111]" ~ "Canola",
    `Farm products` == "Corn for grain [1151111]" ~ "Corn",
    `Farm products` == "Soybeans [1151211]" ~ "Soybeans",
    `Farm products` == "Barley [1151141]" ~ "Barley",
    `Farm products` == "Oats [115113111]" ~ "Oats",
  )) %>% dplyr::rename(province = GEO, value= VALUE) %>% dplyr::select(year, month, province, crop, value)
view(price1)


crops <- c("Wheat (except durum wheat), milling" , "Wheat","Dry peas [114314]" , "Flaxseed [115122111]",
           "Canola (including rapeseed) [113111]", "Corn for grain [1151111]","Soybeans [1151211]",
           "Barley [1151141]", "Oats [115113111]")

prairies <- c("Manitoba","Saskatchewan","Alberta" )

price_new <- prices %>% separate(REF_DATE, sep = "-", into = c("year", "month"), extra = "merge") %>% dplyr::rename(province = GEO, value= VALUE) %>% 
  mutate(value = value*0.03) %>%
  dplyr::select(year, month, province, `Farm products`, value)
view(price_new)

write_csv(price_new, "data/price/price_new.csv")

colnames(prices) 

crops <- c("Wheat", "Peas", "Flax", "Canola","Corn", "Soybeans", "Barley", "Oats")
prairies <- c("Manitoba","Saskatchewan","Alberta" )

#prices <- prices %>% filter(crop %in% crops & province %in% prairies) %>% dplyr::select(year, province, crop, value)

prices_sk_wheat <- price1 %>% filter(province == "Saskatchewan" & crop == "Wheat") %>% group_by(year) %>% dplyr::select(year, value1)
view(prices_sk_wheat)
prices_sk_drypeas <- prices %>% filter(province == "Saskatchewan" & crop == "Peas") %>% group_by(year) %>% dplyr::select(year, value)
prices_sk_flax <- prices %>% filter(province == "Saskatchewan" & crop == "Flax") %>% group_by(year) %>% dplyr::select(year, value)
prices_sk_canola <- prices %>% filter(province == "Saskatchewan" & crop == "Canola") %>% group_by(year) %>% dplyr::select(year, value)
prices_sk_soybeans <- prices %>% filter(province == "Saskatchewan" & crop == "Soybeans") %>% group_by(year) %>% dplyr::select(year, value)
prices_sk_barley <- prices %>% filter(province == "Saskatchewan" & crop == "Barley") %>% group_by(year) %>% dplyr::select(year, value)
prices_sk_corn <- prices %>% filter(province == "Saskatchewan" & crop == "Corn") %>% group_by(year) %>% dplyr::select(year, value)
prices_sk_oats <- prices %>% filter(province == "Saskatchewan" & crop == "Oats") %>% group_by(year) %>% dplyr::select(year, value)


prices_mn_wheat <- prices %>% filter(province == "Manitoba" & crop == "Wheat") %>% group_by(year) %>% dplyr::select(year, value)
prices_mn_drypeas <- prices %>% filter(province == "Manitoba" & crop == "Peas") %>% group_by(year) %>% dplyr::select(year, value)
prices_mn_flax <- prices %>% filter(province == "Manitoba" & crop == "Flax") %>% group_by(year) %>% dplyr::select(year, value)
prices_mn_canola <- prices %>% filter(province == "Manitoba" & crop == "Canola") %>% group_by(year) %>% dplyr::select(year, value)
prices_mn_soybeans <- prices %>% filter(province == "Manitoba" & crop == "Soybeans") %>% group_by(year) %>% dplyr::select(year, value)
prices_mn_barley <- prices %>% filter(province == "Manitoba" & crop == "Barley") %>% group_by(year) %>% dplyr::select(year, value)
prices_mn_corn <- prices %>% filter(province == "Manitoba" & crop == "Corn") %>% group_by(year) %>% dplyr::select(year, value)
prices_mn_oats <- prices %>% filter(province == "Manitoba" & crop == "Oats") %>% group_by(year) %>% dplyr::select(year, value)

prices_ab_wheat <- prices %>% filter(province == "Alberta" & crop == "Wheat") %>% group_by(year) %>% dplyr::select(year, value)
prices_ab_drypeas <- prices %>% filter(province == "Alberta" & crop == "Peas") %>% group_by(year) %>% dplyr::select(year, value)
prices_ab_flax <- prices %>% filter(province == "Alberta" & crop == "Flax") %>% group_by(year) %>% dplyr::select(year, value)
prices_ab_canola <- prices %>% filter(province == "Alberta" & crop == "Canola") %>% group_by(year) %>% dplyr::select(year, value)
prices_ab_soybeans <- prices %>% filter(province == "Alberta" & crop == "Soybeans") %>% group_by(year) %>% dplyr::select(year, value)
prices_ab_barley <- prices %>% filter(province == "Alberta" & crop == "Barley") %>% group_by(year) %>% dplyr::select(year, value)
prices_ab_corn <- prices %>% filter(province == "Alberta" & crop == "Corn") %>% group_by(year) %>% dplyr::select(year, value)
prices_ab_oats <- prices %>% filter(province == "Alberta" & crop == "Oats") %>% group_by(year) %>% dplyr::select(year, value)

prices <- read_csv("data/price_new.csv")

price_df <- function(df, province){
  
  crops <- c("Wheat", "Peas", "Flax", "Canola",
             "Corn", "Soybeans", "Barley", "Oats")
  prairies <- c("Manitoba","Saskatchewan","Alberta" )
  
  prices <- prices %>% filter(crop %in% crops & province == province) %>% dplyr::select(year, province, crop, value)
  
  for (i in crops){
    
    prices <- prices %>% filter(crop == i) %>% group_by(year) %>% dplyr::select(year, crop, value) %>% write_csv(paste("data/", i, ".csv"))
    
  }

}

price_df(df=prices, province = "Saskatchewan")






